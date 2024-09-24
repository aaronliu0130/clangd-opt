//===--- ImplementAbstract.cpp -----------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "refactor/InsertionPoint.h"
#include "refactor/Tweak.h"
#include "support/Logger.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/Basic/Specifiers.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/raw_ostream.h"

namespace clang {
namespace clangd {

namespace {

// FIXME: Have some way to control this, maybe in the config?
constexpr bool DefineMethods = true;
using MethodAndAccess =
    llvm::PointerIntPair<const CXXMethodDecl *, 2, AccessSpecifier>;

AccessSpecifier getMostConstrained(AccessSpecifier InheritSpecifier,
                                   AccessSpecifier DefinedAs) {
  return std::max(InheritSpecifier, DefinedAs);
}

/// Populates \p Overrides with all the methods that are overridden by methods
/// in \p RD.
void buildOverrideSet(const CXXRecordDecl &RD,
                      llvm::SmallPtrSetImpl<const CXXMethodDecl *> &Overrides) {
  for (const CXXMethodDecl *Method : RD.methods()) {
    if (!Method->isVirtual())
      continue;
    for (const auto *Overriding : Method->overridden_methods())
      Overrides.insert(Overriding);
  }
}

bool areAnyBasesDependent(const CXXRecordDecl &RD) {
  for (const CXXBaseSpecifier &Base : RD.bases()) {
    const RecordType *RT = Base.getType()->getAs<RecordType>();
    if (!RT)
      return true;
    const CXXRecordDecl *BaseDecl = cast<CXXRecordDecl>(RT->getDecl());
    if (!BaseDecl->isPolymorphic())
      continue;
    if (areAnyBasesDependent(*BaseDecl))
      return true;
  }
  return false;
}

/// Detects if there are any non overridden methods declared in \p RD.
bool detectPureMethodsImpl(
    const CXXRecordDecl &RD,
    llvm::SmallPtrSetImpl<const CXXMethodDecl *> &Overrides) {
  if (RD.getNumBases() > 0) {
    buildOverrideSet(RD, Overrides);
    for (const CXXBaseSpecifier &Base : RD.bases()) {
      const RecordType *RT = Base.getType()->getAs<RecordType>();
      assert(RT && "There should be no dependent bases at this point");
      const CXXRecordDecl *BaseDecl = cast<CXXRecordDecl>(RT->getDecl());
      if (!BaseDecl->isPolymorphic())
        continue;
      if (detectPureMethodsImpl(*BaseDecl, Overrides))
        return true;
    }
  }
  for (const CXXMethodDecl *Method : RD.methods()) {
    if (!Method->isPureVirtual())
      continue;
    if (!Overrides.contains(Method))
      return true;
  }
  return false;
}

/// Detects if there are pure virtual methods from the base class \p Base from
/// \p RD that need an implementation.
bool hasPureVirtualMethodsFromBase(const CXXRecordDecl &RD,
                                   const CXXBaseSpecifier &Base) {
  assert(llvm::any_of(RD.bases(), [&Base](const CXXBaseSpecifier &Base2) {
    // CXXBaseSpecifier has no operator== and as DynTypedNode holds a copy, we
    // can't use pointer identity. This check should ensure the base we have
    // selected comes from RD.
    return Base.getTypeSourceInfo() == Base2.getTypeSourceInfo() &&
           Base.getSourceRange() == Base2.getSourceRange();
  }));
  const RecordType *RT = Base.getType()->getAs<RecordType>();
  if (!RT)
    return false;
  const CXXRecordDecl *BaseDecl = cast<CXXRecordDecl>(RT->getDecl());
  if (!BaseDecl->isPolymorphic())
    return false;
  if (areAnyBasesDependent(*BaseDecl))
    return false;
  llvm::SmallPtrSet<const CXXMethodDecl *, 16> Overrides;
  buildOverrideSet(RD, Overrides);
  return detectPureMethodsImpl(*BaseDecl, Overrides);
}

/// Detects if there are pure virtual methods from \p RD base classes that need
/// an implementation.
bool hasPureVirtualMethods(const CXXRecordDecl &RD) {
  if (areAnyBasesDependent(RD))
    return false;
  llvm::SmallPtrSet<const CXXMethodDecl *, 16> Overrides;
  buildOverrideSet(RD, Overrides);
  return detectPureMethodsImpl(RD, Overrides);
}

/// Stores all pure methods in \p Record that aren't in \p Overrides in \p
/// Results. The methods are stored the most constrained access of \p Access and
/// the AccessSpecifier of the method.
void collectNonOverriddenPureMethods(
    const CXXRecordDecl &Record,
    llvm::SmallVectorImpl<MethodAndAccess> &Results, AccessSpecifier Access,
    const llvm::SmallPtrSetImpl<const CXXMethodDecl *> &Overrides) {
  for (const CXXMethodDecl *Method : Record.methods()) {
    if (!Method->isPureVirtual())
      continue;
    if (!Overrides.contains(Method))
      Results.emplace_back(Method,
                           getMostConstrained(Access, Method->getAccess()));
  }
}

/// Collect all the pure virtual methods in \p Record and its base classes that
/// don't appear in \p Overrides, store the results in \p Results. Returns true
/// if any of the bases are dependent, otherwise false.
bool collectPureMethodsImpl(
    const CXXRecordDecl &Record,
    llvm::SmallVectorImpl<MethodAndAccess> &Results, AccessSpecifier Access,
    llvm::SmallPtrSetImpl<const CXXMethodDecl *> &Overrides) {
  if (Record.getNumBases() > 0) {
    buildOverrideSet(Record, Overrides);
    for (const CXXBaseSpecifier &Base : Record.bases()) {
      const RecordType *RT = Base.getType()->getAs<RecordType>();
      if (!RT)
        return true;
      const CXXRecordDecl *BaseDecl = cast<CXXRecordDecl>(RT->getDecl());
      if (!BaseDecl->isPolymorphic())
        continue;
      if (collectPureMethodsImpl(
              *BaseDecl, Results,
              getMostConstrained(Access, Base.getAccessSpecifier()), Overrides))
        // Propergate any error back up.
        return true;
    }
  }
  // Add the Pure methods from this class after traversing the bases. This means
  // when it comes time to create implementation, methods from classes higher up
  // the heirachy will appear first.
  collectNonOverriddenPureMethods(Record, Results, Access, Overrides);
  return false;
}

/// Collect all the pure virtual methods from the base class \p Base that
/// haven't been overridden in \p Record. Store the results in \p Results.
bool collectPureMethodsFromBase(
    const CXXRecordDecl &RD, const CXXBaseSpecifier &Base,
    llvm::SmallVectorImpl<MethodAndAccess> &Results) {
  assert(llvm::any_of(RD.bases(), [&Base](const CXXBaseSpecifier &Base2) {
    // CXXBaseSpecifier has no operator== and as DynTypedNode holds a copy, we
    // can't use pointer identity. This check should ensure the base we have
    // selected comes from RD.
    return Base.getTypeSourceInfo() == Base2.getTypeSourceInfo() &&
           Base.getSourceRange() == Base2.getSourceRange();
  }));
  const RecordType *RT = Base.getType()->getAs<RecordType>();
  if (!RT)
    return true;
  const CXXRecordDecl *BaseDecl = cast<CXXRecordDecl>(RT->getDecl());
  if (!BaseDecl->isPolymorphic())
    return true;
  llvm::SmallPtrSet<const CXXMethodDecl *, 16> Overrides;
  buildOverrideSet(RD, Overrides);
  return collectPureMethodsImpl(*BaseDecl, Results, Base.getAccessSpecifier(),
                                Overrides);
}

bool collectAllPureMethods(const CXXRecordDecl &RD,
                           llvm::SmallVectorImpl<MethodAndAccess> &Results) {
  llvm::SmallPtrSet<const CXXMethodDecl *, 16> Overrides;
  buildOverrideSet(RD, Overrides);
  return collectPureMethodsImpl(RD, Results, AS_public, Overrides);
}

/// Gets the class at the Selection \p Inputs. If the selection is in
/// the base-specifier-list, The base that it's over will be stored in \p
/// BaseSpec. \returns nullptr if no class could be found.
const CXXRecordDecl *
getSelectedRecord(const Tweak::Selection &Inputs,
                  std::optional<CXXBaseSpecifier> *BaseSpec) {
  const SelectionTree::Node *Node = Inputs.ASTSelection.commonAncestor();
  if (!Node)
    return nullptr;
  if (const auto *RD = Node->ASTNode.get<CXXRecordDecl>())
    return RD;
  // Handle when the selection is over the base specifier. This is recursive
  // because the base specifier could be a template instantiation containing
  // multiple nodes.
  for (; Node->Parent; Node = Node->Parent) {
    if (const auto *BS = Node->ASTNode.get<CXXBaseSpecifier>()) {
      if (const auto *RD = Node->Parent->ASTNode.get<CXXRecordDecl>()) {
        if (BaseSpec)
          *BaseSpec = *BS;
        return RD;
      }
    }
  }
  return nullptr;
}

/// Some quick to check basic heuristics to check before we try and collect
/// virtual methods.
bool isClassOK(const CXXRecordDecl &RecordDecl) {
  if (!RecordDecl.isThisDeclarationADefinition())
    return false;
  if (!RecordDecl.isClass() && !RecordDecl.isStruct())
    return false;
  if (RecordDecl.hasAnyDependentBases() || RecordDecl.getNumBases() == 0)
    return false;
  // We should check for abstract, but that prevents working on template classes
  // that don't have any dependent bases.
  if (!RecordDecl.isPolymorphic())
    return false;
  return true;
}

class PrintingInContextCallback : public PrintingCallbacks {
public:
  PrintingInContextCallback(const DeclContext *CurContext)
      : CurContext(CurContext) {}
  virtual ~PrintingInContextCallback() = default;
  bool isScopeVisible(const DeclContext *DC) const override {
    return DC->Encloses(CurContext);
  }

  PrintingPolicy getPolicy() const {
    PrintingPolicy P = CurContext->getParentASTContext().getPrintingPolicy();
    P.SuppressScope = false;
    P.Callbacks = this;
    return P;
  }

private:
  const DeclContext *CurContext;
};

bool buildBody(llvm::raw_ostream &Out, ArrayRef<MethodAndAccess> Items,
               AccessSpecifier AccessKind, const PrintingPolicy &Policy) {
  bool Any = false;
  for (const auto &MethodAndAccess : Items) {
    if (MethodAndAccess.getInt() != AccessKind)
      continue;
    Any = true;
    const CXXMethodDecl *Method = MethodAndAccess.getPointer();
    Method->getReturnType().print(Out, Policy);
    Out << ' ';
    Out << Method->getNameAsString() << "(";
    bool IsFirst = true;
    for (const auto &Param : Method->parameters()) {
      if (!IsFirst)
        Out << ", ";
      else
        IsFirst = false;
      Param->print(Out, Policy);
    }
    Out << ") ";
    if (Method->isConst())
      Out << "const ";
    if (Method->isVolatile())
      Out << "volatile ";
    if (DefineMethods) {
      Out << "override {\n";
      if (!Method->getReturnType()->isVoidType())
        Out << "return {};\n";
      Out << "}\n";
    } else {
      Out << "override;\n";
    }
  }
  return Any;
}

class ImplementAbstract : public Tweak {
public:
  const char *id() const override;

  bool prepare(const Selection &Inputs) override {
    Selected = getSelectedRecord(Inputs, &FromBase);
    if (!Selected)
      return false;
    if (!isClassOK(*Selected))
      return false;
    if (FromBase)
      return hasPureVirtualMethodsFromBase(*Selected, *FromBase);

    return hasPureVirtualMethods(*Selected);
  }

  Expected<Effect> apply(const Selection &Inputs) override {

    llvm::SmallVector<MethodAndAccess> PureVirtualMethods;
    auto Res = FromBase ? collectPureMethodsFromBase(*Selected, *FromBase,
                                                     PureVirtualMethods)
                        : collectAllPureMethods(*Selected, PureVirtualMethods);

    (void)Res;
    assert(!Res && !PureVirtualMethods.empty());

    PrintingInContextCallback Callbacks(Selected);
    PrintingPolicy Policy = Callbacks.getPolicy();

    SmallString<256> Buffer;
    llvm::raw_svector_ostream OS(Buffer);
    tooling::Replacements Replacements;

    std::vector<Anchor> Anchors = {
        // Below all constructors
        {[](const Decl *D) { return llvm::isa<CXXConstructorDecl>(D); },
         Anchor::Below},
        // Above destructor
        {[](const Decl *D) { return llvm::isa<CXXDestructorDecl>(D); },
         Anchor::Above},
        // Above fields
        {[](const Decl *D) { return llvm::isa<FieldDecl>(D); }, Anchor::Above},
    };

    AccessSpecifier Last = Selected->isClass() ? AS_private : AS_public;
    for (auto *D : Selected->decls()) {
      if (auto *ASD = dyn_cast<AccessSpecDecl>(D)) {
        Last = ASD->getAccess();
      }
    }

    if (buildBody(OS, PureVirtualMethods, Last, Policy)) {
      auto Replacement = insertDecl(Buffer, *Selected, Anchors, Last);
      if (!Replacement)
        return Replacement.takeError();
      llvm::cantFail(Replacements.add(*Replacement));
      Buffer.clear();
    }

    for (AccessSpecifier Spec : {AS_public, AS_protected, AS_private}) {
      if (Spec == Last)
        continue;
      if (!buildBody(OS, PureVirtualMethods, Spec, Policy))
        continue;
      auto Replacement = insertDecl(Buffer, *Selected, Anchors, Spec);
      if (!Replacement)
        return Replacement.takeError();
      if (auto Err = Replacements.add(*Replacement)) {
        Err = llvm::handleErrors(
            std::move(Err),
            [&Replacements](
                const tooling::ReplacementError &RE) -> llvm::Error {
              if (RE.get() != tooling::replacement_error::insert_conflict)
                return llvm::make_error<tooling::ReplacementError>(RE);
              const auto &Conflict = RE.getNewReplacement();
              tooling::Replacement NewR(
                  Conflict->getFilePath(),
                  Replacements.getShiftedCodePosition(
                      RE.getExistingReplacement()->getOffset()),
                  0, Conflict->getReplacementText());
              Replacements = Replacements.merge(tooling::Replacements(NewR));
              return llvm::Error::success();
            });
        if (Err)
          return std::move(Err);
      }
      Buffer.clear();
    }

    return Effect::mainFileEdit(Inputs.AST->getASTContext().getSourceManager(),
                                std::move(Replacements));
  }

  std::string title() const override {
    if (FromBase) {
      assert(Selected);
      PrintingInContextCallback Callbacks(Selected->getDeclContext());
      std::string Result = "Implement pure virtual methods from '";
      llvm::raw_string_ostream OS(Result);
      FromBase->getTypeSourceInfo()->getType().print(OS, Callbacks.getPolicy());
      OS << '\'';
      OS.flush();
      return Result;
    }
    return "Implement pure virtual methods";
  }

  llvm::StringLiteral kind() const override {
    return CodeAction::REFACTOR_KIND;
  }

private:
  const CXXRecordDecl *Selected;
  std::optional<CXXBaseSpecifier> FromBase;
};

REGISTER_TWEAK(ImplementAbstract)

} // namespace
} // namespace clangd
} // namespace clang
