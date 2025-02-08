//===--- Instantiation.cpp - Getting instantiated symbols--------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/Instantiation.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"
#include "clang/AST/DeclVisitor.h"
#include "clang/AST/RecursiveASTVisitor.h"

namespace clang {
template <typename TemplateDeclTy>
static NamedDecl *getOnlyInstantiationImpl(TemplateDeclTy *TD) {
  NamedDecl *Only = nullptr;
  for (auto *Spec : TD->specializations()) {
    if (Spec->getTemplateSpecializationKind() == TSK_ExplicitSpecialization)
      continue;
    if (Only != nullptr)
      return nullptr;
    Only = Spec;
  }
  return Only;
}

NamedDecl *getOnlyInstantiation(const NamedDecl *TemplatedDecl) {
  if (TemplateDecl *TD = TemplatedDecl->getDescribedTemplate()) {
    if (auto *CTD = llvm::dyn_cast<ClassTemplateDecl>(TD))
      return getOnlyInstantiationImpl(CTD);
    if (auto *FTD = llvm::dyn_cast<FunctionTemplateDecl>(TD))
      return getOnlyInstantiationImpl(FTD);
    if (auto *VTD = llvm::dyn_cast<VarTemplateDecl>(TD))
      return getOnlyInstantiationImpl(VTD);
  }
  return nullptr;
}

namespace {

NamedDecl *getOnlyInstantiatedDecls(const NamedDecl *DependentDecl) {
  if (auto *Instantiation = getOnlyInstantiation(DependentDecl))
    return Instantiation;
  NamedDecl *OuterTemplate = nullptr;
  for (auto *DC = DependentDecl->getDeclContext(); isa<CXXRecordDecl>(DC);
       DC = DC->getParent()) {
    auto *RD = cast<CXXRecordDecl>(DC);
    if (auto *I = getOnlyInstantiation(RD)) {
      OuterTemplate = I;
      break;
    }
  }

  if (!OuterTemplate)
    return nullptr;

  struct Visitor : DeclVisitor<Visitor, NamedDecl *> {
    const NamedDecl *TemplatedDecl;
    Visitor(const NamedDecl *TemplatedDecl) : TemplatedDecl(TemplatedDecl) {}

    NamedDecl *VisitCXXRecordDecl(CXXRecordDecl *RD) {
      if (RD->getTemplateInstantiationPattern() == TemplatedDecl)
        return RD;
      for (auto *F : RD->decls()) {
        if (auto *Injected = llvm::dyn_cast<CXXRecordDecl>(F);
            Injected && Injected->isInjectedClassName())
          continue;
        if (NamedDecl *ND = Visit(F))
          return ND;
      }
      return nullptr;
    }

    NamedDecl *VisitClassTemplateDecl(ClassTemplateDecl *CTD) {
      unsigned Size = llvm::range_size(CTD->specializations());
      if (Size != 1)
        return nullptr;
      return Visit(*CTD->spec_begin());
    }

    NamedDecl *VisitFunctionTemplateDecl(FunctionTemplateDecl *FTD) {
      unsigned Size = llvm::range_size(FTD->specializations());
      if (Size != 1)
        return nullptr;
      return Visit(*FTD->spec_begin());
    }

    NamedDecl *VisitFunctionDecl(FunctionDecl *FD) {
      if (FD->getTemplateInstantiationPattern() == TemplatedDecl)
        return FD;
      return nullptr;
    }

    NamedDecl *VisitVarDecl(VarDecl *VD) {
      if (VD->getCanonicalDecl()->getSourceRange() ==
          TemplatedDecl->getCanonicalDecl()->getSourceRange())
        return VD;
      return nullptr;
    }

    NamedDecl *VisitFieldDecl(FieldDecl *FD) {
      if (FD->getCanonicalDecl()->getSourceRange() ==
          TemplatedDecl->getCanonicalDecl()->getSourceRange())
        return FD;
      return nullptr;
    }
  };
  return Visitor(DependentDecl).Visit(OuterTemplate);
}

} // namespace

std::optional<DynTypedNode>
getOnlyInstantiatedNode(const DeclContext *StartingPoint,
                        const DynTypedNode &DependentNode) {
  if (auto *CTD = DependentNode.get<ClassTemplateDecl>())
    return getOnlyInstantiatedNode(
        StartingPoint, DynTypedNode::create(*CTD->getTemplatedDecl()));
  if (auto *FTD = DependentNode.get<FunctionTemplateDecl>())
    return getOnlyInstantiatedNode(
        StartingPoint, DynTypedNode::create(*FTD->getTemplatedDecl()));

  if (auto *FD = DependentNode.get<FunctionDecl>()) {
    auto *ID = getOnlyInstantiatedDecls(FD);
    if (!ID)
      return std::nullopt;
    return DynTypedNode::create(*ID);
  }
  if (auto *RD = DependentNode.get<CXXRecordDecl>()) {
    auto *ID = getOnlyInstantiatedDecls(RD);
    if (!ID)
      return std::nullopt;
    return DynTypedNode::create(*ID);
  }

  NamedDecl *InstantiatedEnclosingDecl = nullptr;
  for (auto *DC = StartingPoint; DC;
       DC = DC->getParent()) {
    auto *ND = llvm::dyn_cast<NamedDecl>(DC);
    if (!ND)
      continue;
    InstantiatedEnclosingDecl = getOnlyInstantiatedDecls(ND);
    if (InstantiatedEnclosingDecl)
      break;
  }

  if (!InstantiatedEnclosingDecl)
    return std::nullopt;

  auto *InstantiatedFunctionDecl =
      llvm::dyn_cast<FunctionDecl>(InstantiatedEnclosingDecl);
  if (!InstantiatedFunctionDecl)
    return std::nullopt;

  struct FullExprVisitor : RecursiveASTVisitor<FullExprVisitor> {
    const DynTypedNode &DependentNode;
    Stmt *Result;
    FullExprVisitor(const DynTypedNode &DependentNode)
        : DependentNode(DependentNode), Result(nullptr) {}

    bool shouldVisitTemplateInstantiations() const { return true; }

    bool shouldVisitImplicitCode() const { return true; }

    bool VisitStmt(Stmt *S) {
      if (S->getSourceRange() == DependentNode.getSourceRange()) {
        Result = S;
        return false;
      }
      return true;
    }
  };

  FullExprVisitor Visitor(DependentNode);
  Visitor.TraverseFunctionDecl(InstantiatedFunctionDecl);
  if (Visitor.Result)
    return DynTypedNode::create(*Visitor.Result);
  return std::nullopt;
}

NamedDecl *
getOnlyInstantiationForMemberFunction(const CXXMethodDecl *TemplatedDecl) {
  if (auto *MemberInstantiation = getOnlyInstantiation(TemplatedDecl))
    return MemberInstantiation;
  NamedDecl *OuterTemplate = nullptr;
  for (auto *DC = TemplatedDecl->getDeclContext(); isa<CXXRecordDecl>(DC);
       DC = DC->getParent()) {
    auto *RD = cast<CXXRecordDecl>(DC);
    if (auto *I = getOnlyInstantiation(RD)) {
      OuterTemplate = I;
      break;
    }
  }
  if (!OuterTemplate)
    return nullptr;
  struct Visitor : DeclVisitor<Visitor, NamedDecl *> {
    const CXXMethodDecl *TD;
    Visitor(const CXXMethodDecl *TemplatedDecl) : TD(TemplatedDecl) {}
    NamedDecl *VisitCXXRecordDecl(CXXRecordDecl *RD) {
      for (auto *F : RD->decls()) {
        if (!isa<NamedDecl>(F))
          continue;
        if (NamedDecl *ND = Visit(F))
          return ND;
      }
      return nullptr;
    }

    NamedDecl *VisitClassTemplateDecl(ClassTemplateDecl *CTD) {
      unsigned Size = llvm::range_size(CTD->specializations());
      if (Size != 1)
        return nullptr;
      return Visit(*CTD->spec_begin());
    }

    NamedDecl *VisitFunctionTemplateDecl(FunctionTemplateDecl *FTD) {
      unsigned Size = llvm::range_size(FTD->specializations());
      if (Size != 1)
        return nullptr;
      return Visit(*FTD->spec_begin());
    }

    NamedDecl *VisitCXXMethodDecl(CXXMethodDecl *MD) {
      auto *Pattern = MD->getTemplateInstantiationPattern();
      if (Pattern == TD)
        return MD;
      return nullptr;
    }

  };
  return Visitor(TemplatedDecl).Visit(OuterTemplate);
}
} // namespace clang
