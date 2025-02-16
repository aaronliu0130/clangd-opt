//===--- HeuristicResolver.h - Resolution of dependent names -----*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_CLANG_SEMA_HEURISTICRESOLVER_H
#define LLVM_CLANG_SEMA_HEURISTICRESOLVER_H

#include "clang/AST/Decl.h"
#include <vector>

namespace clang {

class ASTContext;
class CallExpr;
class CXXBasePath;
class CXXDependentScopeMemberExpr;
class DeclarationName;
class DependentScopeDeclRefExpr;
class NamedDecl;
class Type;
class UnresolvedUsingValueDecl;

// This class handles heuristic resolution of declarations and types in template
// code.
//
// As a compiler, clang only needs to perform certain types of processing on
// template code (such as resolving dependent names to declarations, or
// resolving the type of a dependent expression) after instantiation. Indeed,
// C++ language features such as template specialization mean such resolution
// cannot be done accurately before instantiation.
//
// However, template code is written and read in uninstantiated form, and clangd
// would like to provide editor features like go-to-definition in template code
// where possible. To this end, clangd attempts to resolve declarations and
// types in uninstantiated code by using heuristics, understanding that the
// results may not be fully accurate but that this is better than nothing.
//
// At this time, the heuristic used is a simple but effective one: assume that
// template instantiations are based on the primary template definition and not
// not a specialization. More advanced heuristics may be added in the future.
class HeuristicResolver {
public:
  HeuristicResolver(const ASTContext &Ctx,
                    const DeclContext *EnclosingDecl = nullptr)
      : Ctx(Ctx), EnclosingDecl(EnclosingDecl) {}

  // Try to heuristically resolve certain types of expressions, declarations, or
  // types to one or more likely-referenced declarations.
  std::vector<const NamedDecl *>
  resolveMemberExpr(const CXXDependentScopeMemberExpr *ME) const;
  std::vector<const NamedDecl *>
  resolveDeclRefExpr(const DependentScopeDeclRefExpr *RE) const;
  std::vector<const NamedDecl *>
  resolveTypeOfCallExpr(const CallExpr *CE) const;
  std::vector<const NamedDecl *>
  resolveCalleeOfCallExpr(const CallExpr *CE) const;
  std::vector<const NamedDecl *>
  resolveUsingValueDecl(const UnresolvedUsingValueDecl *UUVD) const;
  std::vector<const NamedDecl *>
  resolveDependentNameType(const DependentNameType *DNT) const;
  std::vector<const NamedDecl *> resolveTemplateSpecializationType(
      const DependentTemplateSpecializationType *DTST) const;

  // Try to heuristically resolve a dependent nested name specifier
  // to the type it likely denotes. Note that *dependent* name specifiers always
  // denote types, not namespaces.
  QualType
  resolveNestedNameSpecifierToType(const NestedNameSpecifier *NNS) const;

  // Given the type T of a dependent expression that appears of the LHS of a
  // "->", heuristically find a corresponding pointee type in whose scope we
  // could look up the name appearing on the RHS.
  const QualType getPointeeType(QualType T) const;

private:
  const ASTContext &Ctx;
  const DeclContext *EnclosingDecl;

  // Given a tag-decl type and a member name, heuristically resolve the
  // name to one or more declarations.
  // The current heuristic is simply to look up the name in the primary
  // template. This is a heuristic because the template could potentially
  // have specializations that declare different members.
  // Multiple declarations could be returned if the name is overloaded
  // (e.g. an overloaded method in the primary template).
  // This heuristic will give the desired answer in many cases, e.g.
  // for a call to vector<T>::size().
  std::vector<const NamedDecl *> resolveDependentMember(
      const Type *T, DeclarationName Name,
      llvm::function_ref<bool(const NamedDecl *ND)> Filter) const;

  // Try to heuristically resolve the type of a possibly-dependent expression
  // `E`.
  const Type *resolveExprToType(const Expr *E) const;
  std::vector<const NamedDecl *> resolveExprToDecls(const Expr *E) const;

  // Helper function for HeuristicResolver::resolveDependentMember()
  // which takes a possibly-dependent type `T` and heuristically
  // resolves it to a CXXRecordDecl in which we can try name lookup.
  CXXRecordDecl *resolveTypeToRecordDecl(const Type *T) const;

  // This is a reimplementation of CXXRecordDecl::lookupDependentName()
  // so that the implementation can call into other HeuristicResolver helpers.
  // FIXME: Once HeuristicResolver is upstreamed to the clang libraries
  // (https://github.com/clangd/clangd/discussions/1662),
  // CXXRecordDecl::lookupDepenedentName() can be removed, and its call sites
  // can be modified to benefit from the more comprehensive heuristics offered
  // by HeuristicResolver instead.
  std::vector<const NamedDecl *> lookupDependentName(
      CXXRecordDecl *RD, DeclarationName Name,
      llvm::function_ref<bool(const NamedDecl *ND)> Filter) const;
  bool findOrdinaryMemberInDependentClasses(const CXXBaseSpecifier *Specifier,
                                            CXXBasePath &Path,
                                            DeclarationName Name) const;
};

} // namespace clang

#endif
