//===--- Instantiation.h - Getting instantiated symbols----------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/Decl.h"
#include "clang/AST/DeclBase.h"
#include "clang/AST/DeclCXX.h"

namespace clang {

// If TemplatedDecl is the generic body of a template, and the template has
// exactly one visible instantiation, return the instantiated body.
NamedDecl *getOnlyInstantiation(const NamedDecl *TemplatedDecl);

NamedDecl *
getOnlyInstantiationForMemberFunction(const CXXMethodDecl *TemplatedDecl);

std::optional<DynTypedNode>
getOnlyInstantiatedNode(const DeclContext *StartingPoint,
                        const DynTypedNode &DependentNode);

} // namespace clang
