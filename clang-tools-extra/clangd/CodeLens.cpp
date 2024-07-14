//===--- CodeLens.cpp --------------------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "CodeLens.h"
#include "AST.h"
#include "FindSymbols.h"
#include "XRefs.h"
#include "support/Logger.h"

namespace clang {
namespace clangd {
std::optional<Location> declToLocation(const Decl *D) {
  ASTContext &Ctx = D->getASTContext();
  auto &SM = Ctx.getSourceManager();
  SourceLocation NameLoc = nameLocation(*D, Ctx.getSourceManager());
  auto FileFieldRef = SM.getFileEntryRefForID(SM.getFileID(NameLoc));
  if (!FileFieldRef)
    return std::nullopt;
  auto FilePath = getCanonicalPath(*FileFieldRef, SM.getFileManager());
  auto TUFieldRef = SM.getFileEntryRefForID(SM.getFileID(NameLoc));
  if (!TUFieldRef)
    return std::nullopt;
  auto TUPath = getCanonicalPath(*TUFieldRef, SM.getFileManager());
  if (!FilePath || !TUPath)
    return std::nullopt; // Not useful without a uri.

  Position NameBegin = sourceLocToPosition(SM, NameLoc);
  Position NameEnd = sourceLocToPosition(
      SM, Lexer::getLocForEndOfToken(NameLoc, 0, SM, Ctx.getLangOpts()));
  return Location{URIForFile::canonicalize(*FilePath, *TUPath),
                  {NameBegin, NameEnd}};
}

std::vector<Location> lookupIndex(const SymbolIndex *Index, uint32_t Limit,
                                  PathRef Path, Decl *D, RelationKind R) {
  std::vector<Location> Results;
  if (!Index)
    return Results;
  auto ID = getSymbolID(D);
  if (!ID)
    return Results;
  RelationsRequest Req;
  Req.Subjects.insert(ID);
  Req.Limit = Limit;
  Req.Predicate = R;
  Index->relations(Req, [&](const SymbolID &Subject, const Symbol &Object) {
    if (Object.SymInfo.Kind == clang::index::SymbolKind::Class ||
        Object.SymInfo.Kind == clang::index::SymbolKind::Struct) {
      if (auto Loc = indexToLSPLocation(Object.Definition, Path)) {
        Results.emplace_back(std::move(*Loc));
      }
    } else if (auto Loc =
                   indexToLSPLocation(Object.CanonicalDeclaration, Path)) {
      Results.emplace_back(std::move(*Loc));
    }
  });
  return Results;
}

void traverseDecl(ParsedAST &AST, const SymbolIndex *Index, uint32_t Limit,
                  PathRef Path, Decl *D, std::vector<CodeLens> &Results) {
  auto &SM = AST.getSourceManager();
  // Skip symbols which do not originate from the main file.
  if (!isInsideMainFile(D->getLocation(), SM))
    return;
  if (D->isImplicit() || !isa<NamedDecl>(D) || D->getLocation().isMacroID())
    return;

  if (auto *Templ = llvm::dyn_cast<TemplateDecl>(D)) {
    if (auto *TD = Templ->getTemplatedDecl())
      D = TD;
  };
  auto Location = D->getLocation();
  Range Range = {
      sourceLocToPosition(SM, Location),
      sourceLocToPosition(
          SM, Lexer::getLocForEndOfToken(Location, 0, SM, AST.getLangOpts()))};

  // Namspaces are not indexed, so it's meaningless to provide codelens.
  if (!isa<NamespaceDecl, NamespaceAliasDecl>(D)) {
    CodeLensResolveData Data;
    Data.uri = std::string(Path);
    Results.emplace_back(CodeLens{Range, std::nullopt, Data});
  }

  // handle inheritance codelens directly
  CodeLensArgument Sub, Super;
  if (auto *CXXRD = dyn_cast<CXXRecordDecl>(D)) {
    if (!CXXRD->isEffectivelyFinal()) {
      Sub.locations = lookupIndex(Index, Limit, Path, D, RelationKind::BaseOf);
    }
  } else if (auto *CXXMD = dyn_cast<CXXMethodDecl>(D)) {
    if (CXXMD->isVirtual()) {
      Sub.locations =
          lookupIndex(Index, Limit, Path, D, RelationKind::OverriddenBy);
    }
    for (const auto *P : CXXMD->overridden_methods()) {
      if (auto Loc = declToLocation(P->getCanonicalDecl()))
        Super.locations.emplace_back(*Loc);
    }
  }

  if (auto Count = Super.locations.size()) {
    Super.position = Range.start;
    Super.uri = std::string(Path);
    Command Cmd;
    Cmd.command = std::string(CodeAction::SHOW_REFERENCES);
    Cmd.title = llvm::utostr(Count) + " base(s)";
    Cmd.argument = std::move(Super);
    Results.emplace_back(CodeLens{Range, std::move(Cmd), std::nullopt});
  }

  if (auto Count = Sub.locations.size()) {
    Sub.position = Range.start;
    Sub.uri = std::string(Path);
    Command Cmd;
    Cmd.command = std::string(CodeAction::SHOW_REFERENCES);
    Cmd.title = llvm::utostr(Count) + " derived";
    Cmd.argument = std::move(Sub);
    Results.emplace_back(CodeLens{Range, std::move(Cmd), std::nullopt});
  }

  // Skip symbols inside function body.
  if (isa<FunctionDecl>(D)) {
    return;
  }

  if (auto *Scope = dyn_cast<DeclContext>(D)) {
    for (auto *C : Scope->decls())
      traverseDecl(AST, Index, Limit, Path, C, Results);
  }
}

llvm::Expected<std::vector<CodeLens>>
getDocumentCodeLens(ParsedAST &AST, const SymbolIndex *Index, uint32_t Limit,
                    PathRef Path) {
  std::vector<CodeLens> Results;
  Limit = Limit ? Limit : std::numeric_limits<uint32_t>::max();
  for (auto &TopLevel : AST.getLocalTopLevelDecls())
    traverseDecl(AST, Index, Limit, Path, TopLevel, Results);
  return Results;
}

llvm::Expected<CodeLens> resolveCodeLens(ParsedAST &AST, const CodeLens &Params,
                                         uint32_t Limit,
                                         const SymbolIndex *Index,
                                         PathRef Path) {
  Command Cmd;
  Cmd.command = std::string(CodeAction::SHOW_REFERENCES);
  Position Pos = Params.range.start;
  if (Params.data) {
    CodeLensArgument Arg;
    Arg.uri = std::string(Path);
    Arg.position = Pos;
    auto FindedRefs = findReferences(AST, Pos, Limit, Index);
    auto Refs = FindedRefs.References;
    Arg.locations.reserve(Refs.size());
    size_t NotUsedRefs = 0u;
    for (auto &Ref : Refs) {
      if ((Ref.Attributes &
           (ReferencesResult::Declaration | ReferencesResult::Definition |
            ReferencesResult::Override)) != 0)
        NotUsedRefs++;

      Arg.locations.emplace_back(std::move(Ref.Loc));
    }
    if (Arg.locations.empty())
      Cmd.title = "Unused";
    else {
      auto NumRefs = llvm::utostr(Arg.locations.size());
      if (FindedRefs.HasMore)
        NumRefs += "+";
      NumRefs += " ref(s)";
      if (Arg.locations.size() == NotUsedRefs)
        Cmd.title = "Unused " + NumRefs;
      else
        Cmd.title = NumRefs;
    }
    Cmd.argument = std::move(Arg);
    return CodeLens{Params.range, std::move(Cmd), std::nullopt};
  }
  return error("failed to resolve codelens");
}
} // namespace clangd
} // namespace clang
