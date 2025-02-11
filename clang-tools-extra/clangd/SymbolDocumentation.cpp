//===--- SymbolDocumentation.cpp ==-------------------------------*- C++-*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "SymbolDocumentation.h"
#include "clang/AST/CommentVisitor.h"
#include "llvm/Support/JSON.h"

namespace clang {
namespace clangd {

void ensureUTF8(std::string &Str) {
  if (!llvm::json::isUTF8(Str))
    Str = llvm::json::fixUTF8(Str);
}

void ensureUTF8(llvm::MutableArrayRef<std::string> Strings) {
  for (auto &Str : Strings) {
    ensureUTF8(Str);
  }
}

class BlockCommentToString
    : public comments::ConstCommentVisitor<BlockCommentToString> {
public:
  BlockCommentToString(std::string &Out, const ASTContext &Ctx)
      : Out(Out), Ctx(Ctx) {}

  void visitParagraphComment(const comments::ParagraphComment *C) {
    for (const auto *Child = C->child_begin(); Child != C->child_end();
         ++Child) {
      visit(*Child);
    }
  }

  void visitBlockCommandComment(const comments::BlockCommandComment *B) {
    Out << (B->getCommandMarker() == (comments::CommandMarkerKind::CMK_At)
                ? '@'
                : '\\')
        << B->getCommandName(Ctx.getCommentCommandTraits());

    visit(B->getParagraph());
  }

  void visitTextComment(const comments::TextComment *C) {
    // If this is the very first node, the paragraph has no doxygen command,
    // so there will be a leading space -> Trim it
    // Otherwise just trim trailing space
    if (Out.str().empty())
      Out << C->getText().trim();
    else
      Out << C->getText().rtrim();
  }

  void visitInlineCommandComment(const comments::InlineCommandComment *C) {
    const std::string SurroundWith = [C] {
      switch (C->getRenderKind()) {
      case comments::InlineCommandRenderKind::Monospaced:
        return "`";
      case comments::InlineCommandRenderKind::Bold:
        return "**";
      case comments::InlineCommandRenderKind::Emphasized:
        return "*";
      default:
        return "";
      }
    }();

    Out << " " << SurroundWith;
    for (unsigned I = 0; I < C->getNumArgs(); ++I) {
      Out << C->getArgText(I);
    }
    Out << SurroundWith;
  }

private:
  llvm::raw_string_ostream Out;
  const ASTContext &Ctx;
};

class CommentToSymbolDocumentation
    : public comments::ConstCommentVisitor<CommentToSymbolDocumentation> {
public:
  CommentToSymbolDocumentation(const RawComment &RC, const ASTContext &Ctx,
                               const Decl *D, SymbolDocumentationOwned &Doc)
      : FullComment(RC.parse(Ctx, nullptr, D)), Output(Doc), Ctx(Ctx) {

    Doc.CommentText =
        RC.getFormattedText(Ctx.getSourceManager(), Ctx.getDiagnostics());

    for (auto *Block : FullComment->getBlocks()) {
      visit(Block);
    }
  }

  void visitBlockCommandComment(const comments::BlockCommandComment *B) {
    const llvm::StringRef CommandName =
        B->getCommandName(Ctx.getCommentCommandTraits());

    // Visit B->getParagraph() for commands that we have special fields for,
    // so that the command name won't be included in the string.
    // Otherwise, we want to keep the command name, so visit B itself.
    if (CommandName == "brief") {
      BlockCommentToString(Output.Brief, Ctx).visit(B->getParagraph());
    } else if (CommandName == "details") {
      BlockCommentToString(Output.Details, Ctx).visit(B->getParagraph());
    } else if (CommandName == "return") {
      BlockCommentToString(Output.Returns, Ctx).visit(B->getParagraph());
    } else if (CommandName == "warning") {
      BlockCommentToString(Output.Warnings.emplace_back(), Ctx)
          .visit(B->getParagraph());
    } else if (CommandName == "exception" || CommandName == "throw") {
      BlockCommentToString(Output.Exceptions.emplace_back(), Ctx)
          .visit(B->getParagraph());
    } else if (CommandName == "todo") {
      BlockCommentToString(Output.Todos.emplace_back(), Ctx)
          .visit(B->getParagraph());
    } else if (CommandName == "bug") {
      BlockCommentToString(Output.Bugs.emplace_back(), Ctx)
          .visit(B->getParagraph());
    } else if (CommandName == "note") {
      BlockCommentToString(Output.Notes.emplace_back(), Ctx)
          .visit(B->getParagraph());
    } else {
      if (!Output.Description.empty())
        Output.Description += "\n\n";

      BlockCommentToString(Output.Description, Ctx).visit(B);
    }
  }

  void visitParagraphComment(const comments::ParagraphComment *P) {
    BlockCommentToString(Output.Description, Ctx).visit(P);
  }

  void visitParamCommandComment(const comments::ParamCommandComment *P) {
    if (P->hasParamName() && P->hasNonWhitespaceParagraph()) {
      ParameterDocumentationOwned Doc;
      Doc.Name = P->getParamNameAsWritten().str();
      BlockCommentToString(Doc.Description, Ctx).visit(P->getParagraph());
      Output.Parameters.push_back(std::move(Doc));
    }
  }

  void visitTParamCommandComment(const comments::TParamCommandComment *P) {
    if (P->hasParamName() && P->hasNonWhitespaceParagraph()) {
      ParameterDocumentationOwned Doc;
      Doc.Name = P->getParamNameAsWritten().str();
      BlockCommentToString(Doc.Description, Ctx).visit(P->getParagraph());
      Output.TemplateParameters.push_back(std::move(Doc));
    }
  }

private:
  comments::FullComment *FullComment;
  SymbolDocumentationOwned &Output;
  const ASTContext &Ctx;
};

SymbolDocumentationOwned parseDoxygenComment(const RawComment &RC,
                                             const ASTContext &Ctx,
                                             const Decl *D) {
  SymbolDocumentationOwned Doc;
  CommentToSymbolDocumentation(RC, Ctx, D, Doc);

  // Clang requires source to be UTF-8, but doesn't enforce this in comments.
  ensureUTF8(Doc.Brief);
  ensureUTF8(Doc.Details);
  ensureUTF8(Doc.Returns);

  ensureUTF8(Doc.Notes);
  ensureUTF8(Doc.Warnings);
  ensureUTF8(Doc.Exceptions);
  ensureUTF8(Doc.Todos);
  ensureUTF8(Doc.Bugs);

  for (auto &Param : Doc.Parameters) {
    ensureUTF8(Param.Name);
    ensureUTF8(Param.Description);
  }

  for (auto &Param : Doc.TemplateParameters) {
    ensureUTF8(Param.Name);
    ensureUTF8(Param.Description);
  }

  ensureUTF8(Doc.Description);
  ensureUTF8(Doc.CommentText);

  return Doc;
}

template struct ParameterDocumentation<std::string>;
template struct ParameterDocumentation<llvm::StringRef>;

template <class StrOut, class StrIn>
SymbolDocumentation<StrOut> convert(const SymbolDocumentation<StrIn> &In) {
  SymbolDocumentation<StrOut> Doc;

  Doc.Brief = In.Brief;
  Doc.Details = In.Details;
  Doc.Returns = In.Returns;

  Doc.Notes.reserve(In.Notes.size());
  for (const auto &Note : In.Notes) {
    Doc.Notes.emplace_back(Note);
  }

  Doc.Warnings.reserve(In.Warnings.size());
  for (const auto &Warning : In.Warnings) {
    Doc.Warnings.emplace_back(Warning);
  }

  Doc.Exceptions.reserve(In.Exceptions.size());
  for (const auto &Exception : In.Exceptions) {
    Doc.Exceptions.emplace_back(Exception);
  }

  Doc.Todos.reserve(In.Todos.size());
  for (const auto &Todo : In.Todos) {
    Doc.Todos.emplace_back(Todo);
  }

  Doc.Bugs.reserve(In.Bugs.size());
  for (const auto &Bug : In.Bugs) {
    Doc.Bugs.emplace_back(Bug);
  }

  Doc.Parameters.reserve(In.Parameters.size());
  for (const auto &ParamDoc : In.Parameters) {
    Doc.Parameters.emplace_back(ParameterDocumentation<StrOut>{
        StrOut(ParamDoc.Name), StrOut(ParamDoc.Description)});
  }

  Doc.TemplateParameters.reserve(In.TemplateParameters.size());
  for (const auto &ParamDoc : In.TemplateParameters) {
    Doc.TemplateParameters.emplace_back(ParameterDocumentation<StrOut>{
        StrOut(ParamDoc.Name), StrOut(ParamDoc.Description)});
  }

  Doc.Description = In.Description;
  Doc.CommentText = In.CommentText;

  return Doc;
}

template <> SymbolDocumentationRef SymbolDocumentationOwned::toRef() const {
  return convert<llvm::StringRef>(*this);
}

template <> SymbolDocumentationOwned SymbolDocumentationRef::toOwned() const {
  return convert<std::string>(*this);
}

template class SymbolDocumentation<std::string>;
template class SymbolDocumentation<llvm::StringRef>;

} // namespace clangd
} // namespace clang
