====================================================
Extra Clang Tools |release| |ReleaseNotesTitle|
====================================================

.. contents::
   :local:
   :depth: 3

Written by the `LLVM Team <https://llvm.org/>`_

.. only:: PreRelease

  .. warning::
     These are in-progress notes for the upcoming Extra Clang Tools |version| release.
     Release notes for previous releases can be found on
     `the Download Page <https://releases.llvm.org/download.html>`_.

Introduction
============

This document contains the release notes for the Extra Clang Tools, part of the
Clang release |release|. Here we describe the status of the Extra Clang Tools in
some detail, including major improvements from the previous release and new
feature work. All LLVM releases may be downloaded from the `LLVM releases web
site <https://llvm.org/releases/>`_.

For more information about Clang or LLVM, including information about
the latest release, please see the `Clang Web Site <https://clang.llvm.org>`_ or
the `LLVM Web Site <https://llvm.org>`_.

Note that if you are reading this file from a Git checkout or the
main Clang web page, this document applies to the *next* release, not
the current one. To see the release notes for a specific release, please
see the `releases page <https://llvm.org/releases/>`_.

What's New in Extra Clang Tools |release|?
==========================================

Some of the major new features and improvements to Extra Clang Tools are listed
here. Generic improvements to Extra Clang Tools as a whole or to its underlying
infrastructure are described first, followed by tool-specific sections.

Major New Features
------------------

...

Improvements to clangd
----------------------

Inlay hints
^^^^^^^^^^^

Diagnostics
^^^^^^^^^^^

Semantic Highlighting
^^^^^^^^^^^^^^^^^^^^^

- Improved semantic token coverage in some edge cases, e.g. IndirectFieldDecl

Compile flags
^^^^^^^^^^^^^

Hover
^^^^^

Code completion
^^^^^^^^^^^^^^^

- ``--function-arg-placeholders=0`` is now respected for variable template argument lists
   as well
- Macro proposals now use the completion item kind ``Constant`` (for object-like macros)
  or ``Function`` (for function-style macros) even for proposals coming from the index

Code actions
^^^^^^^^^^^^

- The "extract variable" tweak is no longer offered for the initializer expression of a
  declaration
- The tweak for turning unscoped into scoped enums now removes redundant prefixes
  from the enum values.
- Support "move function body out-of-line" in non-header files as well

Signature help
^^^^^^^^^^^^^^

- Signature help now shows function argument names for calls through pointers to
  functions in struct fields

Cross-references
^^^^^^^^^^^^^^^^

- Improve go-to-definition for some concept references

Document outline
^^^^^^^^^^^^^^^^

- Improved precision of document outline information for symbols whose definitions
  involve macro expansions

Clang-tidy integration
^^^^^^^^^^^^^^^^^^^^^^

- The quick fix for clang-tidy's ``readability-identifier-naming`` diagnostic is now
  hooked to invoke ``textDocument/rename``, renaming the identifier across the whole
  project rather than just the translation unit of the diagnostic
- ``misc-const-correctness`` can now be enabled with ``FastCheckFilter: None``
  (previously clangd would force it off unconditionally due to its run time)

Objective-C
^^^^^^^^^^^

- Added support for renaming Objective-C methods

Miscellaneous
^^^^^^^^^^^^^

- Worked around a clang-format bug that caused memory exhaustion when opening some large
  ``.h`` files due to the formatter's language guessing heuristic (#GH85703)
- Various other stability improvements, e.g. crash fixes
- Added a boolean option `AnalyzeAngledIncludes` to `Includes` config section,
  which allows to enable unused includes detection for all angled ("system") headers.
  At this moment umbrella headers are not supported, so enabling this option
  may result in false-positives.

Improvements to clang-doc
-------------------------

Improvements to clang-query
---------------------------

Improvements to clang-rename
----------------------------

The improvements are...

Improvements to clang-tidy
--------------------------

New checks
^^^^^^^^^^

New check aliases
^^^^^^^^^^^^^^^^^

Changes in existing checks
^^^^^^^^^^^^^^^^^^^^^^^^^^

- Improved :doc:`modernize-use-std-format
  <clang-tidy/checks/modernize/use-std-format>` check to support replacing
  member function calls too.

- Improved :doc:`modernize-use-std-print
  <clang-tidy/checks/modernize/use-std-print>` check to support replacing
  member function calls too.

- Improved :doc:`modernize-use-using <clang-tidy/checks/modernize/use-using>`
  check by adding support for detection of typedefs declared on function level.

- Improved :doc:`performance-inefficient-vector-operation
  <clang-tidy/checks/performance/inefficient-vector-operation>` fixing false
  negatives caused by different variable definition type and variable initial
  value type in loop initialization expression.

- Improved :doc:`performance-move-const-arg
  <clang-tidy/checks/performance/move-const-arg>` check by ignoring
  ``std::move()`` calls when their target is used as an rvalue.

- Improved :doc:`performance-unnecessary-copy-initialization
  <clang-tidy/checks/performance/unnecessary-copy-initialization>` check by
  detecting more cases of constant access. In particular, pointers can be
  analyzed, so the check now handles the common patterns
  `const auto e = (*vector_ptr)[i]` and `const auto e = vector_ptr->at(i);`.
  Calls to mutable function where there exists a `const` overload are also
  handled. Fix crash in the case of a non-member operator call.

- Improved :doc:`performance-unnecessary-value-param
  <clang-tidy/checks/performance/unnecessary-value-param>` check
  detecting more cases for template functions including lambdas with ``auto``.
  E.g., ``std::sort(a.begin(), a.end(), [](auto x, auto y) { return a > b; });``
  will be detected for expensive to copy types. Fixed false positives for
  dependent call expressions.

- Improved :doc:`readability-avoid-return-with-void-value
  <clang-tidy/checks/readability/avoid-return-with-void-value>` check by adding
  fix-its.

- Improved :doc:`readability-const-return-type
  <clang-tidy/checks/readability/const-return-type>` check to eliminate false
  positives when returning types with const not at the top level.

- Improved :doc:`readability-container-size-empty
  <clang-tidy/checks/readability/container-size-empty>` check to prevent false
  positives when utilizing ``size`` or ``length`` methods that accept parameter.
  Fixed crash when facing template user defined literals.

- Improved :doc:`readability-duplicate-include
  <clang-tidy/checks/readability/duplicate-include>` check by excluding include
  directives that form the filename using macro.

- Improved :doc:`readability-else-after-return
  <clang-tidy/checks/readability/else-after-return>` check to ignore
  `if consteval` statements, for which the `else` branch must not be removed.

- Improved :doc:`readability-identifier-naming
  <clang-tidy/checks/readability/identifier-naming>` check in `GetConfigPerFile`
  mode by resolving symbolic links to header files. Fixed handling of Hungarian
  Prefix when configured to `LowerCase`. Added support for renaming designated
  initializers. Added support for renaming macro arguments. Fixed renaming
  conflicts arising from out-of-line member function template definitions.

- Improved :doc:`readability-implicit-bool-conversion
  <clang-tidy/checks/readability/implicit-bool-conversion>` check to provide
  valid fix suggestions for ``static_cast`` without a preceding space and
  fixed problem with duplicate parentheses in double implicit casts. Corrected
  the fix suggestions for C23 and later by using C-style casts instead of
  ``static_cast``. Fixed false positives in C++20 spaceship operator by ignoring
  casts in implicit and defaulted functions.

- Improved :doc:`readability-non-const-parameter
  <clang-tidy/checks/readability/non-const-parameter>` check to not crash when
  redeclaration have fewer parameters than expected.

- Improved :doc:`readability-redundant-inline-specifier
  <clang-tidy/checks/readability/redundant-inline-specifier>` check to properly
  emit warnings for static data member with an in-class initializer.

- Improved :doc:`readability-redundant-member-init
  <clang-tidy/checks/readability/redundant-member-init>` check to avoid
  false-positives when type of the member does not match the type of the
  initializer.

- Improved :doc:`readability-static-accessed-through-instance
  <clang-tidy/checks/readability/static-accessed-through-instance>` check to
  support calls to overloaded operators as base expression and provide fixes to
  expressions with side-effects.

- Improved :doc:`readability-simplify-boolean-expr
  <clang-tidy/checks/readability/simplify-boolean-expr>` check to avoid to emit
  warning for macro when IgnoreMacro option is enabled and improve messages
  when auto-fix does not work.

- Improved :doc:`readability-static-definition-in-anonymous-namespace
  <clang-tidy/checks/readability/static-definition-in-anonymous-namespace>`
  check by resolving fix-it overlaps in template code by disregarding implicit
  instances.

- Improved :doc:`readability-string-compare
  <clang-tidy/checks/readability/string-compare>` check to also detect
  usages of ``std::string_view::compare``. Added a `StringLikeClasses` option
  to detect usages of ``compare`` method in custom string-like classes.

Removed checks
^^^^^^^^^^^^^^

Miscellaneous
^^^^^^^^^^^^^

Improvements to include-fixer
-----------------------------

The improvements are...

Improvements to clang-include-fixer
-----------------------------------

The improvements are...

Improvements to modularize
--------------------------

The improvements are...

Improvements to pp-trace
------------------------

Clang-tidy Visual Studio plugin
-------------------------------
