===========================================
Clang |release| |ReleaseNotesTitle|
===========================================

.. contents::
   :local:
   :depth: 2

Written by the `LLVM Team <https://llvm.org/>`_

.. only:: PreRelease

  .. warning::
     These are in-progress notes for the upcoming Clang |version| release.
     Release notes for previous releases can be found on
     `the Releases Page <https://llvm.org/releases/>`_.

Introduction
============

This document contains the release notes for the Clang C/C++/Objective-C
frontend, part of the LLVM Compiler Infrastructure, release |release|. Here we
describe the status of Clang in some detail, including major
improvements from the previous release and new feature work. For the
general LLVM release notes, see `the LLVM
documentation <https://llvm.org/docs/ReleaseNotes.html>`_. For the libc++ release notes,
see `this page <https://libcxx.llvm.org/ReleaseNotes.html>`_. All LLVM releases
may be downloaded from the `LLVM releases web site <https://llvm.org/releases/>`_.

For more information about Clang or LLVM, including information about the
latest release, please see the `Clang Web Site <https://clang.llvm.org>`_ or the
`LLVM Web Site <https://llvm.org>`_.

Potentially Breaking Changes
============================
These changes are ones which we think may surprise users when upgrading to
Clang |release| because of the opportunity they pose for disruption to existing
code bases.

- The ``le32`` and ``le64`` targets have been removed.

C/C++ Language Potentially Breaking Changes
-------------------------------------------

C++ Specific Potentially Breaking Changes
-----------------------------------------

- The type trait builtin ``__is_nullptr`` has been removed, since it has very
  few users and can be written as ``__is_same(__remove_cv(T), decltype(nullptr))``,
  which GCC supports as well.

- Clang will now correctly diagnose as ill-formed a constant expression where an
  enum without a fixed underlying type is set to a value outside the range of
  the enumeration's values.

  .. code-block:: c++

    enum E { Zero, One, Two, Three, Four };
    constexpr E Val1 = (E)3;  // Ok
    constexpr E Val2 = (E)7;  // Ok
    constexpr E Val3 = (E)8;  // Now ill-formed, out of the range [0, 7]
    constexpr E Val4 = (E)-1; // Now ill-formed, out of the range [0, 7]

  Since Clang 16, it has been possible to suppress the diagnostic via
  `-Wno-enum-constexpr-conversion`, to allow for a transition period for users.
  Now, in Clang 20, **it is no longer possible to suppress the diagnostic**.

- Extraneous template headers are now ill-formed by default.
  This error can be disable with ``-Wno-error=extraneous-template-head``.

  .. code-block:: c++

    template <> // error: extraneous template head
    template <typename T>
    void f();

ABI Changes in This Version
---------------------------

- Fixed Microsoft name mangling of placeholder, auto and decltype(auto), return types for MSVC 1920+. This change resolves incompatibilities with code compiled by MSVC 1920+ but will introduce incompatibilities with code compiled by earlier versions of Clang unless such code is built with the compiler option -fms-compatibility-version=19.14 to imitate the MSVC 1914 mangling behavior.

AST Dumping Potentially Breaking Changes
----------------------------------------

Clang Frontend Potentially Breaking Changes
-------------------------------------------
- Removed support for constructing on-stack ``TemplateArgumentList``\ s; interfaces should instead
  use ``ArrayRef<TemplateArgument>`` to pass template arguments. Transitioning internal uses to
  ``ArrayRef<TemplateArgument>`` reduces AST memory usage by 0.4% when compiling clang, and is
  expected to show similar improvements on other workloads.

- The ``-Wgnu-binary-literal`` diagnostic group no longer controls any
  diagnostics. Binary literals are no longer a GNU extension, they're now a C23
  extension which is controlled via ``-pedantic`` or ``-Wc23-extensions``. Use
  of ``-Wno-gnu-binary-literal`` will no longer silence this pedantic warning,
  which may break existing uses with ``-Werror``.

- The normalization of 3 element target triples where ``-none-`` is the middle
  element has changed. For example, ``armv7m-none-eabi`` previously normalized
  to ``armv7m-none-unknown-eabi``, with ``none`` for the vendor and ``unknown``
  for the operating system. It now normalizes to ``armv7m-unknown-none-eabi``,
  which has ``unknown`` vendor and ``none`` operating system.

  The affected triples are primarily for bare metal Arm where it is intended
  that ``none`` means that there is no operating system. As opposed to an unknown
  type of operating system.

  This change can cause clang to not find libraries, or libraries to be built at
  different file system locations. This can be fixed by changing your builds to
  use the new normalized triple. However, we recommend instead getting the
  normalized triple from clang itself, as this will make your builds more
  robust in case of future changes::

    $ clang --target=<your target triple> -print-target-triple
    <the normalized target triple>

- The ``hasTypeLoc`` AST matcher will no longer match a ``classTemplateSpecializationDecl``;
  existing uses should switch to ``templateArgumentLoc`` or ``hasAnyTemplateArgumentLoc`` instead.

Clang Python Bindings Potentially Breaking Changes
--------------------------------------------------
- Parts of the interface returning string results will now return
  the empty string ``""`` when no result is available, instead of ``None``.
- Calling a property on the ``CompletionChunk`` or ``CompletionString`` class
  statically now leads to an error, instead of returning a ``CachedProperty`` object
  that is used internally. Properties are only available on instances.
- For a single-line ``SourceRange`` and a ``SourceLocation`` in the same line,
  but after the end of the ``SourceRange``, ``SourceRange.__contains__``
  used to incorrectly return ``True``. (#GH22617), (#GH52827)

What's New in Clang |release|?
==============================
Some of the major new features and improvements to Clang are listed
here. Generic improvements to Clang as a whole or to its underlying
infrastructure are described first, followed by language-specific
sections with improvements to Clang's support for those languages.

C++ Language Changes
--------------------
- Allow single element access of GCC vector/ext_vector_type object to be
  constant expression. Supports the `V.xyzw` syntax and other tidbits
  as seen in OpenCL. Selecting multiple elements is left as a future work.

C++2c Feature Support
^^^^^^^^^^^^^^^^^^^^^

- Add ``__builtin_is_implicit_lifetime`` intrinsic, which supports
  `P2647R1 A trait for implicit lifetime types <https://wg21.link/p2674r1>`_

- Add ``__builtin_is_virtual_base_of`` intrinsic, which supports
  `P2985R0 A type trait for detecting virtual base classes <https://wg21.link/p2985r0>`_

- Implemented `P2893R3 Variadic Friends <https://wg21.link/P2893>`_

- Implemented `P2747R2 constexpr placement new <https://wg21.link/P2747R2>`_.

C++23 Feature Support
^^^^^^^^^^^^^^^^^^^^^
- Removed the restriction to literal types in constexpr functions in C++23 mode.

C++20 Feature Support
^^^^^^^^^^^^^^^^^^^^^


Resolutions to C++ Defect Reports
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Allow calling initializer list constructors from initializer lists with
  a single element of the same type instead of always copying.
  (`CWG2137: List-initialization from object of same type <https://cplusplus.github.io/CWG/issues/2137.html>`)

- Speculative resolution for CWG2311 implemented so that the implementation of CWG2137 doesn't remove
  previous cases where guaranteed copy elision was done. Given a prvalue ``e`` of class type
  ``T``, ``T{e}`` will try to resolve an initializer list constructor and will use it if successful.
  Otherwise, if there is no initializer list constructor, the copy will be elided as if it was ``T(e)``.
  (`CWG2311: Missed case for guaranteed copy elision <https://cplusplus.github.io/CWG/issues/2311.html>`)

- Casts from a bit-field to an integral type is now not considered narrowing if the
  width of the bit-field means that all potential values are in the range
  of the target type, even if the type of the bit-field is larger.
  (`CWG2627: Bit-fields and narrowing conversions <https://cplusplus.github.io/CWG/issues/2627.html>`_)

- ``nullptr`` is now promoted to ``void*`` when passed to a C-style variadic function.
  (`CWG722: Can nullptr be passed to an ellipsis? <https://cplusplus.github.io/CWG/issues/722.html>`_)

- Allow ``void{}`` as a prvalue of type ``void``.
  (`CWG2351: void{} <https://cplusplus.github.io/CWG/issues/2351.html>`_).

C Language Changes
------------------

C2y Feature Support
^^^^^^^^^^^^^^^^^^^

C23 Feature Support
^^^^^^^^^^^^^^^^^^^

Non-comprehensive list of changes in this release
-------------------------------------------------

- The floating point comparison builtins (``__builtin_isgreater``,
  ``__builtin_isgreaterequal``, ``__builtin_isless``, etc.) and
  ``__builtin_signbit`` can now be used in constant expressions.

- The type traits builtin ``__is_nullptr`` is deprecated in CLang 19 and will be
  removed in Clang 20. ``__is_same(__remove_cv(T), decltype(nullptr))`` can be
  used instead to check whether a type ``T`` is a ``nullptr``.

New Compiler Flags
------------------

Deprecated Compiler Flags
-------------------------

- ``-fheinous-gnu-extensions`` is deprecated; it is now equivalent to
  specifying ``-Wno-error=invalid-gnu-asm-cast`` and may be removed in the
  future.

Modified Compiler Flags
-----------------------

- The ``-ffp-model`` option has been updated to enable a more limited set of
  optimizations when the ``fast`` argument is used and to accept a new argument,
  ``aggressive``. The behavior of ``-ffp-model=aggressive`` is equivalent
  to the previous behavior of ``-ffp-model=fast``. The updated
  ``-ffp-model=fast`` behavior no longer assumes finite math only and uses
  the ``promoted`` algorithm for complex division when possible rather than the
  less basic (limited range) algorithm.

Removed Compiler Flags
-------------------------

- The compiler flag `-Wenum-constexpr-conversion` (and the `Wno-`, `Wno-error-`
  derivatives) is now removed, since it's no longer possible to suppress the
  diagnostic (see above). Users can expect an `unknown warning` diagnostic if
  it's still in use.

Attribute Changes in Clang
--------------------------

- Clang now disallows more than one ``__attribute__((ownership_returns(class, idx)))`` with
  different class names attached to one function.

- Introduced a new format attribute ``__attribute__((format(syslog, 1, 2)))`` from OpenBSD.

- The ``hybrid_patchable`` attribute is now supported on ARM64EC targets. It can be used to specify
  that a function requires an additional x86-64 thunk, which may be patched at runtime.

- ``[[clang::lifetimebound]]`` is now explicitly disallowed on explicit object member functions
  where they were previously silently ignored.

- The ``hybrid_patchable`` attribute is now supported on ARM64EC targets. It can be used to specify
  that a function requires an additional x86-64 thunk, which may be patched at runtime.

Improvements to Clang's diagnostics
-----------------------------------

- Some template related diagnostics have been improved.

  .. code-block:: c++

     void foo() { template <typename> int i; } // error: templates can only be declared in namespace or class scope

     struct S {
      template <typename> int i; // error: non-static data member 'i' cannot be declared as a template
     };

- Clang now has improved diagnostics for functions with explicit 'this' parameters. Fixes #GH97878

- Clang now diagnoses dangling references to fields of temporary objects. Fixes #GH81589.

- Clang now diagnoses undefined behavior in constant expressions more consistently. This includes invalid shifts, and signed overflow in arithmetic.

- -Wdangling-assignment-gsl is enabled by default.
- Clang now always preserves the template arguments as written used
  to specialize template type aliases.

- Clang now diagnoses the use of ``main`` in an ``extern`` context as invalid according to [basic.start.main] p3. Fixes #GH101512.

- Clang now diagnoses when the result of a [[nodiscard]] function is discarded after being cast in C. Fixes #GH104391.

- Don't emit duplicated dangling diagnostics. (#GH93386).

- Improved diagnostic when trying to befriend a concept. (#GH45182).

- Clang now diagnoses integer constant expressions that are folded to a constant value as an extension in more circumstances. Fixes #GH59863

- Clang now diagnoses dangling assignments for pointer-like objects (annotated with `[[gsl::Pointer]]`) under `-Wdangling-assignment-gsl` (off by default)
  Fixes #GH63310.

- Clang now diagnoses uses of alias templates with a deprecated attribute. (Fixes #GH18236).

  .. code-block:: c++

     template <typename T>
     struct NoAttr {
     };

     template <typename T>
     using UsingWithAttr __attribute__((deprecated)) = NoAttr<T>;

     UsingWithAttr<int> objUsingWA; // warning: 'UsingWithAttr' is deprecated

- Clang now diagnoses undefined behavior in constant expressions more consistently. This includes invalid shifts, and signed overflow in arithmetic.

- Clang now diagnoses dangling references to fields of temporary objects. Fixes #GH81589.


Improvements to Clang's time-trace
----------------------------------

Improvements to Coverage Mapping
--------------------------------

Bug Fixes in This Version
-------------------------

- Fixed the definition of ``ATOMIC_FLAG_INIT`` in ``<stdatomic.h>`` so it can
  be used in C++.
- Fixed a failed assertion when checking required literal types in C context. (#GH101304).
- Fixed a crash when trying to transform a dependent address space type. Fixes #GH101685.
- Fixed a crash when diagnosing format strings and encountering an empty
  delimited escape sequence (e.g., ``"\o{}"``). #GH102218

- Fixed the definition of ``ATOMIC_FLAG_INIT`` in ``<stdatomic.h>`` so it can
  be used in C++.

Bug Fixes to Compiler Builtins
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Bug Fixes to Attribute Support
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Bug Fixes to C++ Support
^^^^^^^^^^^^^^^^^^^^^^^^

- Fix crash when calling the constructor of an invalid class.
  (#GH10518) (#GH67914) (#GH78388)
- Fix crash when using lifetimebound attribute in function with trailing return.
  (#GH73619)
- Addressed an issue where constraints involving injected class types are perceived
  distinct from its specialization types. (#GH56482)
- Fixed a bug where variables referenced by requires-clauses inside
  nested generic lambdas were not properly injected into the constraint scope. (#GH73418)
- Fixed a crash where substituting into a requires-expression that refers to function
  parameters during the equivalence determination of two constraint expressions.
  (#GH74447)
- Fixed deducing auto& from const int in template parameters of partial
  specializations. (#GH77189)
- Fix for crash when using a erroneous type in a return statement.
  (#GH63244) (#GH79745)
- Fixed an out-of-bounds error caused by building a recovery expression for ill-formed
  function calls while substituting into constraints. (#GH58548)
- Fix incorrect code generation caused by the object argument
  of ``static operator()`` and ``static operator[]`` calls not being evaluated. (#GH67976)
- Fix crash and diagnostic with const qualified member operator new.
  Fixes (#GH79748)
- Fixed a crash where substituting into a requires-expression that involves parameter packs
  during the equivalence determination of two constraint expressions. (#GH72557)
- Fix a crash when specializing an out-of-line member function with a default
  parameter where we did an incorrect specialization of the initialization of
  the default parameter. (#GH68490)
- Fix a crash when trying to call a varargs function that also has an explicit object parameter.
  Fixes (#GH80971)
- Reject explicit object parameters on `new` and `delete` operators. (#GH82249)
- Fix a crash when trying to call a varargs function that also has an explicit object parameter. (#GH80971)
- Fixed a bug where abbreviated function templates would append their invented template parameters to
  an empty template parameter lists.
- Fix parsing of abominable function types inside type traits. Fixes #GH77585
- Clang now classifies aggregate initialization in C++17 and newer as constant
  or non-constant more accurately. Previously, only a subset of the initializer
  elements were considered, misclassifying some initializers as constant. Partially fixes
  #GH80510.
- Clang now ignores top-level cv-qualifiers on function parameters in template partial orderings. (#GH75404)
- No longer reject valid use of the ``_Alignas`` specifier when declaring a
  local variable, which is supported as a C11 extension in C++. Previously, it
  was only accepted at namespace scope but not at local function scope.
- Clang no longer tries to call consteval constructors at runtime when they appear in a member initializer. (#GH82154)
- Fix crash when using an immediate-escalated function at global scope. (#GH82258)
- Correctly immediate-escalate lambda conversion functions. (#GH82258)
- Fixed an issue where template parameters of a nested abbreviated generic lambda within
  a requires-clause lie at the same depth as those of the surrounding lambda. This,
  in turn, results in the wrong template argument substitution during constraint checking.
  (#GH78524)
- Clang no longer instantiates the exception specification of discarded candidate function
  templates when determining the primary template of an explicit specialization.
- Fixed a crash in Microsoft compatibility mode where unqualified dependent base class
  lookup searches the bases of an incomplete class.
- Fix a crash when an unresolved overload set is encountered on the RHS of a ``.*`` operator.
  (#GH53815)
- In ``__restrict``-qualified member functions, attach ``__restrict`` to the pointer type of
  ``this`` rather than the pointee type.
  Fixes (#GH82941), (#GH42411) and (#GH18121).
- Clang now properly reports supported C++11 attributes when using
  ``__has_cpp_attribute`` and parses attributes with arguments in C++03 (#GH82995)
- Clang now properly diagnoses missing 'default' template arguments on a variety
  of templates. Previously we were diagnosing on any non-function template
  instead of only on class, alias, and variable templates, as last updated by
  CWG2032. Fixes (#GH83461)
- Fixed an issue where an attribute on a declarator would cause the attribute to
  be destructed prematurely. This fixes a pair of Chromium that were brought to
  our attention by an attempt to fix in (#GH77703). Fixes (#GH83385).
- Fix evaluation of some immediate calls in default arguments.
  Fixes (#GH80630)
- Fixed an issue where the ``RequiresExprBody`` was involved in the lambda dependency
  calculation. (#GH56556), (#GH82849).
- Fix a bug where overload resolution falsely reported an ambiguity when it was comparing
  a member-function against a non member function or a member-function with an
  explicit object parameter against a member function with no explicit object parameter
  when one of the function had more specialized templates. Fixes #GH82509 and #GH74494
- Clang now supports direct lambda calls inside of a type alias template declarations.
  This addresses (#GH70601), (#GH76674), (#GH79555), (#GH81145) and (#GH82104).
- Allow access to a public template alias declaration that refers to friend's
  private nested type. (#GH25708).
- Fixed a crash in constant evaluation when trying to access a
  captured ``this`` pointer in a lambda with an explicit object parameter.
  Fixes (#GH80997)
- Fix an issue where missing set friend declaration in template class instantiation.
  Fixes (#GH84368).
- Fixed a crash while checking constraints of a trailing requires-expression of a lambda, that the
  expression references to an entity declared outside of the lambda. (#GH64808)
- Clang's __builtin_bit_cast will now produce a constant value for records with empty bases. See:
  (#GH82383)
- Fix a crash when instantiating a lambda that captures ``this`` outside of its context. Fixes (#GH85343).
- Fix an issue where a namespace alias could be defined using a qualified name (all name components
  following the first `::` were ignored).
- Fix an out-of-bounds crash when checking the validity of template partial specializations. (part of #GH86757).
- Fix an issue caused by not handling invalid cases when substituting into the parameter mapping of a constraint. Fixes (#GH86757).
- Fixed a bug that prevented member function templates of class templates declared with a deduced return type
  from being explicitly specialized for a given implicit instantiation of the class template.
- Fixed a crash when ``this`` is used in a dependent class scope function template specialization
  that instantiates to a static member function.
- Fix crash when inheriting from a cv-qualified type. Fixes #GH35603
- Fix a crash when the using enum declaration uses an anonymous enumeration. Fixes (#GH86790).
- Handled an edge case in ``getFullyPackExpandedSize`` so that we now avoid a false-positive diagnostic. (#GH84220)
- Clang now correctly tracks type dependence of by-value captures in lambdas with an explicit
  object parameter.
  Fixes (#GH70604), (#GH79754), (#GH84163), (#GH84425), (#GH86054), (#GH86398), and (#GH86399).
- Fix a crash when deducing ``auto`` from an invalid dereference (#GH88329).
- Fix a crash in requires expression with templated base class member function. Fixes (#GH84020).
- Fix a crash caused by defined struct in a type alias template when the structure
  has fields with dependent type. Fixes (#GH75221).
- Fix the Itanium mangling of lambdas defined in a member of a local class (#GH88906)
- Fixed a crash when trying to evaluate a user-defined ``static_assert`` message whose ``size()``
  function returns a large or negative value. Fixes (#GH89407).
- Fixed a use-after-free bug in parsing of type constraints with default arguments that involve lambdas. (#GH67235)
- Fixed bug in which the body of a consteval lambda within a template was not parsed as within an
  immediate function context.
- Fix CTAD for ``std::initializer_list``. This allows ``std::initializer_list{1, 2, 3}`` to be deduced as
  ``std::initializer_list<int>`` as intended.
- Fix a bug on template partial specialization whose template parameter is `decltype(auto)`.
- Fix a bug on template partial specialization with issue on deduction of nontype template parameter
  whose type is `decltype(auto)`. Fixes (#GH68885).
- Clang now correctly treats the noexcept-specifier of a friend function to be a complete-class context.
- Fix an assertion failure when parsing an invalid members of an anonymous class. (#GH85447)
- Fixed a misuse of ``UnresolvedLookupExpr`` for ill-formed templated expressions. Fixes (#GH48673), (#GH63243)
  and (#GH88832).
- Clang now defers all substitution into the exception specification of a function template specialization
  until the noexcept-specifier is instantiated.
- Fix a crash when an implicitly declared ``operator==`` function with a trailing requires-clause has its
  constraints compared to that of another declaration.
- Fix a bug where explicit specializations of member functions/function templates would have substitution
  performed incorrectly when checking constraints. Fixes (#GH90349).
- Clang now allows constrained member functions to be explicitly specialized for an implicit instantiation
  of a class template.
- Fix a C++23 bug in implementation of P2564R3 which evaluates immediate invocations in place
  within initializers for variables that are usable in constant expressions or are constant
  initialized, rather than evaluating them as a part of the larger manifestly constant evaluated
  expression.
- Fix a bug in access control checking due to dealyed checking of friend declaration. Fixes (#GH12361).
- Correctly treat the compound statement of an ``if consteval`` as an immediate context. Fixes (#GH91509).
- When partial ordering alias templates against template template parameters,
  allow pack expansions when the alias has a fixed-size parameter list. Fixes (#GH62529).
- Clang now ignores template parameters only used within the exception specification of candidate function
  templates during partial ordering when deducing template arguments from a function declaration or when
  taking the address of a function template.
- Fix a bug with checking constrained non-type template parameters for equivalence. Fixes (#GH77377).
- Fix a bug where the last argument was not considered when considering the most viable function for
  explicit object argument member functions. Fixes (#GH92188).
- Fix a C++11 crash when a non-const non-static member function is defined out-of-line with
  the ``constexpr`` specifier. Fixes (#GH61004).
- Clang no longer transforms dependent qualified names into implicit class member access expressions
  until it can be determined whether the name is that of a non-static member.
- Clang now correctly diagnoses when the current instantiation is used as an incomplete base class.
- Clang no longer treats ``constexpr`` class scope function template specializations of non-static members
  as implicitly ``const`` in language modes after C++11.
- Fixed a crash when trying to emit captures in a lambda call operator with an explicit object
  parameter that is called on a derived type of the lambda.
  Fixes (#GH87210), (GH89541).
- Clang no longer tries to check if an expression is immediate-escalating in an unevaluated context.
  Fixes (#GH91308).
- Fix a crash caused by a regression in the handling of ``source_location``
  in dependent contexts. Fixes (#GH92680).
- Fixed a crash when diagnosing failed conversions involving template parameter
  packs. (#GH93076)
- Fixed a regression introduced in Clang 18 causing a static function overloading a non-static function
  with the same parameters not to be diagnosed. (Fixes #GH93456).
- Clang now diagnoses unexpanded parameter packs in attributes. (Fixes #GH93269).
- Clang now allows ``@$``` in raw string literals. Fixes (#GH93130).
- Fix an assertion failure when checking invalid ``this`` usage in the wrong context. (Fixes #GH91536).
- Clang no longer models dependent NTTP arguments as ``TemplateParamObjectDecl`` s. Fixes (#GH84052).
- Fix incorrect merging of modules which contain using declarations which shadow
  other declarations. This could manifest as ODR checker false positives.
  Fixes (`#80252 <https://github.com/llvm/llvm-project/issues/80252>`_)
- Fix a regression introduced in Clang 18 causing incorrect overload resolution in the presence of functions only
  differering by their constraints when only one of these function was variadic.
- Fix a crash when a variable is captured by a block nested inside a lambda. (Fixes #GH93625).
- Fixed a type constraint substitution issue involving a generic lambda expression. (#GH93821)
- Fix a crash caused by improper use of ``__array_extent``. (#GH80474)
- Fixed several bugs in capturing variables within unevaluated contexts. (#GH63845), (#GH67260), (#GH69307),
  (#GH88081), (#GH89496), (#GH90669), (#GH91633) and (#GH97453).
- Fixed a crash in constraint instantiation under nested lambdas with dependent parameters.
- Fixed handling of brace ellison when building deduction guides. (#GH64625), (#GH83368).
- Fixed a failed assertion when attempting to convert an integer representing the difference
  between the addresses of two labels (a GNU extension) to a pointer within a constant expression. (#GH95366).
- Fix immediate escalation bugs in the presence of dependent call arguments. (#GH94935)
- Clang now diagnoses explicit specializations with storage class specifiers in all contexts.
- Fix an assertion failure caused by parsing a lambda used as a default argument for the value of a
  forward-declared class. (#GH93512).
- Fixed a bug in access checking inside return-type-requirement of compound requirements. (#GH93788).
- Fixed an assertion failure about invalid conversion when calling lambda. (#GH96205).
- Fixed a bug where the first operand of binary ``operator&`` would be transformed as if it was the operand
  of the address of operator. (#GH97483).
- Fixed an assertion failure about a constant expression which is a known integer but is not
  evaluated to an integer. (#GH96670).
- Fixed a bug where references to lambda capture inside a ``noexcept`` specifier were not correctly
  instantiated. (#GH95735).
- Fixed a CTAD substitution bug involving type aliases that reference outer template parameters. (#GH94614).
- Clang now correctly handles unexpanded packs in the template parameter list of a generic lambda expression
  (#GH48937)
- Fix a crash when parsing an invalid type-requirement in a requires expression. (#GH51868)
- Fix parsing of built-in type-traits such as ``__is_pointer`` in libstdc++ headers. (#GH95598)
- Fixed failed assertion when resolving context of defaulted comparison method outside of struct. (#GH96043).
- Clang now diagnoses explicit object parameters in member pointers and other contexts where they should not appear.
  Fixes (#GH85992).
- Fixed a crash-on-invalid bug involving extraneous template parameter with concept substitution. (#GH73885)
- Fixed assertion failure by skipping the analysis of an invalid field declaration. (#GH99868)
- Fix an issue with dependent source location expressions (#GH106428), (#GH81155), (#GH80210), (#GH85373)
- Fix handling of ``_`` as the name of a lambda's init capture variable. (#GH107024)


Bug Fixes to AST Handling
^^^^^^^^^^^^^^^^^^^^^^^^^

- Fixed a crash that occurred when dividing by zero in complex integer division. (#GH55390).

Miscellaneous Bug Fixes
^^^^^^^^^^^^^^^^^^^^^^^

Miscellaneous Clang Crashes Fixed
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

- Fixed a crash in C due to incorrect lookup that members in nested anonymous struct/union
  can be found as ordinary identifiers in struct/union definition. (#GH31295)

- Fixed a crash caused by long chains of ``sizeof`` and other similar operators
  that can be followed by a non-parenthesized expression. (#GH45061)

- Fixed an crash when compiling ``#pragma STDC FP_CONTRACT DEFAULT`` with
  ``-ffp-contract=fast-honor-pragmas``. (#GH104830)

- Fixed a crash when function has more than 65536 parameters.
  Now a diagnostic is emitted. (#GH35741)

OpenACC Specific Changes
------------------------

Target Specific Changes
-----------------------

AMDGPU Support
^^^^^^^^^^^^^^

X86 Support
^^^^^^^^^^^

- The MMX vector intrinsic functions from ``*mmintrin.h`` which
  operate on `__m64` vectors, such as ``_mm_add_pi8``, have been
  reimplemented to use the SSE2 instruction-set and XMM registers
  unconditionally. These intrinsics are therefore *no longer
  supported* if MMX is enabled without SSE2 -- either from targeting
  CPUs from the Pentium-MMX through the Pentium 3, or explicitly via
  passing arguments such as ``-mmmx -mno-sse2``. MMX assembly code
  remains supported without requiring SSE2, including inside
  inline-assembly.

- The compiler builtins such as ``__builtin_ia32_paddb`` which
  formerly implemented the above MMX intrinsic functions have been
  removed. Any uses of these removed functions should migrate to the
  functions defined by the ``*mmintrin.h`` headers. A mapping can be
  found in the file ``clang/www/builtins.py``.

- Support ISA of ``AVX10.2``.
  * Supported MINMAX intrinsics of ``*_(mask(z)))_minmax(ne)_p[s|d|h|bh]`` and
  ``*_(mask(z)))_minmax_s[s|d|h]``.

Arm and AArch64 Support
^^^^^^^^^^^^^^^^^^^^^^^

- ARMv7+ targets now default to allowing unaligned access, except Armv6-M, and
  Armv8-M without the Main Extension. Baremetal targets should check that the
  new default will work with their system configurations, since it requires
  that SCTLR.A is 0, SCTLR.U is 1, and that the memory in question is
  configured as "normal" memory. This brings Clang in-line with the default
  settings for GCC and Arm Compiler. Aside from making Clang align with other
  compilers, changing the default brings major performance and code size
  improvements for most targets. We have not changed the default behavior for
  ARMv6, but may revisit that decision in the future. Users can restore the old
  behavior with -m[no-]unaligned-access.

- An alias identifier (rdma) has been added for targeting the AArch64
  Architecture Extension which uses Rounding Doubling Multiply Accumulate
  instructions (rdm). The identifier is available on the command line as
  a feature modifier for -march and -mcpu as well as via target attributes
  like ``target_version`` or ``target_clones``.

- Support has been added for the following processors (-mcpu identifiers in parenthesis):
    * Arm Cortex-R52+ (cortex-r52plus).
    * Arm Cortex-R82AE (cortex-r82ae).
    * Arm Cortex-A78AE (cortex-a78ae).
    * Arm Cortex-A520AE (cortex-a520ae).
    * Arm Cortex-A720AE (cortex-a720ae).
    * Arm Cortex-A725 (cortex-a725).
    * Arm Cortex-X925 (cortex-x925).
    * Arm Neoverse-N3 (neoverse-n3).
    * Arm Neoverse-V3 (neoverse-v3).
    * Arm Neoverse-V3AE (neoverse-v3ae).
- ``-mbranch-protection=gcs`` has been added which enables support for the
  Guarded Control Stack extension, and ``-mbranch-protection=standard`` also
  enables this. Enabling GCS causes the GCS GNU property bit to be set on output
  objects. It doesn't cause any code generation changes, as the code generated
  by clang is already compatible with GCS.

 - Experimental support has been added for pointer authentication ABI for С/C++.

 - Pointer authentication ABI could be enabled for AArch64 Linux via
   ``-mabi=pauthtest`` option or via specifying ``pauthtest`` environment part of
   target triple.

 - The C23 ``_BitInt`` implementation has been brought into compliance
   with AAPCS32 and AAPCS64.

Android Support
^^^^^^^^^^^^^^^

Windows Support
^^^^^^^^^^^^^^^

- Clang no longer allows references inside a union when emulating MSVC 1900+ even if `fms-extensions` is enabled.
  Starting with VS2015, MSVC 1900, this Microsoft extension is no longer allowed and always results in an error.
  Clang now follows the MSVC behavior in this scenario.
  When `-fms-compatibility-version=18.00` or prior is set on the command line this Microsoft extension is still
  allowed as VS2013 and prior allow it.

LoongArch Support
^^^^^^^^^^^^^^^^^

RISC-V Support
^^^^^^^^^^^^^^

- ``__attribute__((rvv_vector_bits(N)))`` is now supported for RVV vbool*_t types.
- Profile names in ``-march`` option are now supported.
- Passing empty structs/unions as arguments in C++ is now handled correctly. The behavior is similar to GCC's.
- ``-m[no-]scalar-strict-align`` and ``-m[no-]vector-strict-align`` options have
  been added to give separate control of whether scalar or vector misaligned
  accesses may be created. ``-m[no-]strict-align`` applies to both scalar and
  vector.

PowerPC Support
^^^^^^^^^^^^^^^

- Clang now emits errors for impossible ``__attribute__((musttail))``.
- Added support for ``-mcpu=[pwr11 | power11]`` and ``-mtune=[pwr11 | power11]``.
- Added support for ``builtin_cpu_supports`` on AIX, along with a subset of
  features that can be queried.

CUDA/HIP Language Changes
^^^^^^^^^^^^^^^^^^^^^^^^^

CUDA Support
^^^^^^^^^^^^

AIX Support
^^^^^^^^^^^

- Introduced the ``-maix-small-local-dynamic-tls`` option to produce a faster
  access sequence for local-dynamic TLS variables where the offset from the TLS
  base is encoded as an immediate operand.
  This access sequence is not used for TLS variables larger than 32KB, and is
  currently only supported on 64-bit mode.
- Introduced the options ``-mtocdata/-mno-tocdata`` to enable/disable TOC data
  transformations for the listed suitable variables.
- Introduced the ``-maix-shared-lib-tls-model-opt`` option to enable the tuning
  of changing local-dynamic mode access(es) to initial-exec access(es) at the
  function level on 64-bit mode.
- Clang now emits errors for ``-gdwarf-5``.
- Added the support of the OpenMP runtime libomp on AIX. OpenMP applications can be
  compiled with ``-fopenmp`` and execute on AIX.

NetBSD Support
^^^^^^^^^^^^^^

WebAssembly Support
^^^^^^^^^^^^^^^^^^^

AVR Support
^^^^^^^^^^^

DWARF Support in Clang
----------------------

Floating Point Support in Clang
-------------------------------

Fixed Point Support in Clang
----------------------------

AST Matchers
------------

- Fixed an issue with the `hasName` and `hasAnyName` matcher when matching
  inline namespaces with an enclosing namespace of the same name.

- Fixed an ordering issue with the `hasOperands` matcher occuring when setting a
  binding in the first matcher and using it in the second matcher.

clang-format
------------

- Adds ``BreakBinaryOperations`` option.

libclang
--------
- Add ``clang_isBeforeInTranslationUnit``. Given two source locations, it determines
  whether the first one comes strictly before the second in the source code.

Static Analyzer
---------------

New features
^^^^^^^^^^^^

- MallocChecker now checks for ``ownership_returns(class, idx)`` and ``ownership_takes(class, idx)``
  attributes with class names different from "malloc". Clang static analyzer now reports an error
  if class of allocation and deallocation function mismatches.
  `Documentation <https://clang.llvm.org/docs/analyzer/checkers.html#unix-mismatcheddeallocator-c-c>`__.

Crash and bug fixes
^^^^^^^^^^^^^^^^^^^

- Fixed crashing on loops if the loop variable was declared in switch blocks
  but not under any case blocks if ``unroll-loops=true`` analyzer config is
  set. (#GH68819)

- Fixed a crash in ``security.cert.env.InvalidPtr`` checker when accidentally
  matched user-defined ``strerror`` and similar library functions. (#GH88181)

- Fixed a crash when storing through an address that refers to the address of
  a label. (#GH89185)

- Fixed a crash when using ``__builtin_bitcast(type, array)`` as an array
  subscript. (#GH94496)

- Z3 crosschecking (aka. Z3 refutation) is now bounded, and can't consume
  more total time than the eymbolic execution itself. (#GH97298)

- In clang-18, we regressed in terms of analysis time for projects having many
  nested loops with buffer indexing or shifting or other binary operations.
  For example, functions computing different hash values. Some of this slowdown
  was attributed to taint analysis, which is fixed now. (#GH105493)

- ``std::addressof``, ``std::as_const``, ``std::forward``,
  ``std::forward_like``, ``std::move``, ``std::move_if_noexcept``, are now
  modeled just like their builtin counterpart. (#GH94193)

Improvements
^^^^^^^^^^^^

- Improved the handling of the ``ownership_returns`` attribute. Now, Clang reports an
  error if the attribute is attached to a function that returns a non-pointer value.
  Fixes (#GH99501)

Moved checkers
^^^^^^^^^^^^^^

- The checker ``alpha.security.MallocOverflow`` was deleted because it was
  badly implemented and its agressive logic produced too many false positives.
  To detect too large arguments passed to malloc, consider using the checker
  ``alpha.taint.TaintedAlloc``.

.. _release-notes-sanitizers:

Sanitizers
----------
- Introduced Realtime Sanitizer, activated by using the -fsanitize=realtime
  flag. This sanitizer detects unsafe system library calls, such as memory
  allocations and mutex locks. If any such function is called during invocation
  of a function marked with the ``[[clang::nonblocking]]`` attribute, an error
  is printed to the console and the process exits non-zero.

- Added the ``-fsanitize-undefined-ignore-overflow-pattern`` flag which can be
  used to disable specific overflow-dependent code patterns. The supported
  patterns are: ``add-signed-overflow-test``, ``add-unsigned-overflow-test``,
  ``negated-unsigned-const``, and ``unsigned-post-decr-while``. The sanitizer
  instrumentation can be toggled off for all available patterns by specifying
  ``all``. Conversely, you may disable all exclusions with ``none`` which is
  the default.

  .. code-block:: c++

     /// specified with ``-fsanitize-undefined-ignore-overflow-pattern=add-unsigned-overflow-test``
     int common_overflow_check_pattern(unsigned base, unsigned offset) {
       if (base + offset < base) { /* ... */ } // The pattern of `a + b < a`, and other re-orderings, won't be instrumented
     }

     /// specified with ``-fsanitize-undefined-ignore-overflow-pattern=add-signed-overflow-test``
     int common_overflow_check_pattern_signed(signed int base, signed int offset) {
       if (base + offset < base) { /* ... */ } // The pattern of `a + b < a`, and other re-orderings, won't be instrumented
     }

     /// specified with ``-fsanitize-undefined-ignore-overflow-pattern=negated-unsigned-const``
     void negation_overflow() {
       unsigned long foo = -1UL; // No longer causes a negation overflow warning
       unsigned long bar = -2UL; // and so on...
     }

     /// specified with ``-fsanitize-undefined-ignore-overflow-pattern=unsigned-post-decr-while``
     void while_post_decrement() {
       unsigned char count = 16;
       while (count--) { /* ... */ } // No longer causes unsigned-integer-overflow sanitizer to trip
     }

  Many existing projects have a large amount of these code patterns present.
  This new flag should allow those projects to enable integer sanitizers with
  less noise.

Python Binding Changes
----------------------
- Fixed an issue that led to crashes when calling ``Type.get_exception_specification_kind``.

OpenMP Support
--------------
- Added support for 'omp assume' directive.

- Added support for the `[[omp::assume]]` attribute.
- AIX added an include directory for ``omp.h`` at ``/opt/IBM/openxlCSDK/include/openmp``.

Additional Information
======================

A wide variety of additional information is available on the `Clang web
page <https://clang.llvm.org/>`_. The web page contains versions of the
API documentation which are up-to-date with the Git version of
the source code. You can access versions of these documents specific to
this release by going into the "``clang/docs/``" directory in the Clang
tree.

If you have any questions or comments about Clang, please feel free to
contact us on the `Discourse forums (Clang Frontend category)
<https://discourse.llvm.org/c/clang/6>`_.
