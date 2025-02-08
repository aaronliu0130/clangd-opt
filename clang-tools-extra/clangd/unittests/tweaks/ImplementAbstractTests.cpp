//===-- ImplementAbstractTests.cpp ------------------------------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//

#include "TestTU.h"
#include "TweakTesting.h"
#include "gmock/gmock-matchers.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

using ::testing::Not;

namespace clang {
namespace clangd {
namespace {

bool stringEqIgnoreWs(StringRef LHS, StringRef RHS) {
  auto TrimmedL = LHS.trim();
  auto TrimmedR = RHS.trim();
  static constexpr llvm::StringLiteral WS(" \t\r\n\f\v");

  while (!TrimmedL.empty() && !TrimmedR.empty()) {
    auto LPos = TrimmedL.find_first_of(WS);
    auto RPos = TrimmedR.find_first_of(WS);
    if (TrimmedL.take_front(LPos) != TrimmedR.take_front(RPos))
      return false;
    TrimmedL =
        TrimmedL.substr(LPos).drop_while([](char C) { return WS.contains(C); });
    TrimmedR =
        TrimmedR.substr(RPos).drop_while([](char C) { return WS.contains(C); });
  }
  return TrimmedL == TrimmedR;
}

MATCHER_P(STREQWS, EqualTo, "") {
  if (stringEqIgnoreWs(arg, EqualTo))
    return true;

  auto Result =
      testing::internal::EqFailure("", "", arg, std::string(EqualTo), false);
  *result_listener << Result.message();
  return false;
}

TWEAK_TEST(ImplementAbstract);

TEST_F(ImplementAbstractTest, TestUnavailable) {

  StringRef Cases[]{
      // Not a pure virtual method.
      R"cpp(
        class A {
          virtual void Foo();
        };
        class ^B : public A {};
      )cpp",
      // Pure virtual method overridden in class.
      R"cpp(
        class A {
          virtual void Foo() = 0;
        };
        class ^B : public A {
          void Foo() override;
        };
      )cpp",
      // Pure virtual method overridden in class with virtual keyword
      R"cpp(
        class A {
          virtual void Foo() = 0;
        };
        class ^B : public A {
          virtual void Foo() override;
        };
      )cpp",
      // Pure virtual method overridden in class without override keyword
      R"cpp(
        class A {
          virtual void Foo() = 0;
        };
        class ^B : public A {
          void Foo();
        };
      )cpp",
      // Pure virtual method overriden in base class.
      R"cpp(
        class A {
          virtual void Foo() = 0;
        };
        class B : public A {
          void Foo() override;
        };
        class ^C : public B {};
      )cpp"};
  for (const auto &Case : Cases) {
  	EXPECT_UNAVAILABLE(Case);
  }
}

TEST_F(ImplementAbstractTest, NormalAvailable) {
  struct Case {
    llvm::StringRef TestHeader;
    llvm::StringRef TestSource;
    llvm::StringRef ExpectedSource;
  };

  Case Cases[]{
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;
            };)cpp",
          R"cpp(
            class B : public A {
            ^};
          )cpp",
          R"cpp(
            class B : public A {
              void Foo() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
            public:
              virtual int Foo() = 0;
            };)cpp",
          R"cpp(
            class ^B : public A {
            };
          )cpp",
          R"cpp(
            class B : public A {
            public:
              int Foo() override {
                return {};
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo(int Param) = 0;
            };)cpp",
          R"cpp(
            class ^B : public A {
            };
          )cpp",
          R"cpp(
            class B : public A {
              void Foo(int Param) override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo(int Param) = 0;
            };)cpp",
          R"cpp(
            struct ^B : public A {
            };
          )cpp",
          R"cpp(
            struct B : public A {
            private:
              void Foo(int Param) override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo(int Param) const volatile = 0;

            public:
              virtual void Bar(int Param) = 0;
            };)cpp",
          R"cpp(
            class ^B : public A {
              void Foo(int Param) const volatile override;
            };
          )cpp",
          R"cpp(
            class B : public A {
              void Foo(int Param) const volatile override;

            public:
              void Bar(int Param) override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;
              virtual void Bar() = 0;
            };
            class B : public A {
              void Foo() override {
              }
            };
          )cpp",
          R"cpp(
            class ^C : public B {
              virtual void Baz();
            };
          )cpp",
          R"cpp(
            class C : public B {
              virtual void Baz();
              void Bar() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;
            };)cpp",
          R"cpp(
            class ^B : public A {
              ~B();
            };
          )cpp",
          R"cpp(
            class B : public A {
              void Foo() override {
              }

              ~B();
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;

            public:
              virtual void Bar() = 0;
            };)cpp",
          R"cpp(
            class ^B : public A {
            };
          )cpp",
          R"cpp(
            class B : public A {
              void Foo() override {
              }

            public:
              void Bar() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;
            };
            struct B : public A {
              virtual void Bar() = 0;
            };)cpp",
          R"cpp(
            class ^C : public B {
            };
          )cpp",
          R"cpp(
            class C : public B {
              void Foo() override {
              }

            public:
              void Bar() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;
            };
            struct B : public A {
              virtual void Bar() = 0;
            };)cpp",
          R"cpp(
            class ^C : private B {
            };
          )cpp",
          R"cpp(
            class C : private B {
              void Foo() override {
              }
              void Bar() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            struct A {
              virtual void Foo() = 0;
            };
            struct B {
              virtual void Bar() = 0;
            };)cpp",
          R"cpp(
            class C : public ^A, B {
            };
          )cpp",
          R"cpp(
            class C : public A, B {
            public:
              void Foo() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            struct A {
              virtual void Foo() = 0;
            };
            struct B {
              virtual void Bar() = 0;
            };)cpp",
          R"cpp(
            class ^C : public A, B {
            };
          )cpp",
          R"cpp(
            class C : public A, B {
              void Bar() override {
              }

            public:
              void Foo() override {
              }
            };
          )cpp",
      },
  };

  for (const auto &Case : Cases) {
    Header = Case.TestHeader.str();
    EXPECT_THAT(apply(Case.TestSource), STREQWS(Case.ExpectedSource));
  }
}

TEST_F(ImplementAbstractTest, TemplateUnavailable) {
  StringRef Cases[]{
      R"cpp(
        template <typename T> class A { virtual void Foo() = 0; };
        template <typename T> class ^B : public A<T> {};
      )cpp",
      R"cpp(
        template <typename T> class ^B : public T{};
      )cpp",
  };
  for (const auto &Case : Cases) {
  	EXPECT_UNAVAILABLE(Case);
  }
}

TEST_F(ImplementAbstractTest, TemplateAvailable) {
  struct Case {
    llvm::StringRef TestHeader;
    llvm::StringRef TestSource;
    llvm::StringRef ExpectedSource;
  };
  Case Cases[]{
      {
          R"cpp(
            template <typename T> class A { virtual void Foo() = 0; };
          )cpp",
          R"cpp(
            class ^B : public A<int> {

            };
          )cpp",
          R"cpp(
            class B : public A<int> {
              void Foo() override {
              }
            };
          )cpp",
      },
      {
          R"cpp(
            class A {
              virtual void Foo() = 0;
            };)cpp",
          R"cpp(
            template <typename T> class ^B : public A {

            };
          )cpp",
          R"cpp(
            template <typename T> class B : public A {
              void Foo() override {
              }
            };
          )cpp",
      },
  };
  for (const auto &Case : Cases) {
    Header = Case.TestHeader.str();
    EXPECT_THAT(apply(Case.TestSource), STREQWS(Case.ExpectedSource));
  }
}

} // namespace
} // namespace clangd
} // namespace clang
