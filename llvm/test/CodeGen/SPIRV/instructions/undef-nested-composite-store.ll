; RUN: llc -verify-machineinstrs -O0 -mtriple=spirv32-unknown-unknown %s -o - | FileCheck %s

; CHECK-DAG: %[[#I32:]] = OpTypeInt 32
; CHECK-DAG: %[[#I16:]] = OpTypeInt 16
; CHECK-DAG: %[[#STRUCT:]] = OpTypeStruct %[[#I32]] %[[#I16]]
; CHECK-DAG: %[[#NESTED_STRUCT:]] = OpTypeStruct %[[#STRUCT]] %[[#I16]]
; CHECK-DAG: %[[#UNDEF:]] = OpUndef %[[#NESTED_STRUCT]]

; CHECK: %[[#]] = OpFunction %[[#]] None %[[#]]
; CHECK: %[[#]] = OpLabel
; CHECK: OpStore %[[#]] %[[#UNDEF]] Aligned 4
; CHECK: OpReturn
; CHECK: OpFunctionEnd

%struct = type {
  i32,
  i16
}

%nested_struct = type {
  %struct,
  i16
}

define void @foo(ptr %ptr) {
  store %nested_struct undef, ptr %ptr
  ret void
}
