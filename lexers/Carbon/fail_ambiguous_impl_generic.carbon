// Part of the Carbon Language project, under the Apache License v2.0 with LLVM
// Exceptions. See /LICENSE for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
// RUN: %{not} %{explorer} %s 2>&1 | \
// RUN:   %{FileCheck} --match-full-lines --allow-unused-prefixes=false %s
// RUN: %{not} %{explorer} --parser_debug --trace_file=- %s 2>&1 | \
// RUN:   %{FileCheck} --match-full-lines --allow-unused-prefixes %s
// AUTOUPDATE: %{explorer} %s

package ExplorerTest api;

class A(T:! Type) {}
interface B(T:! Type) {}

external impl forall [T:! Type] A(T) as B(i32) {}
external impl forall [T:! Type] A(i32) as B(T) {}

fn F[T:! B(i32)](x: T) {}
fn G[T:! B(Bool)](x: T) {}

fn Main() -> i32 {
  let a: A(Bool) = {};
  let b: A(i32) = {};
  F(a);
  G(b);
  // CHECK: COMPILATION ERROR: {{.*}}/explorer/testdata/impl/fail_ambiguous_impl_generic.carbon:[[@LINE+1]]: ambiguous implementations of interface B(T = i32) for class A(T = i32)
  F(b);
  return 0;
}
