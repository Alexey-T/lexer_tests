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

fn id[T:! Type](x: T) -> T {
  // CHECK: COMPILATION ERROR: {{.*}}/explorer/testdata/generic_function/fail_not_addable.carbon:[[@LINE+3]]: type error in addition(1)
  // CHECK: expected: i32
  // CHECK: actual: T:! Type
  return x + 0;
}

fn Main() -> i32 {
  return id(0);
}
