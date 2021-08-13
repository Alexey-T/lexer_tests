Require Import Sint63.

Set Implicit Arguments.

Open Scope sint63_scope.

Check (eq_refl : 6 / 3 = 2).
Check (eq_refl 2 <: 6 / 3 = 2).
Check (eq_refl 2 <<: 6 / 3 = 2).
Definition compute1 := Eval compute in 6 / 3.
Check (eq_refl compute1 : 2 = 2).

Check (eq_refl : -6 / 3 = -2).
Check (eq_refl (-2) <: -6 / 3 = -2).
Check (eq_refl (-2) <<: -6 / 3 = -2).
Definition compute2 := Eval compute in -6 / 3.
Check (eq_refl compute2 : -2 = -2).

Check (eq_refl : 6 / -3 = -2).
Check (eq_refl (-2) <: 6 / -3 = -2).
Check (eq_refl (-2) <<: 6 / -3 = -2).
Definition compute3 := Eval compute in 6 / -3.
Check (eq_refl compute3 : -2 = -2).

Check (eq_refl : -6 / -3 = 2).
Check (eq_refl 2 <: -6 / -3 = 2).
Check (eq_refl 2 <<: -6 / -3 = 2).
Definition compute4 := Eval compute in -6 / -3.
Check (eq_refl compute4 : 2 = 2).

Check (eq_refl : 3 / 2 = 1).
Check (eq_refl 1 <: 3 / 2 = 1).
Check (eq_refl 1 <<: 3 / 2 = 1).
Definition compute5 := Eval compute in 3 / 2.
Check (eq_refl compute5 : 1 = 1).

Check (eq_refl : -3 / 2 = -1).
Check (eq_refl (-1) <: -3 / 2 = -1).
Check (eq_refl (-1) <<: -3 / 2 = -1).
Definition compute6 := Eval compute in -3 / 2.
Check (eq_refl compute6 : -1 = -1).

Check (eq_refl : 3 / -2 = -1).
Check (eq_refl (-1) <: 3 / -2 = -1).
Check (eq_refl (-1) <<: 3 / -2 = -1).
Definition compute7 := Eval compute in 3 / -2.
Check (eq_refl compute7 : -1 = -1).

Check (eq_refl : -3 / -2 = 1).
Check (eq_refl 1 <: -3 / -2 = 1).
Check (eq_refl 1 <<: -3 / -2 = 1).
Definition compute8 := Eval compute in -3 / -2.
Check (eq_refl compute8 : 1 = 1).

Check (eq_refl : -4611686018427387904 / -1 = -4611686018427387904).
Check (eq_refl (-4611686018427387904) <:
  -4611686018427387904 / -1 = -4611686018427387904).
Check (eq_refl (-4611686018427387904) <<:
  -4611686018427387904 / -1 = -4611686018427387904).
Definition compute9 := Eval compute in -4611686018427387904 / -1.
Check (eq_refl compute9 : -4611686018427387904 = -4611686018427387904).
