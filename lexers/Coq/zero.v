Require Import ZArith Uint63 Floats.

Open Scope float_scope.

Fail Check (eq_refl : zero = neg_zero).
Fail Check (eq_refl <: zero = neg_zero).
Fail Check (eq_refl <<: zero = neg_zero).
