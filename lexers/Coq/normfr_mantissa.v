Require Import Uint63 ZArith Floats.

Definition half := ldexp one (-1)%Z.
Definition three_quarters := (half + (ldexp one (-2)%Z))%float.

Check (eq_refl : normfr_mantissa one = 0%uint63).
Check (eq_refl : normfr_mantissa half = (1 << 52)%uint63).
Check (eq_refl : normfr_mantissa (-half) = (1 << 52)%uint63).
Check (eq_refl : normfr_mantissa (-one) = 0%uint63).
Check (eq_refl : normfr_mantissa zero = 0%uint63).
Check (eq_refl : normfr_mantissa nan = 0%uint63).
Check (eq_refl : normfr_mantissa three_quarters = (3 << 51)%uint63).

Check (eq_refl 0%uint63 <: normfr_mantissa one = 0%uint63).
Check (eq_refl (1 << 52)%uint63 <: normfr_mantissa half = (1 << 52)%uint63).
Check (eq_refl (1 << 52)%uint63 <: normfr_mantissa (-half) = (1 << 52)%uint63).
Check (eq_refl 0%uint63 <: normfr_mantissa (-one) = 0%uint63).
Check (eq_refl 0%uint63 <: normfr_mantissa zero = 0%uint63).
Check (eq_refl 0%uint63 <: normfr_mantissa nan = 0%uint63).
Check (eq_refl (3 << 51)%uint63 <: normfr_mantissa three_quarters = (3 << 51)%uint63).

Check (eq_refl 0%uint63 <<: normfr_mantissa one = 0%uint63).
Check (eq_refl (1 << 52)%uint63 <<: normfr_mantissa half = (1 << 52)%uint63).
Check (eq_refl (1 << 52)%uint63 <<: normfr_mantissa (-half) = (1 << 52)%uint63).
Check (eq_refl 0%uint63 <<: normfr_mantissa (-one) = 0%uint63).
Check (eq_refl 0%uint63 <<: normfr_mantissa zero = 0%uint63).
Check (eq_refl 0%uint63 <<: normfr_mantissa nan = 0%uint63).
Check (eq_refl (3 << 51)%uint63 <<: normfr_mantissa three_quarters = (3 << 51)%uint63).
