(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2019       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

Require Import Rbase.
Require Import Rtrigo1.
Require Import Rfunctions.

Require Import Lra.
Require Import Ranalysis_reg.

Local Open Scope R_scope.

(*********************************************************)
(** * Bounds of expressions with trigonometric functions *)
(*********************************************************)

Lemma sin2_bound : forall x,
  0 <= (sin x)² <= 1.
Proof.
  intros x.
  rewrite <- Rsqr_1.
  apply Rsqr_bounds_le.
  apply SIN_bound.
Qed.

Lemma cos2_bound : forall x,
  0 <= (cos x)² <= 1.
Proof.
  intros x.
  rewrite <- Rsqr_1.
  apply Rsqr_bounds_le.
  apply COS_bound.
Qed.

(*********************************************************)
(** * Express trigonometric functions with each other    *)
(*********************************************************)

(** ** Express sin and cos with each other *)

Lemma cos_sin : forall x, cos x >=0 ->
  cos x = sqrt(1 - (sin x)²).
Proof.
  intros x H.
  apply Rsqr_inj.
  - lra.
  - apply sqrt_pos.
  - rewrite Rsqr_sqrt.
    apply cos2.
    pose proof sin2_bound x.
    lra.
Qed.

Lemma cos_sin_opp : forall x, cos x <=0 ->
  cos x = - sqrt(1 - (sin x)²).
Proof.
  intros x H.
  rewrite <- (Ropp_involutive (cos x)).
  apply Ropp_eq_compat.
  apply Rsqr_inj.
  - lra.
  - apply sqrt_pos.
  - rewrite Rsqr_sqrt.
    rewrite <- Rsqr_neg.
    apply cos2.
    pose proof sin2_bound x.
    lra.
Qed.

Lemma cos_sin_Rabs : forall x,
  Rabs (cos x) = sqrt(1 - (sin x)²).
Proof.
  intros x.
  unfold Rabs.
  destruct (Rcase_abs (cos x)).
  - rewrite <- (Ropp_involutive (sqrt (1 - (sin x)²))).
    apply Ropp_eq_compat.
    apply cos_sin_opp; lra.
  - apply cos_sin; assumption.
Qed.

Lemma sin_cos : forall x, sin x >=0 ->
  sin x = sqrt(1 - (cos x)²).
Proof.
  intros x H.
  apply Rsqr_inj.
  - lra.
  - apply sqrt_pos.
  - rewrite Rsqr_sqrt.
    apply sin2.
    pose proof cos2_bound x.
    lra.
Qed.

Lemma sin_cos_opp : forall x, sin x <=0 ->
  sin x = - sqrt(1 - (cos x)²).
Proof.
  intros x H.
  rewrite <- (Ropp_involutive (sin x)).
  apply Ropp_eq_compat.
  apply Rsqr_inj.
  - lra.
  - apply sqrt_pos.
  - rewrite Rsqr_sqrt.
    rewrite <- Rsqr_neg.
    apply sin2.
    pose proof cos2_bound x.
    lra.
Qed.

Lemma sin_cos_Rabs : forall x,
  Rabs (sin x) = sqrt(1 - (cos x)²).
Proof.
  intros x.
  unfold Rabs.
  destruct (Rcase_abs (sin x)).
  - rewrite <- ( Ropp_involutive (sqrt (1 - (cos x)²))).
    apply Ropp_eq_compat.
    apply sin_cos_opp; lra.
  - apply sin_cos; assumption.
Qed.

(** ** Express tan with sin and cos *)

Lemma tan_sin : forall x, 0 <= cos x ->
  tan x = sin x / sqrt (1 - (sin x)²).
Proof.
  intros x H.
  unfold tan.
  rewrite <- (sqrt_Rsqr (cos x)) by assumption.
  rewrite <- (cos2 x).
  reflexivity.
Qed.

Lemma tan_sin_opp : forall x, 0 > cos x ->
  tan x = - (sin x / sqrt (1 - (sin x)²)).
Proof.
  intros x H.
  unfold tan.
  rewrite cos_sin_opp by lra.
  rewrite Ropp_div_den.
  reflexivity.
  pose proof cos_sin_opp x.
  lra.
Qed.

(** Note: tan_sin_Rabs wouldn't make a lot of sense, because one would need Rabs on both sides *)

Lemma tan_cos : forall x, 0 <= sin x ->
  tan x = sqrt (1 - (cos x)²) / cos x.
Proof.
  intros x H.
  unfold tan.
  rewrite <- (sqrt_Rsqr (sin x)) by assumption.
  rewrite <- (sin2 x).
  reflexivity.
Qed.

Lemma tan_cos_opp : forall x, 0 >= sin x ->
  tan x = - sqrt (1 - (cos x)²) / cos x.
Proof.
  intros x H.
  unfold tan.
  rewrite sin_cos_opp by lra.
  reflexivity.
Qed.

(** ** Express sin and cos with tan *)

Lemma sin_tan : forall x, 0 < cos x ->
  sin x = tan x / sqrt (1 + (tan x)²).
Proof.
  intros.
  assert(Hcosle:0<=cos x) by lra.
  pose proof tan_sin x Hcosle as Htan.
  rewrite Htan.
  repeat rewrite <- Rsqr_pow2 in *.
  assert (forall a b c:R, b<>0 -> c<> 0 -> a/b/c = a/(b*c)) as R_divdiv_divmul by (intros; field; lra).
  rewrite R_divdiv_divmul.
  rewrite <- sqrt_mult_alt.
  rewrite Rsqr_div, Rsqr_sqrt.
  field_simplify ((1 - (sin x)²) * (1 + (sin x)² / (1 - (sin x)²))).
  rewrite sqrt_1.
  field.
  all: pose proof (sin2 x); pose proof Rsqr_pos_lt (cos x); try lra.
  all: assert( forall a, 0 < a -> a <> 0) as Hne by (intros; lra).
  all: apply Hne, sqrt_lt_R0; try lra.
  rewrite <- Htan.
  pose proof Rle_0_sqr (tan x); lra.
Qed.

Lemma cos_tan : forall x, 0 < cos x ->
  cos x = 1 / sqrt (1 + (tan x)²).
Proof.
  intros.
  destruct (Rcase_abs (sin x)) as [Hsignsin|Hsignsin].
  - assert(Hsinle:0>=sin x) by lra.
    pose proof tan_cos_opp x Hsinle as Htan.
    rewrite Htan.
    rewrite Rsqr_div.
    rewrite <- Rsqr_neg.
    rewrite Rsqr_sqrt.
    field_simplify( 1 + (1 - (cos x)²) / (cos x)² ).
    rewrite sqrt_div_alt.
    rewrite sqrt_1.
    field_simplify_eq.
    rewrite sqrt_Rsqr.
    reflexivity.
    all: pose proof cos2_bound x.
    all: pose proof Rsqr_pos_lt (cos x) ltac:(lra).
    all: pose proof sqrt_lt_R0 (cos x)² ltac:(assumption).
    all: lra.
  - assert(Hsinge:0<=sin x) by lra.
    pose proof tan_cos x Hsinge as Htan.
    rewrite Htan.
    rewrite Rsqr_div.
    rewrite Rsqr_sqrt.
    field_simplify( 1 + (1 - (cos x)²) / (cos x)² ).
    rewrite sqrt_div_alt.
    rewrite sqrt_1.
    field_simplify_eq.
    rewrite sqrt_Rsqr.
    reflexivity.
    all: pose proof cos2_bound x.
    all: pose proof Rsqr_pos_lt (cos x) ltac:(lra).
    all: pose proof sqrt_lt_R0 (cos x)² ltac:(assumption).
    all: lra.
Qed.

(*********************************************************)
(** * Additional shift lemmas for sin, cos, tan          *)
(*********************************************************)

Lemma sin_pi_minus : forall x,
  sin (PI - x) = sin x.
Proof.
  intros x.
  rewrite sin_minus, cos_PI, sin_PI.
  ring.
Qed.

Lemma sin_pi_plus : forall x,
  sin (PI + x) = - sin x.
Proof.
  intros x.
  rewrite sin_plus, cos_PI, sin_PI.
  ring.
Qed.

Lemma cos_pi_minus : forall x,
  cos (PI - x) = - cos x.
Proof.
  intros x.
  rewrite cos_minus, cos_PI, sin_PI.
  ring.
Qed.

Lemma cos_pi_plus : forall x,
  cos (PI + x) = - cos x.
Proof.
  intros x.
  rewrite cos_plus, cos_PI, sin_PI.
  ring.
Qed.

Lemma tan_pi_minus : forall x, cos x <> 0 ->
  tan (PI - x) = - tan x.
Proof.
  intros x H.
  unfold tan; rewrite sin_pi_minus, cos_pi_minus.
  field; assumption.
Qed.

Lemma tan_pi_plus : forall x, cos x <> 0 ->
  tan (PI + x) = tan x.
Proof.
  intros x H.
  unfold tan; rewrite sin_pi_plus, cos_pi_plus.
  field; assumption.
Qed.
