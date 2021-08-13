Module test1.
Section test1.

Variable (A B C A' B' C' : Type).
Variable (f1 : A -> A') (f2 : B -> B') (f3 : C -> C').
Variable (g1 : A -> B) (g2 : A' -> B') (h1 : B -> C) (h2 : B' -> C').

Local Coercion g1 : A >-> B.
Local Coercion g2 : A' >-> B'.
Local Coercion h1 : B >-> C.
Local Coercion h2 : B' >-> C'.
Local Coercion f1 : A >-> A'.
Local Coercion f2 : B >-> B'.
Local Coercion f3 : C >-> C'.
(* [g1; h1; f3], [f1; g2; h2] : A >-> C' should not be reported as ambiguous  *)
(* paths because they are redundant with `[g1; f2], [f1; g2] : A >-> B'` and  *)
(* `[h1; f3], [f2; h2] : B >-> C'`.                                           *)

Print Graph.

End test1.
End test1.

Module test2.
Section test2.

Variable (A B C D : Type).
Variable (ab : A -> B) (bc : B -> C) (ac : A -> C) (cd : C -> D).

Local Coercion ac : A >-> C.
Local Coercion cd : C >-> D.
Local Coercion ab : A >-> B.
Local Coercion bc : B >-> C.
(* `[ab; bc; cd], [ac; cd] : A >-> D` should not be reported as ambiguous     *)
(* paths because they are redundant with `[ab; bc], [ac] : A >-> C`.          *)

Print Graph.

End test2.
End test2.

Module test3.
Section test3.

Variable (A B C : Type).
Variable (ab : A -> B) (ba : B -> A) (ac : A -> C) (bc : B -> C).

Local Coercion ac : A >-> C.
Local Coercion bc : B >-> C.
Local Coercion ab : A >-> B.
Local Coercion ba : B >-> A.
(* `[ba; ac], [bc] : B >-> C` should not be reported as ambiguous paths       *)
(* because they are redundant with `[ab; bc], [ac] : A >-> C` and             *)
(* `[ba; ab] : B >-> B`.                                                      *)

Print Graph.

End test3.
End test3.

Module test4.
Section test4.
Variable (A : Type) (P Q : A -> Prop).

Record B := {
  B_A : A;
  B_P : P B_A }.

Record C := {
  C_A : A;
  C_Q : Q C_A }.

Record D := {
  D_A : A;
  D_P : P D_A;
  D_Q : Q D_A }.

Local Coercion B_A : B >-> A.
Local Coercion C_A : C >-> A.
Local Coercion D_A : D >-> A.
Local Coercion D_B (d : D) : B := Build_B (D_A d) (D_P d).
Local Coercion D_C (d : D) : C := Build_C (D_A d) (D_Q d).

Print Graph.

End test4.
End test4.

Module test5.
Section test5.

Variable (A : Type) (P Q : A -> Prop).

Definition A' (x : bool) := A.

Record B (x : bool) := {
  B_A' : A' x;
  B_P : P B_A' }.

Record C (x : bool) := {
  C_A' : A' x;
  C_Q : Q C_A' }.

Record D := {
  D_A : A;
  D_P : P D_A;
  D_Q : Q D_A }.

Local Coercion A'_A (x : bool) (a : A' x) : A := a.
Local Coercion B_A' : B >-> A'.
Local Coercion C_A' : C >-> A'.
Local Coercion D_A : D >-> A.
Local Coercion D_B (d : D) : B false := Build_B false (D_A d) (D_P d).
Local Coercion D_C (d : D) : C true := Build_C true (D_A d) (D_Q d).

Print Graph.

End test5.
End test5.

Module test6.
Section test6.

Variable (A : Type) (P Q : A -> Prop).

Record A' (x : bool) := { A'_A : A }.

Record B (x : bool) := {
  B_A' : A' x;
  B_P : P (A'_A x B_A') }.

Record C (x : bool) := {
  C_A' : A' x;
  C_Q : Q (A'_A x C_A') }.

Record D := {
  D_A : A;
  D_P : P D_A;
  D_Q : Q D_A }.

Local Coercion A'_A : A' >-> A.
Local Coercion B_A' : B >-> A'.
Local Coercion C_A' : C >-> A'.
Local Coercion D_A : D >-> A.
Local Coercion D_B (d : D) : B false :=
  Build_B false (Build_A' false (D_A d)) (D_P d).
Local Coercion D_C (d : D) : C true :=
  Build_C true (Build_A' true (D_A d)) (D_Q d).

Print Graph.

End test6.
End test6.

Module test7.
Record > NAT := wrap_nat { unwrap_nat :> nat }.
Record > LIST (T : Type) := wrap_list { unwrap_list :> list T }.
Record > TYPE := wrap_Type { unwrap_Type :> Type }.
End test7.

Module test8.
Set Primitive Projections.
Record > NAT_prim := wrap_nat { unwrap_nat :> nat }.
Record > LIST_prim (T : Type) := wrap_list { unwrap_list :> list T }.
Record > TYPE_prim := wrap_Type { unwrap_Type :> Type }.
End test8.
