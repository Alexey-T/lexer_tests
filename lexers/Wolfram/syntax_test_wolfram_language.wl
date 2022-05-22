(* SYNTAX TEST "WolframLanguage.sublime-syntax" *)

(*
  For information on how this file is used, see
  https://www.sublimetext.com/docs/3/syntax.html#testing
  Run tests by pressing `ctrl+shift+b` (or `cmd+b` on macOS), i.e. run the `build` command
*)

(* NUMBERS *)

   11
(* ^^ constant.numeric *)
   .11
(* ^^^ constant.numeric *)
   11.
(* ^^^ constant.numeric *)
   11.11
(* ^^^^^ constant.numeric *)
   11.11`
(* ^^^^^^ constant.numeric *)
   11.11`11.11
(* ^^^^^^^^^^^ constant.numeric *)
   11.11``123
(* ^^^^^^^^^^ constant.numeric *)
   11.11``
(* ^^^^^^^ invalid.illegal *)
   11.11*^23
(* ^^^^^^^^^ constant.numeric *)
   11.11*^
(* ^^^^^^^ invalid.illegal *)
   123..
(* ^^^ constant.numeric *)
(*    ^^ keyword.operator *)
   123...
(* ^^^ constant.numeric *)
(*    ^^^ keyword.operator *)


(* LANGUAGE CONSTANTS *)

   Catalan
(* ^ constant.language.wolfram *)
   Pi
(* ^ constant.language.wolfram *)

   True
(* ^^^^ constant.language *)
   Left
(* ^^^^ constant.language *)

(* OPERATORS *)

  +
(*^ keyword.operator.arithmetic*)
  -
(*^ keyword.operator.arithmetic*)
  /
(*^ keyword.operator.arithmetic*)
  *
(*^ keyword.operator.arithmetic*)


(* ! could be Not or Factorial, so do not mark as keyword.operator.logical *)
  !
(*^ keyword.operator*)


  &&
(*^^ keyword.operator.logical*)
  ||
(*^^ keyword.operator.logical*)

  >
(*^ keyword.operator*)
  <
(*^ keyword.operator*)
  ==
(*^^ keyword.operator*)
  >=
(*^^ keyword.operator*)
  <=
(*^^ keyword.operator*)
  ===
(*^^^ keyword.operator*)
  =!=
(*^^^ keyword.operator*)

   @
(* ^ keyword.operator *)
   @@
(* ^^ keyword.operator *)
   @@@
(* ^^^ keyword.operator *)
   @*
(* ^^ keyword.operator *)
   /*
(* ^^ keyword.operator *)
   /@
(* ^^ keyword.operator *)
   /;
(* ^^ keyword.operator *)
   //
(* ^^ keyword.operator *)
   /:
(* ^^ keyword.operator *)
   =
(* ^ keyword.operator *)
   :=
(* ^^ keyword.operator *)
   :>
(* ^^ keyword.operator *)
   ->
(* ^^ keyword.operator *)
   <->
(* ^^^ keyword.operator *)

  f'[x]
(* ^ keyword.operator.Derivative *)


(* STRINGIFYING OPERATORS *)

  >>>
(*^^^ invalid.illegal *)

  >>
(*^^ invalid.illegal *)

  <<
(*^^ invalid.illegal *)


  abc >>> def
(*^^^ symbol.unrecognized.wolfram*)
(*    ^^^ keyword.operator.PutAppend.wolfram *)
(*        ^^^ string.unquoted.wolfram *)

  abc >>> "def"
(*^^^ symbol.unrecognized.wolfram*)
(*    ^^^ keyword.operator.PutAppend.wolfram *)
(*        ^^^^^ string.quoted.double.wolfram *)

  abc >>> "/a/b/c/d"
(*^^^ symbol.unrecognized.wolfram*)
(*    ^^^ keyword.operator.PutAppend.wolfram *)
(*        ^^^^^^^^^^ string.quoted.double.wolfram *)

  abc >> def
(*^^^ symbol.unrecognized.wolfram*)
(*    ^^ keyword.operator.Put.wolfram *)
(*       ^^^ string.unquoted.wolfram *)

  abc >> "def"
(*^^^ symbol.unrecognized.wolfram*)
(*    ^^ keyword.operator.Put.wolfram *)
(*       ^^^^^ string.quoted.double.wolfram *)

  << qwerty
(*^^ keyword.operator.Get.wolfram *)
(*   ^^^^^^ string.unquoted.wolfram *)

  << "qwerty"
(*^^ keyword.operator.Get.wolfram *)
(*   ^^^^^^^^ string.quoted.double.wolfram *)

  abc << def
(*^^^ symbol.unrecognized.wolfram*)
(*    ^^ keyword.operator.Get.wolfram *)
(*       ^^^ string.unquoted.wolfram *)

  ::
(*^^ invalid.illegal *)

  symbol::tag
(*^^^^^^ symbol.unrecognized.wolfram *)
(*      ^^ keyword.operator.MessageName.wolfram *)
(*        ^^^ string.unquoted.wolfram *)

  symbol::"tag"
(*^^^^^^ symbol.unrecognized.wolfram *)
(*      ^^ keyword.operator.MessageName.wolfram *)
(*        ^^^^^ string.quoted.double.wolfram *)


(* VARIABLES *)

  f[x]
(*^ variable.function.wolfram *)
  f@x
(*^ variable.function.wolfram *)
  f@@x
(*^ symbol.unrecognized.wolfram *)
  foo$bar12
(*^^^^^^^^^ symbol.unrecognized.wolfram *)
  $foo
(*^^^^ symbol.unrecognized.wolfram *)
  my`context12`$foo
(*^^^^^^^^^^^^^^^^^ symbol.unrecognized.wolfram *)
  1$12foo
(*^ constant.numeric.wolfram *)
(* ^^^^^^ symbol.unrecognized.wolfram *)

  System`foo
(*^^^^^^^^^^ invalid.illegal.system.wolfram *)
  URLFetch
(*^^^^^^^^ invalid.deprecated.wolfram *)
  DiskBox
(*^^^^^^^ support.function.undocumented.wolfram *)
  ActiveClassification
(*^^^^^^^^^^^^^^^^^^^^ support.function.experimental.wolfram *)

  Plus
(*^^^^ support.function *)
  System`Plus
(*^^^^^^^^^^^ support.function *)

  Image[Red, Interleaving -> True]
(*^^^^^ support.function.builtin.wolfram *)
(*      ^^^ constant.language.wolfram *)
(*           ^^^^^^^^^^^^ constant.language.wolfram *)
(*                        ^^ keyword.operator *)

  `foo
(*^^^^ symbol.unrecognized.wolfram *)

  `$desktopCacheBase
(*^^^^^^^^^^^^^^^^^^ symbol.unrecognized.wolfram *)

  \[FormalX]
(*^^^^^^^^^^ constant.language.wolfram *)

  System`\[FormalX]
(*^^^^^^^^^^^^^^^^^ constant.language.wolfram *)






(* STRINGS *)

  "This is a `string` (* this is not a comment*)"
(* ^ string.quoted *)
(*                       ^ string.quoted *)
(*                                    ^ string.quoted.double *)

  foo::bar = "message"
(*   ^^ keyword.operator.MessageName *)
(*     ^^^ string.unquoted*)
(*             ^ string.quoted *)

  "this`is`a`context"
(*^ punctuation.definition.string.begin *)
(* ^^^^^^^^^^^^^^^^^ string.quoted.double.wolfram *)


  "\[Alpha]"
(*^ punctuation.definition.string.begin *)
(* ^^^^^^^^ donothighlight.constant.character.escape *)
(* ^^^^^^^^^ string.quoted.double.wolfram *)






(* COMMENTS *)

(* simple comment *)
(* ^ comment.block *)

(* comment (*in a comment*) *)
(* ^^^^^^^^ comment.block.wolfram *)
(*         ^^^^^^^^^^^^^^^^ comment.block.wolfram comment.block.wolfram *)


(* BRACKETS *)

  <|   |>  foo
(*^^ meta.associations.wolfram punctuation.section.associations.begin.wolfram  *)
(*   ^ meta.associations.wolfram  *)
(*     ^^ meta.associations.wolfram punctuation.section.associations.end.wolfram *)
(*         ^^^ source.wolfram symbol.unrecognized.wolfram *)

  [ ]
(*^ meta.brackets.wolfram punctuation.section.brackets.begin.wolfram *)
(* ^ meta.brackets.wolfram *)
(*  ^ meta.brackets.wolfram punctuation.section.brackets.end.wolfram *)

  { }
(*^ meta.braces.wolfram punctuation.section.braces.begin.wolfram *)
(* ^ meta.braces.wolfram *)
(*  ^ meta.braces.wolfram punctuation.section.braces.end.wolfram *)

  ( )
(*^ meta.parens.wolfram punctuation.section.parens.begin.wolfram *)
(* ^ meta.parens.wolfram *)
(*  ^ meta.parens.wolfram punctuation.section.parens.end.wolfram *)

  [ [ ]]
(*^^^ invalid.whitespace.Part.wolfram *)
(*    ^^ invalid.illegal.stray-parts-end.wolfram *)

  [[ ]]
(*^^ meta.parts.wolfram punctuation.section.parts.begin.wolfram *)
(*  ^ meta.parts.wolfram *)
(*   ^^ meta.parts.wolfram punctuation.section.parts.end.wolfram *)




(* Out[] syntax*)


  %
(*^ keyword.other.Out.wolfram *)

  %%
(*^^ keyword.other.Out.wolfram *)

  %%%
(*^^^ keyword.other.Out.wolfram *)

  %%%%
(*^^^^keyword.other.Out.wolfram *)

  %1
(*^^ keyword.other.Out.wolfram *)

  %2
(*^^ keyword.other.Out.wolfram *)

  %123
(*^^^^ keyword.other.Out.wolfram *)

  %9999
(*^^^^^ keyword.other.Out.wolfram *)





(* Slot[] and SlotSequence[] syntax *)

  #
(*^ keyword.other.Slot.wolfram *)

  ##
(*^^ keyword.other.SlotSequence.wolfram *)

  #1
(*^^ keyword.other.Slot.wolfram *)

  ##1
(*^^^ keyword.other.SlotSequence.wolfram *)

  #123
(*^^^^ keyword.other.Slot.wolfram *)

  ##123
(*^^^^^ keyword.other.SlotSequence.wolfram *)

  #abc
(*^^^^ keyword.other.Slot.wolfram *)

  #Sin
(*^^^^ keyword.other.Slot.wolfram *)

  #abc123
(*^^^^^^^ keyword.other.Slot.wolfram *)






(* Function Definitions *)

  foo[] := xxx
(*^^^ entity.name.function.wolfram *)

  foo[g[], h[]] := xxx
(*^^^ entity.name.function.wolfram *)

  foo[1, 2, a__, b__, c__, g[h[j[]]]] := xxx
(*^^^ entity.name.function.wolfram *)

  foo`bar[g[], h[]] := xxx
(*^^^ entity.name.Context.wolfram *)
(*    ^^^ entity.name.function.wolfram *)



  Attributes[foo] = { HoldFirst }
(*^^^^^^^^^^ support.function.builtin.wolfram *)
(*          ^ punctuation.section.brackets.begin.wolfram *)
(*           ^^^ entity.name.function.wolfram *)
(*              ^ punctuation.section.brackets.end.wolfram *)
(*                ^ keyword.operator.assignment.wolfram *)







(* Constant definitions *)

  a = 1
(*^ entity.name.constant.wolfram *)

Module[{}, a = 1]
(*         ^ symbol.unrecognized.wolfram *)

  $constant := 3 + 4
(*^^^^^^^^^ entity.name.constant.wolfram *)

  a`b = 1
(*^ entity.name.Context.wolfram *)
(*  ^ entity.name.constant.wolfram *)




(*
Regression Tests
*)

   NotAfter["\[\["]
(* ^^^^^^^^ variable.function.wolfram *)
(*         ^ punctuation.section.brackets.begin.wolfram *)
(*          ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*           ^^ invalid.illegal *)
(*             ^^ invalid.illegal *)
(*               ^ string.quoted.double.wolfram punctuation.definition.string.end *)
(*                ^ punctuation.section.brackets.end.wolfram *)

   "\[Alpa]"
(* ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*  ^^^^^^^ invalid.illegal.BadLongName *)
(*         ^ string.quoted.double.wolfram punctuation.definition.string.end *)

   "\[Alpa"
(* ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*  ^^^^^^ invalid.illegal.BadLongName *)
(*        ^ string.quoted.double.wolfram punctuation.definition.string.end *)

   "\Alpa]"
(* ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*  ^^^^^^ invalid.illegal.BadLongName *)
(*        ^ string.quoted.double.wolfram punctuation.definition.string.end *)

   "\Alpha]"
(* ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*  ^^^^^^^ invalid.illegal.BadLongName *)
(*         ^ string.quoted.double.wolfram punctuation.definition.string.end *)

   "\["
(* ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*  ^^ invalid.illegal.BadLongName *)
(*    ^ string.quoted.double.wolfram punctuation.definition.string.end *)

   "\[]"
(* ^ string.quoted.double.wolfram punctuation.definition.string.begin *)
(*  ^^^ invalid.illegal.BadLongName *)
(*     ^ string.quoted.double.wolfram punctuation.definition.string.end *)








