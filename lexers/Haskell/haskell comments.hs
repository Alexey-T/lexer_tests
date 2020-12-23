
-- COMMENTS STARTING WITH SPECIAL SYMBOL CHARS

   --
-- ^^^ comment
   --_
-- ^^^ comment
   --"
-- ^^^ comment
   --'
-- ^^^ comment
   --(
-- ^^^ comment
   --)
-- ^^^ comment
   --,
-- ^^^ comment
   ---
-- ^^^ comment
   --;
-- ^^^ comment
   --[
-- ^^^ comment
   --]
-- ^^^ comment
   --`
-- ^^^ comment
   --{
-- ^^^ comment
   --}
-- ^^^ comment


-- NO COMMENTS

   --!
-- ^^^ - comment
   --#
-- ^^^ - comment
   --$
-- ^^^ - comment
   --%
-- ^^^ - comment
   --&
-- ^^^ - comment
   --*
-- ^^^ - comment
   --+
-- ^^^ - comment
   --.
-- ^^^ - comment
   --.
-- ^^^ - comment
   --/
-- ^^^ - comment
   --:
-- ^^^ - comment
   --<
-- ^^^ - comment
   --=
-- ^^^ - comment
   -->
-- ^^^ - comment
   --?
-- ^^^ - comment
   --\
-- ^^^ - comment
   --\
-- ^^^ - comment
   --^
-- ^^^ - comment
   --|
-- ^^^ - comment
   --~
-- ^^^ - comment
   --~
-- ^^^ - comment


--DECLARATIONS

   module Name where
@@ -138,6 +217,21 @@
--       ^^^ variable.function.infix.haskell
--             ^ meta.number.integer.decimal.haskell constant.numeric.value.haskell

   a a = (--) a 2
--     ^ keyword.operator.haskell
--       ^^^^ - variable.function.infix
--       ^ punctuation.section.group.begin.haskell
--        ^^^^^^^^ comment.line.double-dash.haskell
         )
--       ^ punctuation.section.group.end.haskell

   a a = (---) a 2
--     ^ keyword.operator.haskell
--       ^^^^^ - variable.function.infix
--       ^ punctuation.section.group.begin.haskell
--        ^^^^^^^^^ comment.line.double-dash.haskell
         )
--       ^ punctuation.section.group.end.haskell

