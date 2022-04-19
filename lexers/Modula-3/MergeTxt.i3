
(* -----------------------------------------------------------------------1- *)
(* This file is part of the Schutz semantic editor.                          *)
(* Copyright 1988..2017, Rodney M. Bates.                                    *)
(* rodney.m.bates@acm.org                                                    *)
(* Licensed under the MIT License.                                           *)
(* -----------------------------------------------------------------------2- *)

INTERFACE MergeTxt 

(* This module merges a localized set of text edit changes into an Est. *) 

; IMPORT Thread 

; IMPORT EstHs 
; IMPORT LbeStd 
; IMPORT Marks 
; IMPORT Strings 

; FROM Assertions IMPORT AssertionFailure 

(* There could be up to four new TokMarks, denoting new beginnings of
   lines, built by MergeTextEdit. If the old line is actually a blank
   line mod, it could be split into two blank lines with a text
   insertion line between.  Furthermore, the insertion could have an
   imbedded new line, making it two text insertions.  If the line is
   not a blank line, it can become two if there is a new line in the
   insertion. *)

; CONST TokMarkCt = 4 
; TYPE TokMarkSsTyp = [ 0 .. TokMarkCt - 1 ] 
; TYPE TokMarkArrayTyp = ARRAY TokMarkSsTyp OF Marks . TokMarkTyp 
; TYPE EstRefArrayTyp = ARRAY TokMarkSsTyp OF LbeStd . EstRootTyp   

; PROCEDURE MergeTextEdit 
    ( Lang : LbeStd . LangTyp 
    ; EstRootRef : EstHs . EstRefTyp 
    ; StartTokMark : Marks . TokMarkTyp 
    ; EndTokMark : Marks . TokMarkTyp 
    ; BlankLineNo : LbeStd . LineNoTyp 
      (* Line number within a blank line mod or EndOfImage. *) 
    ; DelFromPos : LbeStd . LimitedCharNoTyp 
    ; DelToPos : LbeStd . LimitedCharNoTyp 
      (* ^Chars in [ DelFromPos .. DelToPos - 1 ] are deleted and replaced
          by those in InsText [ DelFromPos .. DelFromPos + InsLen - 1 ] *) 
    ; READONLY InsText : Strings . StringTyp  
      (* ^This is an image of the entire line, after the edits to be merged. *)
    ; InsLen : LbeStd . LimitedCharNoTyp 
    ; InsNlPos : LbeStd . LimitedCharNoTyp 
      (* ^Position in the edited line before which an inserted new line goes. 
          LbeStd . LimitedCharNoInfinity if no new line at all. *) 
    ; NlIndentPos : LbeStd . LimitedCharNoTyp 
      (* ^If InsNlPos # LbeStd . LimitedCharNoInfinity, this the amount 
          of indentation of the new text line, after the inserted Nl. 
          Ignored otherwise. *) 
    ; DelNlShift : LbeStd . LimitedCharNoTyp 
      (* ^When LbeStd . LimitedCharNoInfinity, no new line is deleted. 
          Otherwise, the new line at the end of the line denoted by 
          BolTokMark/BlankLineNo is deleted, and DelNlShift is the 
          amount to add to character positions on the formerly following 
          line to get it appended to the first line. 
          InsNlPos and DelNlShift cannot both be unequal to 
          LbeStd . LimitedCharNoInfinity. 
      *) 
    ; VAR NewEstRootRef : EstHs . EstRefTyp  
    ; VAR NodeNoChange : LbeStd . EstNodeNoTyp 
    ; VAR MaxTouchedNodeNo : LbeStd . EstNodeNoTyp 
    ; VAR NewBolTokMark : Marks . TokMarkTyp 
    ; VAR NewLinesCt : LbeStd . LineNoTyp 
    ; VAR LeadingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^This many lines from leading blank mods were incorporated into the 
         resulting set of tree children. *)  
    ; VAR TrailingBlankLinesIncluded : LbeStd . LineNoTyp  
      (* ^Similarly, for trailing mods. *) 
    ) 
  RAISES { AssertionFailure , Thread . Alerted } 

  (* MergeTextEdit puts a single location group of text modifications
     into an Est.  The string at ModTextStringRef will have no leading
     or trailing blanks.  ModTextStringRef will be NIL instead of
     pointing to an empty string. *)

; END MergeTxt 
. 
