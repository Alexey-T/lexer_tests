MODULE Str;

(* ------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * ------------------------------------------------------------------------ *)

IMPORT SYSTEM;

(** ------------------------------------------------------------------------
  * Basic string manipulations.
  * ----------------------------------------------------------------------- *)

TYPE
   CaseTable* = ARRAY ORD (MAX (CHAR)) + 1 OF CHAR;
   ListProcessingCallback* = PROCEDURE (VAR str: ARRAY OF CHAR; 
      beg, end: LONGINT; VAR tag: LONGINT): BOOLEAN;

VAR
   (* Case conversion tables, e.g. upperCase [ORD ('a')] = 'A', etc. The
    * tables are initialized to ANSI only, i.e. upperCase is equivalent of the 
    * CAP standard procedure. *)
   sameCase*, upperCase*, lowerCase*, invertCase*: CaseTable;

PROCEDURE Pos* (ch: CHAR; VAR str: ARRAY OF CHAR): LONGINT;
VAR
   res: LONGINT;
BEGIN
   res := 0;
   WHILE str [res] # ch DO
      INC (res);
   END;
   RETURN res
END Pos;

PROCEDURE Length* (VAR str: ARRAY OF CHAR): LONGINT;
BEGIN
   RETURN Pos (0X, str)
END Length;

PROCEDURE Concat* (VAR str, tail: ARRAY OF CHAR);
VAR
   i, c: LONGINT;
BEGIN
   i := Length (str);
   c := Length (tail);
   SYSTEM.MOVE (SYSTEM.ADR (tail [0]), SYSTEM.ADR (str [i]), c);
   str [i + c] := 0X;
END Concat;

PROCEDURE ConcatC* (VAR str: ARRAY OF CHAR; tail: ARRAY OF CHAR);
BEGIN
   Concat (str, tail);
END ConcatC;

PROCEDURE ChangeCase* (VAR str: ARRAY OF CHAR; VAR table: CaseTable);
(** Change case of all characters in the string upto 0X terminator according
  * to the 'table'. The table can be one of (sameCase, upperCase, lowerCase, 
  * invertCase) or a custom one. *)
VAR
   i: LONGINT;
BEGIN
   i := 0;
   WHILE str [i] # 0X DO
      str [i] := table [ORD (str [i])];
      INC (i);
   END;
END ChangeCase;

PROCEDURE ProcessList* (VAR list: ARRAY OF CHAR; separator: CHAR;
   callback: ListProcessingCallback; VAR tag: LONGINT): BOOLEAN;
(** Process the 'list' by calling 'callback' for every substring between the
  * 'separator' characters. Keep processing while 'callback' returns TRUE.
  * Result is either TRUE, or FALSE if the last 'callback' call returned
  * FALSE. I.e. TRUE means that the full list was processed, and FALSE means
  * premature interruption per the 'callback's request. If the callback is
  * used for searching for a substring, then 'FALSE' means 'found', while
  * 'TRUE' means 'not found', i.e. the result is inverted.
  * When the callback is called the following holds: beg <= end. If beg = end,
  * then an empty substring was found, like the second item in the following
  * list: "first item,,third item" (separator = ',').
  * The trailing separator not is reported as an empty item, i.e. the
  * following list contains only one empty item, not two: ",". An empty string
  * contains zero items. To put an empty item at the end of a list, add an
  * extra separaator.
  * The list must be terminated with 0X, i.e. you can't pass a single CHAR
  * variable for the list parameter, unless it's value is 0X. 
  * 'tag' is just a variable that is passed to the 'callback' unmodified. *)
VAR
   beg, end: LONGINT;
   res: BOOLEAN;
BEGIN
   res := TRUE;
   beg := 0;
   WHILE res & (list [beg] # 0X) DO
      end := beg;
      WHILE (list [end] # separator) & (list [end] # 0X) DO
         INC (end);
      END;
      res := callback (list, beg, end, tag);
      beg := end;
      IF list [beg] # 0X THEN
         INC (beg);
      END;
   END;
   RETURN res
END ProcessList;

PROCEDURE Init ();
VAR i, cap: INTEGER;
BEGIN
   (* same case *)
   i := ORD (MAX (CHAR));
   WHILE i # 0 DO
      sameCase [i] := CHR (i);
      DEC (i);
   END;
   sameCase [0] := CHR (0);
   (* upper, lower and invert cases *)
   upperCase := sameCase;
   lowerCase := sameCase;
   invertCase := sameCase;
   i := ORD ('a');
   WHILE i <= ORD ('z') DO
      cap := ORD (CAP (CHR (i)));
      upperCase [i] := CHR (cap);
      lowerCase [cap] := CHR (i);
      invertCase [i] := CHR (cap);
      invertCase [cap] := CHR (i);
      INC (i);
   END;
END Init;

BEGIN
   Init;
END Str.
