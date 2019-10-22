MODULE StrU;

(* ------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * ------------------------------------------------------------------------ *)

IMPORT SYSTEM, Win:=Windows, Str;

(** ------------------------------------------------------------------------
  * Basic Unicode string manipulations.
  * ----------------------------------------------------------------------- *)

TYPE
   Char* = SYSTEM.CARD16;

PROCEDURE Pos* (ch: Char; VAR str: ARRAY OF Char): LONGINT;
VAR
   res: LONGINT;
BEGIN
   res := 0;
   WHILE str [res] # ch DO
      INC (res);
   END;
   RETURN res
END Pos;

PROCEDURE Length* (VAR str: ARRAY OF Char): LONGINT;
BEGIN
   RETURN Pos (0, str)
END Length;

PROCEDURE CopyTo* (VAR src: ARRAY OF CHAR; VAR dst: ARRAY OF Char; beg, end, to: INTEGER);
(* Same as CopyTo, but src is copied to the Unicode dst with conversion. *)
VAR res: LONGINT;
BEGIN
   res := to;
   IF beg < end THEN
      res := Win.MultiByteToWideChar (Win.CP_ACP, Win.MULTIBYTE_SET {},
         src [beg], end - beg, dst [to], LEN (dst) - to - 1);
      ASSERT (res # 0, 60);
      INC (res, to);
   END;
   dst [res] := 0
END CopyTo;

PROCEDURE Append* (VAR str: ARRAY OF Char; end: ARRAY OF CHAR);
(* Append end to str, both strings and the result are null-terminated. End is
 * converted to Unicode on the fly. *)
VAR i, c, max: LONGINT;
BEGIN
   i := Length (str);
   c := Str.Length (end);
   max := LEN (str) - i - 1;
   IF c > max THEN
      c := max
   END;
   CopyTo (end, str, 0, SHORT (c), SHORT (i));
   str [i + c] := 0
END Append;

PROCEDURE Assign* (value: ARRAY OF CHAR; VAR str: ARRAY OF Char);
(* Assign the contents of 'value' to 'str'. *)
BEGIN
   str [0] := 0;
   Append (str, value)
END Assign;

END StrU.
