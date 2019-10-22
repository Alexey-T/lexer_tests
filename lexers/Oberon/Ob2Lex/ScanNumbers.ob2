MODULE ScanNumbers;

(* ---------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * --------------------------------------------------------------------------- *)

IMPORT
   Scanner, ScanStretches, SC:=ScannerConsts;

<* NEW ErrorOnNumberPlusIdent+ *>
(* With ErrorOnNumberPlusIdent enabled this module detects when a number is
 * immediately followed by an identifier, and reports the ident as 'error'.
 * Revert to the regular version of the code when this error is moved to a
 * higher level of the scanner/parser. *)

CONST
   (* Additional tokens recognized by this scanner: *)
   hexNumber*      = ScanStretches.lastToken - 1; (* hexadecimal number *)
   hexChar*        = ScanStretches.lastToken - 2; (* hexadecimal CHAR *)
   realNumber*     = ScanStretches.lastToken - 3; (* REAL number *)
   longRealNumber* = ScanStretches.lastToken - 4; (* LONGREAL number *)
   error*          = ScanStretches.lastToken - 5; (* Error while parsing a number *)
   lastToken*      = error; (* Allocate more tokens below this value *)
<* IF ErrorOnNumberPlusIdent THEN *>
   firstToken      = hexNumber;
<* END *>

TYPE
   ScannerDesc* = RECORD
      scanner: ScanStretches.ScannerDesc;
      pos-, currToken, currTokenPos, nextToken, nextTokenPos: LONGINT;
      currIdent, nextIdent: Scanner.Ident;
      currIdentLen, nextIdentLen: LONGINT;
      done-: BOOLEAN; (* if TRUE after Scan, then read token, tokenPos *)
      token-, tokenPos-: LONGINT; (* recognized token when 'done' = TRUE *)
      ident-: Scanner.Ident;
      identLen-: LONGINT;
      numberState: INTEGER; (* Status of parsing a number constant. *)
      realChar: CHAR; (* When parsing a REAL number this is set to either "E" or "D" *)
   END;

PROCEDURE (VAR s: ScannerDesc) Init*;
BEGIN
   s.pos := 0;
   s.done := FALSE;
   s.scanner.Init;
   s.token := Scanner.none;
   s.tokenPos := 0;
   s.currToken := Scanner.none;
   s.currTokenPos := 0;
   s.nextToken := Scanner.none;
   s.nextTokenPos := 0;
   s.ident [0] := 0X;
   s.identLen := 0;
   s.currIdent [0] := 0X;
   s.currIdentLen := 0;
   s.nextIdent [0] := 0X;
   s.nextIdentLen := 0;
   s.numberState := 1;
END Init;

PROCEDURE HandleNextToken (VAR s: ScannerDesc);

   PROCEDURE OutputCurrToken ();
   VAR id: BOOLEAN;
   BEGIN
      s.done := TRUE;
<* IF ErrorOnNumberPlusIdent THEN *>
      id := s.currToken = Scanner.ident;
      IF id OR (SC.FirstPredefinedIdentToken >= s.currToken) & (s.currToken >= SC.lastToken)
      THEN
         IF (s.token = Scanner.number) 
            OR (firstToken >= s.token) & (s.token > error)
         THEN
            s.token := error;
         ELSE
            s.token := s.currToken;
            IF id THEN
               COPY (s.currIdent, s.ident);
               s.identLen := s.currIdentLen;
            END;
         END;
      ELSE
         s.token := s.currToken;
      END;
      s.tokenPos := s.currTokenPos;
<* ELSE *>
      s.token := s.currToken;
      s.tokenPos := s.currTokenPos;
      IF s.token = Scanner.ident THEN
         COPY (s.currIdent, s.ident);
         s.identLen := s.currIdentLen;
      END;
<* END *>
   END OutputCurrToken;

   PROCEDURE CopyNextTokenToCurr;
   BEGIN
      s.currToken := s.nextToken;
      s.currTokenPos := s.nextTokenPos;
      IF s.currToken = Scanner.ident THEN
         COPY (s.nextIdent, s.currIdent);
         s.currIdentLen := s.nextIdentLen;
      END;
   END CopyNextTokenToCurr;

   PROCEDURE PushCurrToken;
   BEGIN
      OutputCurrToken;
      CopyNextTokenToCurr;
   END PushCurrToken;

   PROCEDURE ScanNumber ();

      PROCEDURE Ident2ToToken (): INTEGER;
      (* hexDigit{hexDigit} ("X"|"H")
       * Result can be: hexChar, hexNum, error *)
      VAR
         i, c: LONGINT;
         res: INTEGER;
      BEGIN
         i := 0;
         c := LEN (s.scanner.ident) - 1;
         WHILE (i < c)
            & (('A' <= s.scanner.ident [i]) & (s.scanner.ident [i] <= 'F')
               OR ('0' <= s.scanner.ident [i]) & (s.scanner.ident [i] <= '9'))
         DO
            INC (i);
         END;
         IF (s.scanner.ident [i] = 'X') & (s.scanner.ident [i + 1] = 0X) THEN
            res := hexChar;
         ELSIF (s.scanner.ident [i] = 'H') & (s.scanner.ident [i + 1] = 0X) THEN
            res := hexNumber;
         ELSE
            res := error;
         END;
         RETURN res
      END Ident2ToToken;

      PROCEDURE IsValidIdent5 (): BOOLEAN;
      (* ("E"|"D") [digit{digit}] *)
      VAR
         i, c: LONGINT;
         res: BOOLEAN;
      BEGIN
         s.realChar := s.scanner.ident [0];
         res := (s.realChar = 'E') OR (s.realChar = 'D');
         IF res THEN
            i := 1;
            c := LEN (s.scanner.ident) - 1;
            WHILE (i < c) & ('0' <= s.scanner.ident [i]) & (s.scanner.ident [i] <= '9') DO
               INC (i);
            END;
            res := s.scanner.ident [i] = 0X;
         END;
         RETURN res
      END IsValidIdent5;

      PROCEDURE RealNumFinished ();
      BEGIN
         IF s.realChar = 'E' THEN
            s.currToken := realNumber;
         ELSE
            ASSERT (s.realChar = 'D', 21);
            s.currToken := longRealNumber;
         END;
         s.numberState := 1;
      END RealNumFinished;

      PROCEDURE Error ();
      BEGIN
         s.currToken := error;
         s.numberState := 1;
         PushCurrToken;
      END Error;

   BEGIN
(* We are parsing a number or a CHAR constant:
charConst   = digit{hexDigit}"X"
integer     = digit{digit} | digit{hexDigit}"H".
real        = digit{digit}"."[{digit} [ScaleFactor]].

ScaleFactor = ("E" | "D") ["+" | "-"] digit {digit}.
hexDigit    = digit|"A"|"B"|"C"|"D"|"E"|"F".
digit       = "0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9".
All productions start with a digit, so numberState is triggered by the
Scanner.number token. In terms on Scanner tokens:
charConst = number ident.
integer   = number [ident].
real      = number period [number [ident [plus | minus] number]]
So, overall sequence is:
             number ([ident] | period [number] [ident | ident [plus | minus] number])
numberState =   1       2         3      4        5a      5      6      6       7
numberState flow diagram:
->1>-------------\
  |              |
  |--->2>--------|
  |              |
  3>-->4>--------|
  |    |         |
  \--->?>->5a>---|
       |         |
       5>-----\  |
       |      |  |
       \->6>->7>-|
               (END)

The digits are the 'numberState' variable values, which it assumes when the
scanner encounters each of the tokens. When a non-matching token is met (e.g.
Scanner.space), numberState is reset back to 0 and number parsing is aborted.
Idents in states (2), (5) and (5a) are analysed.
Ident (2) must meet the following production: {hexDigit} ("X"|"H"). Trailing
character determines if the constant is CHAR ("X") or LONGINT ("H", stands for
"Hexadecimal").
Ident (5) can only be = "E" (REAL type) or "D" (LONGREAL type, "D" stands for
"Double precision").
Note that if plus or minus (6) are not present, e.g. as in "1.0E4", then
ident (5a) will include the number (ident = "E4"), which otherwise would be
found in state (7). The state (?) decides which of the two states is to be
taken by the scanner.
Invalid tokens or token contents (bad ident values) abort the number parsing.
The only error condition is misssing required token in (7), otherwise the
offending token is output separately as if it was never intended to be part of
a number. The situation will arise when a number token is immediately followed
by an ident token, but that should be flagged as error on a higher level of
parsing, e.g. in parsing of the expressions.

If I were to design the number format, I'd simplify it as follows:
             number ([ident] | period number [ident (plus | minus) number])
numberState =   1       2         3     4       5      6      6       7
numberState flow diagram:
->1>-------------\    => simple decimal number ("1")
  |              |
  |--->2>--------|    => hexadecimal number or a CHAR constant ("1H" or "1X")
  |              |
  3>->4>---------|    => simple REAL number ("1.0")
      |          |
      5>->6>->7>-|    => REAL or LONGREAL number with exponent ("1.0E+0")
                 |
               (END)
*)
(* Notes for the CASE:
- we should transform the s.currToken, advance numberState;
- s.currToken should remain = Scanner.number until the number parsing is finished;
- when number parsing is finished, set s.numberState = 0;
- call PushCurrToken if s.nextToken is valid and is not considered part of the
number, otherwise simply reset the s.numberState and assign s.currToken #
Scanner.number. *)
      CASE s.numberState OF
      | 1:
         IF s.nextToken = Scanner.ident THEN (* 2 *)
            s.currToken := Ident2ToToken ();
            ASSERT (s.currToken # Scanner.number, 20); (* prevent loop *)
            IF s.currToken = error THEN
               s.currToken := Scanner.number;
               PushCurrToken;
            END;
         ELSIF s.nextToken = Scanner.period THEN
            s.numberState := 3;
         ELSE
            PushCurrToken;
         END;
      | 3:
         IF s.nextToken = Scanner.number THEN
            s.numberState := 4;
         ELSIF (s.nextToken = Scanner.ident) & IsValidIdent5 () THEN (* 5a or 5 *)
            IF s.scanner.ident [1] = 0X THEN (* 5 *)
               s.numberState := 5;
            ELSE (* 5a *)
               RealNumFinished;
            END;
         ELSE
            PushCurrToken;
            s.numberState := 1;
         END;
      | 4:
         IF (s.nextToken = Scanner.ident) & IsValidIdent5 () THEN (* 5a or 5b *)
            IF s.scanner.ident [1] = 0X THEN (* 5b *)
               s.numberState := 5;
            ELSE (* 5a *)
               RealNumFinished;
            END;
         ELSE
            PushCurrToken;
            s.numberState := 1;
         END;
      | 5:
         IF (s.nextToken = Scanner.plus) OR (s.nextToken = Scanner.minus) THEN
            s.numberState := 6;
         ELSIF s.nextToken = Scanner.number THEN
            RealNumFinished;
         ELSE
            Error;
         END;
      | 6:
         IF s.nextToken = Scanner.number THEN
            RealNumFinished;
         ELSE
            Error;
         END;
      END;
   END ScanNumber;

BEGIN
   (* transform currToken according to the received nextToken *)
   CASE s.currToken OF
   | Scanner.none:
      CopyNextTokenToCurr;
   | Scanner.number:
      ScanNumber;
   ELSE
      PushCurrToken;
   END;
END HandleNextToken;

PROCEDURE AssignNextToken (VAR s: ScannerDesc);
(* Copy s.scanner.token to s.nextToken, and related fields. *)
BEGIN
   s.nextToken := s.scanner.token;
   s.nextTokenPos := s.scanner.tokenPos;
   IF s.nextToken = Scanner.ident THEN
      COPY (s.scanner.ident, s.nextIdent);
      s.nextIdentLen := s.scanner.identLen;
   END;
END AssignNextToken;

PROCEDURE (VAR s: ScannerDesc) Scan* (ch: CHAR);
BEGIN
   s.done := FALSE;
   s.scanner.Scan (ch);
   IF s.scanner.done THEN
      AssignNextToken (s);
      HandleNextToken (s);
   END;
   s.pos := s.scanner.pos;
END Scan;

PROCEDURE (VAR s: ScannerDesc) Eof* ();
(** Call Eof and retrieve s.token until s.done = FALSE. *)
VAR prev: LONGINT;

   PROCEDURE ScanEof (): BOOLEAN;
   BEGIN
      s.scanner.Eof;
      RETURN s.scanner.done
   END ScanEof;

BEGIN
   s.done := FALSE;
   WHILE ~s.done & ScanEof () DO
      AssignNextToken (s);
      HandleNextToken (s);
   END;
   IF ~s.done THEN
      prev := s.token;
      s.nextToken := Scanner.none;
      s.nextTokenPos := s.pos;
      HandleNextToken (s);
      s.done := s.token # prev;
   END;
END Eof;

END ScanNumbers.
