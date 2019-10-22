MODULE Scanner;

(* ---------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * --------------------------------------------------------------------------- *)

IMPORT
   SC:=ScannerConsts, Str;

(* Usage: declare an instance of s: ScannerDesc and initialize it by calling
 * s.Init. Then call s.Scan feeding it one character of the input stream at a
 * time. After every s.Scan call check if s.done = TRUE. If it is, then you
 * can read the current recognized token in s.token. If s.token = ident, then
 * also read s.ident and s.identLen. If s.identLen > LEN (s.ident) - 1, then
 * the current identifier was too long for the buffer, and s.identLen contains
 * its actual size. s.ident is still 0X-terminated in this case, but it only
 * contains the first part of the identifier.
 *
 * The positions are calculated starting from 0 (reset by the Init method).
 * s.pos is increased after every s.Scan call, and s.identPos is updated every
 * time s.done = TRUE after the s.Scan call.
 *
 * When the input stream is over there may still be some tokens cached or
 * being in the process of recognition. To flush the cache you should keep
 * calling s.Eof and receiving the s.token values until s.done = FALSE. *)

CONST
   MaxIdentLength = 254; (* As supported by XDS Oberon. *)

   (* token values *)
   none*       =  0; (* not a valid token identifier *)
   unknown*    = -1; (* default value for unhandled characters *)
   eolCR*      = -2; (* internal token for 0DX, reported as 'eol' *)
   eolLF*      = -3; (* internal token for 0AX, reported as 'eol' *)
   eol*        = -4; (* end-of-line token, stands for: CR, LF or CRLF *)
   space*      = -5; (* ' ' *)
   tab*        = -6; (* tab character = 9X *)
   lparen*     = -7; (* '(' *)
   rparen*     = -8; (* ')' *)
   times*      = -9; (* '*' *)
   commentBeg* = -10; (* lparen + times *)
   commentEnd* = -11; (* times + rparen *)
   slash*      = -12; (* '/' *)
   and*        = -13; (* '&' *)
   plus*       = -14; (* '+' *)
   minus*      = -15; (* '-' *)
   equal*      = -16; (* '=' *)
   notEqual*   = -17; (* '#' *)
   less*       = -18; (* '<' *)
   greater*    = -19; (* '>' *)
   lessEq*     = -20; (* <= *)
   greaterEq*  = -21; (* >= *)
   arrow*      = -22; (* '^' *)
   period*     = -23; (* '.' *)
   comma*      = -24; (* ',' *)
   colon*      = -25; (* ':' *)
   upto*       = -26; (* .. *)
   rbrak*      = -27; (* ']' *)
   rbrace*     = -28; (* '}' *)
   lbrak*      = -29; (* '[' *)
   lbrace*     = -30; (* '{' *)
   not*        = -31; (* '~' *)
   becomes*    = -32; (* := *)
   semicolon*  = -33; (* ';' *)
   bar*        = -34; (* '|' *)
   ident*      = -35; (* an identifier, except a predefined one *)
   number*     = -36; (* a sequence of digits *)
   pragmaBeg*  = -37; (* <* *)
   pragmaEnd*  = -38; (* *> *)
   quote*      = -39; (* "'" *)
   dblQuote*   = -40; (* '"' *)
   dblMinus*   = -41; (* -- *)
   lastToken*  = -100; (* Create more tokens below this value *)

TYPE
   PredefIdent = POINTER TO PredefIdentDesc;
   PredefIdentDesc = RECORD
      ident: POINTER TO ARRAY OF CHAR;
      token: LONGINT;
      next: PredefIdent;
   END;

   Ident* = ARRAY MaxIdentLength + 1 OF CHAR;
   ScannerDesc* = RECORD
      pos-, currToken, currTokenPos, nextToken, nextTokenPos: LONGINT;
      done-: BOOLEAN; (* if TRUE after Scan, then read token, tokenPos *)
      token-, tokenPos-: LONGINT; (* recognized token when 'done' = TRUE *)
      ident-: Ident; (* ident is always 0X-terminated *)
      identLen-: LONGINT; (* may be > LEN (ident) *)
   END;
   (* As soon as you get (.done = TRUE) & (.token = ident), you must grab a
    * copy of the .ident value, because the buffer is reused for the next
    * identifier. IF .identLen > MaxIdentLength THEN .ident contains a
    * 0X-terminated copy of the first part of the read identifier. *)

VAR
   tokenTable: ARRAY ORD (MAX (CHAR)) + 1 OF LONGINT;
   predefinedIdents: ARRAY ORD (MAX (CHAR)) + 1 OF PredefIdent;
   maxPredefinedIdentLength: LONGINT;

PROCEDURE (VAR s: ScannerDesc) Init*;
BEGIN
   s.pos := 0;
   s.done := FALSE;
   s.token := none;
   s.tokenPos := 0;
   s.currToken := none;
   s.currTokenPos := 0;
   s.nextToken := none;
   s.nextTokenPos := 0;
   s.ident [LEN (s.ident) - 1] := 0X; (* this value is preserved *)
   s.identLen := 0;
END Init;

PROCEDURE HandleNextToken (VAR s: ScannerDesc);

   PROCEDURE OutputCurrToken ();
   BEGIN
      s.done := TRUE;
      IF (s.currToken = eolCR) OR (s.currToken = eolLF) THEN
         s.token := eol;
      ELSE
         s.token := s.currToken;
      END;
      s.tokenPos := s.currTokenPos;
   END OutputCurrToken;

   PROCEDURE CopyNextTokenToCurr;
   BEGIN
      s.currToken := s.nextToken;
      s.currTokenPos := s.nextTokenPos;
   END CopyNextTokenToCurr;

   PROCEDURE PushCurrToken;
   BEGIN
      OutputCurrToken;
      CopyNextTokenToCurr;
   END PushCurrToken;

   PROCEDURE UpdateOrPush (ifNextToken, thenCurrentToken: LONGINT);
   BEGIN
      IF s.nextToken = ifNextToken THEN
         s.currToken := thenCurrentToken;
      ELSE
         PushCurrToken;
      END;
   END UpdateOrPush;

   PROCEDURE RecognizePredefinedIdent ();
   (* If the currently read identifier is recognized, then replace s.currToken
    * with the corresponding token. *)
   VAR id: PredefIdent;
   BEGIN
      IF s.identLen < LEN (s.ident) THEN
         s.ident [s.identLen] := 0X;
         id := predefinedIdents [ORD (s.ident [0])];
         WHILE (id # NIL) & (id.ident^ # s.ident) DO
            id := id.next;
         END;
         IF id # NIL THEN
            s.currToken := id.token;
         END;
      END;
   END RecognizePredefinedIdent;

BEGIN
   s.done := FALSE;
   (* transform currToken according to the received nextToken *)
   IF s.currToken = ident THEN (* letter { letter | digit } *)
      IF (s.nextToken # ident) & (s.nextToken # number) THEN
         RecognizePredefinedIdent;
         PushCurrToken;
      END;
   ELSE
      CASE s.currToken OF
      | none:
         CopyNextTokenToCurr;
      | space: (* accumulate consecutive spaces *)
         UpdateOrPush (space, space);
      | eolCR: (* CR + LF = eol *)
         UpdateOrPush (eolLF, eol);
      | lparen: (* ( * *)
         UpdateOrPush (times, commentBeg);
      | times: (* * ), *> *)
         IF s.nextToken = rparen THEN
            s.currToken := commentEnd;
         ELSE
            UpdateOrPush (greater, pragmaEnd);
         END;
      | less: (* <=, <* *)
         IF s.nextToken = equal THEN
            s.currToken := lessEq;
         ELSE
            UpdateOrPush (times, pragmaBeg);
         END;
      | greater: (* >= *)
         UpdateOrPush (equal, greaterEq);
      | period: (* .. *)
         UpdateOrPush (period, upto);
      | colon: (* := *)
         UpdateOrPush (equal, becomes);
      | number: (* digit { digit } *)
         UpdateOrPush (number, number);
      | minus: (* -- *)
         UpdateOrPush (minus, dblMinus);
      ELSE
         PushCurrToken;
      END;
   END;
END HandleNextToken;

PROCEDURE (VAR s: ScannerDesc) Scan* (ch: CHAR);
BEGIN
   s.nextToken := tokenTable [ORD (ch)];
   s.nextTokenPos := s.pos;
   HandleNextToken (s);
   IF s.currToken = ident THEN
      (* remember the current char to match with the predefined idents *)
      IF s.identLen < LEN (s.ident) - 1 THEN (* last char is always 0X *)
         s.ident [s.identLen] := ch;
      END;
      INC (s.identLen);
   ELSE
      s.identLen := 0;
   END;
   INC (s.pos);
END Scan;

PROCEDURE (VAR s: ScannerDesc) Eof* ();
(** Call Eof and retrieve s.token until s.done = FALSE. *)
BEGIN
   s.token := none;
   s.nextToken := none;
   s.nextTokenPos := s.pos;
   HandleNextToken (s);
   s.done := s.token # none;
END Eof;

PROCEDURE AddPredefinedIdent (VAR id: ARRAY OF CHAR; beg, end: LONGINT; VAR token: LONGINT): BOOLEAN;
VAR
   len: LONGINT;
   ident: PredefIdent;
BEGIN
   ASSERT ((end > beg) & (id [beg] # 0X), 20);
   ASSERT ((token # none) & (token # unknown) & (token < 0), 21);
   len := end - beg;
   IF maxPredefinedIdentLength < len THEN
      maxPredefinedIdentLength := len;
   END;
   NEW (ident);
   NEW (ident.ident, len + 1);
   ident.ident^ [len] := 0X;
   WHILE len # 0 DO
      DEC (len);
      DEC (end);
      ident.ident^ [len] := id [end];
   END;
   ident.token := token;
   ident.next := predefinedIdents [ORD (ident.ident^ [0])];
   predefinedIdents [ORD (ident.ident^ [0])] := ident;
   DEC (token);
   RETURN TRUE
END AddPredefinedIdent;

PROCEDURE Init ();

   PROCEDURE ClearTokenTable ();
   VAR
      i: INTEGER;
   BEGIN
      i := 0;
      WHILE i < LEN (tokenTable) DO
         tokenTable [i] := unknown;
         INC (i);
      END;
   END ClearTokenTable;

   PROCEDURE AddDigits ();
   VAR
      i: INTEGER;
   BEGIN
      i := ORD ('0');
      WHILE i <= ORD ('9') DO
         tokenTable [i] := number;
         INC (i);
      END;
   END AddDigits;

   PROCEDURE AddLetters ();
   VAR
      i: INTEGER;
   BEGIN
      i := ORD ('a');
      WHILE i <= ORD ('z') DO
         tokenTable [i] := ident;
         tokenTable [ORD (CAP (CHR (i)))] := ident;
         INC (i);
      END;
      tokenTable [ORD ('_')] := ident;
   END AddLetters;

   PROCEDURE AddToken (ch: CHAR; token: LONGINT);
   BEGIN
      ASSERT (tokenTable [ORD (ch)] = unknown, 20);
      ASSERT ((token # none) & (token # unknown) & (token < 0), 21);
      tokenTable [ORD (ch)] := token;
   END AddToken;

   PROCEDURE AddPredefinedIdentList (list: ARRAY OF CHAR; token: LONGINT);
   BEGIN
      IF Str.ProcessList (list, ',', AddPredefinedIdent, token) THEN
      END;
   END AddPredefinedIdentList;

BEGIN (* Init *)
   ClearTokenTable;
   AddDigits;
   AddLetters;
   AddToken (09X, tab);
   AddToken (0AX, eolLF);
   AddToken (0DX, eolCR);
   AddToken (' ', space);
   AddToken ('(', lparen);
   AddToken (')', rparen);
   AddToken ('*', times);
   AddToken ('/', slash);
   AddToken ('&', and);
   AddToken ('+', plus);
   AddToken ('-', minus);
   AddToken ('=', equal);
   AddToken ('#', notEqual);
   AddToken ('<', less);
   AddToken ('>', greater);
   AddToken ('^', arrow);
   AddToken ('.', period);
   AddToken (',', comma);
   AddToken (':', colon);
   AddToken (']', rbrak);
   AddToken ('}', rbrace);
   AddToken ('[', lbrak);
   AddToken ('{', lbrace);
   AddToken ('~', not);
   AddToken (';', semicolon);
   AddToken ('|', bar);
   AddToken ("'", quote);
   AddToken ('"', dblQuote);
   AddPredefinedIdentList (SC.PredefinedIdentList, SC.FirstPredefinedIdentToken);
END Init;

BEGIN Init
END Scanner.
