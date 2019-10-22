MODULE ScanStretches;

(* ---------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * --------------------------------------------------------------------------- *)

IMPORT
   Scanner;

(* This scanner module detects stretches such as string constants, comments
 * and compiler pragmas. *)

CONST
   (* Additional tokens recognized by this scanner: *)
   comment*   = Scanner.lastToken - 1; (* comment contents *)
   pragma*    = Scanner.lastToken - 2; (* compiler pragma contents *)
   string*    = Scanner.lastToken - 3; (* string contents *)
   error*     = Scanner.lastToken - 4; (* Error while scanning a stretch *)
   lastToken* = error; (* Allocate more tokens below this value *)

(* The additional tokens are used as follows:
 * comment     = (Scanner.commentBeg [commentText] Scanner.commentEnd)
                 | (Scanner.dblMinus [commentText])
 * pragma      = (Scanner.pragmaBeg  [pragmaText] Scanner.pragmaEnd)
 * string      = (Scanner.quote      [stringText] Scanner.quote)
                 | (Scanner.dblQuote [stringText] Scanner.dblQuote)
 * Comments inside pragmas are not reported separately, they can be obtained
 * by scanning inside the 'pragmaText' stretch. Likewise, nested comments are
 * considered to be part of the outermost 'comment' stretch. Scanner.eol is
 * not considered part of a wing comment stretch.
 * 'error' token is output when either of the following occurs:
 * - commentEnd or pragmaEnd do not terminate a comment or pragma respectively;
 * - a string is terminated by Scanner.eol. *)

TYPE
   ScannerDesc* = RECORD
      scanner: Scanner.ScannerDesc;
      pos-, currToken, currTokenPos, nextToken, nextTokenPos: LONGINT;
      currIdent, nextIdent: Scanner.Ident;
      currIdentLen, nextIdentLen: LONGINT;
      done-: BOOLEAN; (* if TRUE after Scan, then read token, tokenPos *)
      token-, tokenPos-: LONGINT; (* recognized token when 'done' = TRUE *)
      ident-: Scanner.Ident;
      identLen-: LONGINT;
      lineState: LONGINT; (* The lineState DIV 2 is the comment nesting level
      * at the end of the line. If lineState = 1, then the last line ended
      * inside a compiler directive, which can't be nested. *)
      wingComment: BOOLEAN; (* Only valid when lineState = 0 *)
      stringConstantCh: CHAR; (* ' or " when 'scanner' is inside a string, 0X otherwise *)
      outputCurrToken: BOOLEAN; (* use this when you need to output the currToken
      * after eating the nextToken. *)
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
   s.lineState := 0;
   s.wingComment := FALSE;
   s.stringConstantCh := 0X;
   s.outputCurrToken := FALSE;
END Init;

PROCEDURE HandleNextToken (VAR s: ScannerDesc);
(* Pass Scanner.none to signal EOF, since it is known that s.scanner would
 * never produce the value. *)

   PROCEDURE OutputCurrToken ();
   BEGIN
      s.done := TRUE;
      s.token := s.currToken;
      s.tokenPos := s.currTokenPos;
      IF s.token = Scanner.ident THEN
         COPY (s.currIdent, s.ident);
         s.identLen := s.currIdentLen;
      END;
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

   PROCEDURE HandleNextTokenInsideCommentOrPragma;
   BEGIN
      IF s.wingComment THEN
         (* wing comments inside multiline comments are not recognized as such *)
         ASSERT ((s.lineState = 0) OR (s.lineState = 1), 100);
         IF (s.nextToken = Scanner.eol) OR (s.nextToken = Scanner.none) THEN
            s.wingComment := FALSE;
            IF s.lineState = 0 THEN (* not in pragma *)
               PushCurrToken;
            END;
         END;
      ELSE
         ASSERT (s.lineState > 0, 101);
         IF s.nextToken = Scanner.commentEnd THEN
            IF s.lineState > 1 THEN (* minus one comment nesting level *)
               DEC (s.lineState, 2);
               IF s.lineState = 0 THEN (* comment is terminated *)
                  s.outputCurrToken := TRUE;
               END;
            ELSE
               PushCurrToken;
               s.currToken := error; (* unexpected comment terminator in pragma *)
               s.lineState := 0;
            END;
         ELSIF s.nextToken = Scanner.commentBeg THEN
            INC (s.lineState, 2);
         ELSIF s.nextToken = Scanner.pragmaEnd THEN
            IF s.lineState = 1 THEN
               s.lineState := 0;
               s.outputCurrToken := TRUE;
            END;
         ELSIF s.nextToken = Scanner.dblMinus THEN
            IF s.lineState < 2 THEN
               s.wingComment := TRUE;
            END;
         ELSIF s.nextToken = Scanner.pragmaBeg THEN
            IF s.lineState = 1 THEN (* pragmas can't be nested *)
               PushCurrToken;
               s.currToken := error;
               s.lineState := 0;
            END;
         ELSIF s.nextToken = Scanner.none THEN
            s.currToken := error;
            PushCurrToken;
            s.lineState := 0;
         END;
      END;
   END HandleNextTokenInsideCommentOrPragma;

   PROCEDURE HandleNextTokenNormally ();
   (* We are not inside a comment or pragma stretch. *)
   BEGIN
      CASE s.nextToken OF
      | Scanner.pragmaBeg:
         s.currToken := pragma;
         s.lineState := 1;
      | Scanner.pragmaEnd, Scanner.commentEnd:
         s.currToken := error;
      | Scanner.commentBeg:
         s.currToken := comment;
         s.lineState := 2;
      | Scanner.quote:
         s.currToken := string;
         s.stringConstantCh := "'";
      | Scanner.dblQuote:
         s.currToken := string;
         s.stringConstantCh := '"';
      | Scanner.dblMinus:
         ASSERT (s.lineState = 0, 102);
         s.currToken := comment;
         s.wingComment := TRUE;
      ELSE
         CopyNextTokenToCurr;
      END;
   END HandleNextTokenNormally;

BEGIN
   IF s.outputCurrToken THEN (* unconditionally output currToken once *)
      ASSERT ((s.currToken = comment) OR (s.currToken = pragma) OR (s.currToken = string), 103);
      s.outputCurrToken := FALSE;
      PushCurrToken;
      s.currToken := Scanner.none;
   END;
   (* transform currToken according to the received nextToken *)
   CASE s.currToken OF
   | Scanner.none:
      HandleNextTokenNormally;
   | string:
      IF (s.nextToken = Scanner.quote) & (s.stringConstantCh = "'")
         OR (s.nextToken = Scanner.dblQuote) & (s.stringConstantCh = '"')
      THEN (* found the end of string constant *)
         s.outputCurrToken := TRUE;
         s.stringConstantCh := 0X;
      ELSIF (s.nextToken = Scanner.eol) OR (s.nextToken = Scanner.none) THEN
         s.currToken := error; (* string terminated by EOL or EOF is an error *)
         PushCurrToken;
      END;
   | comment, pragma:
      HandleNextTokenInsideCommentOrPragma;
   ELSE
      PushCurrToken;
      HandleNextTokenNormally;
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

END ScanStretches.
