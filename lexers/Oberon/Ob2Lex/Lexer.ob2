MODULE Lexer;

(* ---------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * --------------------------------------------------------------------------- *)

IMPORT
   Sci:=Scintilla, Scanner, ScanNumbers, ScanStretches, SC:=ScannerConsts;

(* ---------------------------------------------------------------------------
 * This is the Oberon-2 Lexer for Scintilla (written in XDS Oberon).
 * --------------------------------------------------------------------------- *)

CONST
   (* Style constants 0..31 max. Correspond to Oberon2Lexer.xml settings. *)
   DefaultStyle = 0;
   KeywordStyle = 1;
   FlowBreakerStyle = 2;
   UnsafeStyle = 3;
   NumberStyle = 4;
   StringStyle = 5;
   CommentStyle = 6;
   PragmaStyle = 7;
   ErrorStyle = 8;

PROCEDURE Lex* (startPos, length: LONGINT; hnd: Sci.Handle);
(** Initially startPos points to the beginning of a line. *)
CONST
   LowerStyleMask = 1FH; (* Allow lower 31 styles to be set. *)
   BuffSize = 256;
VAR
   line, lineBeg, toLineEnd: LONGINT; (* Current line number and its beg/end positions. *)
   lineCount: LONGINT; (* Number of lines in the document. *)
   buff: ARRAY BuffSize OF CHAR; (* Buffered document contents [0..end-1]. *)
   buffPos, buffLen: LONGINT; (* Current 'buff' position, and its used length. *)
   stylingPos: LONGINT; (* Current styling position in the document. *)
   currStyle, currStyleLen: LONGINT; (* Cache to avoid too many SetStyling calls *)
   scanner: ScanNumbers.ScannerDesc;
   prevStyle, prevTokenPos, savedTokenPos: LONGINT;
   canStyleFromThisLine: BOOLEAN;

   PROCEDURE FillBuff (VAR buff: ARRAY OF CHAR; VAR end: LONGINT);
   (* Fill 'buff' from 'hnd' -> buff [0..end - 1]. Contents of 'buff' do not
    * exceed the (stylingPos + length) limit. *)
   BEGIN
      IF length >= LEN (buff) THEN
         end := Sci.GetTextRange (hnd, stylingPos, stylingPos + LEN (buff) - 1, buff);
      ELSE
         end := Sci.GetTextRange (hnd, stylingPos, stylingPos + length, buff);
      END;
   END FillBuff;

   PROCEDURE ApplyCachedStyle ();
   BEGIN
      Sci.SetStyling (hnd, currStyleLen, currStyle);
   END ApplyCachedStyle;

   PROCEDURE AppendStyle (style, len: LONGINT);
   BEGIN
      IF style = currStyle THEN
         INC (currStyleLen, len);
      ELSE
         ApplyCachedStyle;
         currStyle := style;
         currStyleLen := len;
      END;
   END AppendStyle;

   PROCEDURE Style ();
   VAR
      style: INTEGER;
   BEGIN
      CASE scanner.token OF
      | SC.exit, SC.halt, SC.return:
         style := FlowBreakerStyle;
      | SC.system:
         style := UnsafeStyle;
      | Scanner.unknown, ScanStretches.error, ScanNumbers.error, Scanner.pragmaBeg,
      Scanner.pragmaEnd, Scanner.commentBeg, Scanner.commentEnd:
         style := ErrorStyle;
      | Scanner.number, ScanNumbers.hexNumber, ScanNumbers.realNumber, ScanNumbers.longRealNumber:
         style := NumberStyle;
      | ScanNumbers.hexChar, ScanStretches.string:
         style := StringStyle;
      | ScanStretches.pragma:
         style := PragmaStyle;
      | ScanStretches.comment:
         style := CommentStyle;
      ELSE
         IF scanner.token <= SC.FirstPredefinedIdentToken THEN
            style := KeywordStyle;
         ELSE
            style := DefaultStyle;
         END;
      END;
      AppendStyle (prevStyle, scanner.tokenPos - prevTokenPos);
      prevStyle := style;
      prevTokenPos := scanner.tokenPos;
   END Style;

   PROCEDURE StyleBuffer;
   (* Style contents of buff [0..beffLen-1]. *)
   BEGIN
      buffPos := 0;
      WHILE buffPos < buffLen DO
         scanner.Scan (buff [buffPos]);
         IF scanner.done THEN
            Style;
            IF scanner.tokenPos = lineBeg THEN
               canStyleFromThisLine := TRUE;
            END;
         END;
         INC (buffPos);
         INC (stylingPos);
         DEC (toLineEnd);
         IF toLineEnd = 0 THEN
            Sci.SetLineState (hnd, line, ORD (canStyleFromThisLine));
            INC (line);
            canStyleFromThisLine := FALSE;
            lineBeg := Sci.PositionFromLine (hnd, line) - startPos;
            toLineEnd := Sci.GetLineEndPosition (hnd, line) - stylingPos;
         END;
      END;
   END StyleBuffer;

BEGIN
   (* Implementation note: We set LineState = 0 for the lines that can be
    * safely styled from the beginning, and LineState = 1 for the lines that
    * can't. So, whenever you need to style a line with LineState = 1, you
    * have to look upwards until you find a line with LineState = 0 and start
    * styling from there. Effectively, the only lines with LineState = 1 are
    * the lines that are the ones with their beginning inside a multiline
    * comment or a compiler pragma stretch (no other token spans over EOL). *)
   line := Sci.LineFromPosition (hnd, startPos);
   scanner.Init;
   IF (line > 0) & (Sci.GetLineState (hnd, line) = 0) THEN
      DEC (line);
      WHILE (line > 0) & (Sci.GetLineState (hnd, line) = 0) DO
         DEC (line);
      END;
      (* correct the 'length' and the 'startPos' to the new 'line' value *)
      lineBeg := Sci.PositionFromLine (hnd, line);
      INC (length, startPos - lineBeg);
      startPos := lineBeg;
   END;
   Sci.StartStyling (hnd, startPos, LowerStyleMask);
   prevTokenPos := scanner.pos;
   prevStyle := DefaultStyle;
   currStyle := DefaultStyle;
   currStyleLen := 0;
   stylingPos := startPos;
   lineBeg := prevTokenPos;
   lineCount := Sci.GetLineCount (hnd);
   toLineEnd := Sci.GetLineEndPosition (hnd, line) - stylingPos;
   canStyleFromThisLine := TRUE;
   WHILE (length > 0) & (stylingPos >= 0) & (0 <= line) & (line < lineCount) DO
      FillBuff (buff, buffLen);
      StyleBuffer;
      DEC (length, buffLen);
   END;
   (* Keep styling for at least one token past the requested 'length'. *)
   savedTokenPos := prevTokenPos; (* when this changes, we're done *)
   length := Sci.GetTextLength (hnd) - stylingPos; (* left to EOF *)
   WHILE (length > 0) & (savedTokenPos = prevTokenPos) & (stylingPos >= 0) & (0 <= line) & (line < lineCount) DO
      FillBuff (buff, buffLen);
      StyleBuffer;
      DEC (length, buffLen);
   END;
   IF stylingPos >= Sci.GetTextLength (hnd) THEN
      scanner.Eof;
      WHILE scanner.done DO
         Style;
         scanner.Eof;
      END;
   END;
   AppendStyle (prevStyle, stylingPos - (startPos + prevTokenPos));
   ApplyCachedStyle;
END Lex;

END Lexer.
