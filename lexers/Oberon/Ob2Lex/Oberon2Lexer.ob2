<*+main*> (* This marks the main module of a program or library.            *)
<*heaplimit="2000000"*> (* Maximum heap size should be set in the main module,
because the changes do not take effect until the main module is recompiled. *)

MODULE Oberon2Lexer;

(* ------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * ------------------------------------------------------------------------ *)

IMPORT
   SYSTEM, Win:=Windows, Sci:=Scintilla, Npp:=NotepadPP, Lexer,
   Oberon2LexerVer;

(* ------------------------------------------------------------------------
 * This is the Oberon-2 Lexer plugin for Notepad++ (written in XDS Oberon).
 *
 * For the free XDS Modula-2/Oberon-2 compiler go to:
 *   http://www.excelsior-usa.com/xdsx86win.html
 *   (6.6 Mb to download, 17 Mb installed);
 * Oberon-2 is object-oriented programming language, both powerful and simple.
 * The full language report is only 20 pages long:
 *   http://europrog.ru/paper/oberon-2.pdf
 * It was created by prof. N.Wirth, author of Pascal and Modula. Some of his
 * publications:
 *   Programming in Oberon: http://europrog.ru/book/obnw2004e.pdf
 *   Compiler Construction: http://europrog.ru/book/ccnw2005e.pdf
 *   Project Oberon — The Design of an Operating System and Compiler:
 *     http://europrog.ru/book/ponw2005e.pdf
 * ------------------------------------------------------------------------ *)

CONST
   PluginName = 'Oberon-2 Lexer';
   LexerName = 'Oberon-2';               (* Lexer name <= 12 characters      *)
   LexerStatus = 'Oberon-2 source file'; (* Text displayed in the status bar *)

   (* Menu items *)
   AboutStr = 'About...';

   CRLF = ''+0DX+0AX;
   AboutMsg = 'This plugin adds '+PluginName+' to Notepad++. Version: '+Oberon2LexerVer.String+'.'+CRLF
      +CRLF
      +'Created by Alexander Iljin (Amadeus IT Solutions) using XDS Oberon, November - December 2009.'+CRLF
      +'Contact e-mail: AlexIljin@users.SourceForge.net'+CRLF
      +CRLF
      +'(You can press Ctrl+C to copy this text.)';

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

(*
TYPE
   JumpPos = POINTER TO RECORD
      file: Tools.Path;
      pos, anchor: LONGINT;
      next: JumpPos;
   END;

PROCEDURE GetCurrentPos (sc: Sci.Handle): JumpPos;
(* Return the current file and position as a jump target. *)
VAR res: JumpPos;
BEGIN
   NEW (res);
   Npp.GetFullCurrentPath (res.file);
   res.pos := Sci.GetCurrentPos (sc);
   res.anchor := Sci.GetAnchor (sc);
   RETURN res
END GetCurrentPos;

PROCEDURE SamePos (pos1, pos2: JumpPos): BOOLEAN;
BEGIN
   RETURN (pos1.file = pos2.file) & (pos1.pos = pos2.pos);
END SamePos;
*)

PROCEDURE ['C'] About ();
(* Show info about this plugin. *)
BEGIN
   Win.MessageBox (Npp.handle, AboutMsg, PluginName, Win.MB_OK)
END About;

PROCEDURE NppReady ();
(* This procedure is executed when Notepad++ has finished its startup. *)
BEGIN
END NppReady;

PROCEDURE CopyToBuff (str: ARRAY OF CHAR; buff: SYSTEM.ADDRESS; buflength: LONGINT);
VAR
   i: LONGINT;
   ch0X: CHAR;
BEGIN
   IF buflength > 0 THEN
      i := 0;
      DEC (buflength);
      WHILE (i < buflength) & (str [i] # 0X) DO
         INC (i);
      END;
      SYSTEM.MOVE (SYSTEM.ADR (str), buff, i);
      ch0X := 0X;
      SYSTEM.MOVE (SYSTEM.ADR (ch0X), buff + i, SIZE (CHAR));
   END;
END CopyToBuff;

<* DLLEXPORT+ *>

PROCEDURE ['StdCall'] GetLexerCount* (): LONGINT;
(** Return the number of lexers in this plugin. Up to 30 per instance of
  * Notepad++ (includes all external plugins.) *)
BEGIN
   RETURN 1
END GetLexerCount;

PROCEDURE ['StdCall'] GetLexerName* (index: LONGINT; VAR buff: ARRAY OF CHAR; buflength: LONGINT);
BEGIN
   IF index = 0 THEN
      CopyToBuff (LexerName, SYSTEM.ADR (buff), buflength);
   END;
END GetLexerName;

PROCEDURE ['StdCall'] GetLexerStatusText* (index: LONGINT; VAR buff: ARRAY OF CHAR; buflength: LONGINT);
BEGIN
   IF index = 0 THEN
      CopyToBuff (LexerStatus, SYSTEM.ADR (buff), buflength);
   END;
END GetLexerStatusText;

<* PUSH *><* +WOFF301 (* Disable "unused parameters" warning *) *>
PROCEDURE ['StdCall'] Lex* (lexer, startPos, length, initStyle: LONGINT; VAR words: ARRAY OF POINTER TO ARRAY OF CHAR; hwnd: Win.HWND; VAR props: ARRAY OF CHAR);
VAR hnd: Sci.Handle;
BEGIN
   IF lexer = 0 THEN (* Oberon-2 lexer *)
      Sci.Init (hnd, hwnd, TRUE);
      Lexer.Lex (startPos, length, hnd);
   END;
END Lex;
<* POP *>

<* PUSH *><* +WOFF301 (* Disable "unused parameters" warning *) *>
PROCEDURE ['StdCall'] Fold* (lexer, startPos, length, initStyle: LONGINT; VAR words: ARRAY OF POINTER TO ARRAY OF CHAR; hwnd: Win.HWND; VAR props: ARRAY OF CHAR);
BEGIN
   IF lexer = 0 THEN
      (* TODO *)
   END;
END Fold;
<* POP *>

PROCEDURE Init ();
BEGIN
   Npp.PluginName := PluginName;
   Npp.onReady := NppReady;
   Npp.AddMenuItem (AboutStr, About, FALSE, NIL)
END Init;

BEGIN Init
END Oberon2Lexer.
