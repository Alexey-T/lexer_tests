<* MAIN+ *> (* This marks the main module of a program or library.          *)
MODULE ScannerTool;

(* ------------------------------------------------------------------------
 * (C) 2009 by Alexander Iljin
 * ------------------------------------------------------------------------ *)

IMPORT Out, Str;

(** ------------------------------------------------------------------------
  * This program will create the ScannerConsts interface module.
  * ----------------------------------------------------------------------- *)

CONST
   ModuleName = 'ScannerConsts';
   EOL = '' + 0AX; (* The Out module will produce 0D+0A instead. *)

   (* Within groups of identifiers starting with the same letter it is best to
    * list the identifiers in the reverse order of use frequency for an
    * additional bit of recognition speed. It is assumed that all the
    * predefined identifiers satisfy the 'ident' production, otherwise they
    * won't be recognized as identifiers in the first place. *)
   PredefinedIdentList = 'ASH,ABS,ARRAY,ASSERT,'
      + 'BY,BYTE,BOOLEAN,BEGIN,'
      + 'CAP,CHR,CASE,COPY,CONST,CHAR,'
      + 'DIV,DEC,DO,'
      + 'EXIT,ENTIER,EXCL,ELSIF,ELSE,END,'
      + 'FINALLY,FINALIZE,FOR,FALSE,'
      + 'HALT,'
      + 'INCL,IN,IMPORT,IS,INC,INTEGER,IF,'
      + 'LOOP,LONGREAL,LONG,LONGINT,LEN,'
      + 'MODULE,MOD,MIN,MAX,'
      + 'NIL,NEW,'
      + 'ODD,OF,ORD,OR,'
      + 'POINTER,PROCEDURE,'
      + 'REPEAT,REAL,RECORD,RETURN,'
      + 'SET,SIZE,SHORT,SHORTINT,SYSTEM,'
      + 'TRUE,TO,TYPE,THEN,'
      + 'UNTIL,'
      + 'VAR,'
      + 'WITH,WHILE';
   (* For each identifier from the list above a token value is automatically
    * assigned starting with FirstPredefinedIdentToken and lower in the order
    * of the list. *)
   FirstPredefinedIdentToken = -1001;

VAR
   lastToken: LONGINT;

PROCEDURE Head ();
BEGIN
   Out.String ('MODULE ' + ModuleName + ';' + EOL
      + EOL
      + '(* ------------------------------------------------------------------------' + EOL
      + ' * (C) 2009 by Alexander Iljin' + EOL
      + ' * ------------------------------------------------------------------------ *)' + EOL
      + EOL
      + '(* ------------------------------------------------------------------------' + EOL
      + ' * This module is automatically generated by ScannerTool.' + EOL
      + ' * ------------------------------------------------------------------------ *)' + EOL
      + EOL
   );
END Head;

PROCEDURE AddToken (VAR str: ARRAY OF CHAR; beg, end: LONGINT; VAR tag: LONGINT): BOOLEAN;
BEGIN
   ASSERT (beg < end, 20);
   Out.String ('   ');
   WHILE beg < end DO
      Out.Char (str [beg]);
      INC (beg);
   END;
   Out.String ('* = ');
   Out.Int (tag, 0);
   Out.String (';' + EOL);
   lastToken := tag;
   DEC (tag);
   RETURN TRUE
END AddToken;

PROCEDURE Body ();

   PROCEDURE Pre (VAR name: ARRAY OF CHAR);
   BEGIN
      Out.String ('   ');
      Out.String (name);
      Out.String ('* = ');
   END Pre;

   PROCEDURE StrConst (name, value: ARRAY OF CHAR; quote: CHAR);
   CONST (* value is output in pieces not longer than MaxLen *)
      LineWidth = 78;
      ComponentOverhead = 6 (* spaces *) + 2 (* "+ " *) + 2 (* quotes *);
      NameOverhead = 3 (* spaces *) + 3 (* "* = " *) + 2 (* quotes *);
      MaxLen = LineWidth - ComponentOverhead;
   VAR
      i: LONGINT;

      PROCEDURE WritePiece (numChars: LONGINT);
      BEGIN
         Out.Char (quote);
         WHILE (numChars > 0) & (value [i] # 0X) DO
            Out.Char (value [i]);
            INC (i);
            DEC (numChars);
         END;
         Out.Char (quote);
      END WritePiece;

   BEGIN (* StrConst *)
      Pre (name);
      i := 0;
      WritePiece (LineWidth - LEN (name) - NameOverhead);
      WHILE value [i] # 0X DO
         Out.String (EOL + '      + ');
         WritePiece (MaxLen);
      END;
      Out.String (';' + EOL);
   END StrConst;

   PROCEDURE IntConst (name: ARRAY OF CHAR; value: LONGINT);
   BEGIN
      Pre (name);
      Out.Int (value, 0);
      Out.String (';' + EOL);
   END IntConst;

   PROCEDURE ListTokens (list: ARRAY OF CHAR; tag: LONGINT);
   BEGIN
      Str.ChangeCase (list, Str.lowerCase);
      IF Str.ProcessList (list, ',', AddToken, tag) THEN
      END;
   END ListTokens;

BEGIN (* Body *)
   Out.String ('CONST' + EOL);
   StrConst ('PredefinedIdentList', PredefinedIdentList, "'");
   IntConst ('FirstPredefinedIdentToken', FirstPredefinedIdentToken);
   ListTokens (PredefinedIdentList, FirstPredefinedIdentToken);
   IntConst ('lastToken', lastToken);
   Out.String (EOL);
END Body;

PROCEDURE Tail ();
BEGIN
   Out.String ('END ' + ModuleName + '.' + EOL);
END Tail;

BEGIN
   Out.Open;
   Head;
   Body;
   Tail;
END ScannerTool.
