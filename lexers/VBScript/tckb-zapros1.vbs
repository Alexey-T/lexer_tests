Option Explicit : Dim C, List, Path, Count, Ext, ShA, FSO, F,_
Test, Filt, BN, Ent, M, Items, Cnt, Ln, x, Item, Max, i, FN
 
With WScript.Arguments
  C = .Count : If C = 0 Then WScript.Quit
  If C < 3 Then MsgBox "ƒолжно быть указано не менее 3-х параметров!", 48 : WScript.Quit : End If
  List = .Item(0) : Path = .Item(1) : Count = .Item(2) : If C = 4 Then Ext = "." & .Item(3)
End With : Set ShA  = CreateObject("Shell.Application")
Set FSO  = CreateObject("Scripting.FileSystemObject")
Set List = FSO.OpenTextFile(List,,,-1)
Do Until List.AtEndOfStream
  F = Trim(List.ReadLine)
  If F <> "" Then
    If FSO.FolderExists(F) Then
      Test = 1 : Filt = 160 : BN = FSO.GetFileName(F)
    Else
      Test = 0 : Filt = 192 : BN = FSO.GetBaseName(F)
      If C = 3 Then Ext = "." & FSO.GetExtensionName(F)
    End If : Ent = InStrRev(BN, "(") : Max = 0
    If Ent And Right(BN, 1) = ")" Then
      M = Mid(BN, Ent + 1, Len(BN) - Ent - 1)
      If IsNumeric(M) Then Max = CLng(M)
      BN = Left(BN, Ent - 2)
    End If
    Set Items = ShA.NameSpace(Path).Items
    Items.Filter Filt, BN & " (*)" & Ext
    Cnt = Items.Count : Ln = Len(BN) + 3
    If Cnt Then
      For x = 0 to Cnt - 1
        Item = Items.Item(x) : M = Mid(Item, Ln, Len(Item) - Ln - Len(Ext))
        If IsNumeric(M) Then : If CLng(M) > Max Then Max = CLng(M) End If
      Next
    End If
    For i = 1 To Count
      FN = FSO.BuildPath(Path, BN & " (" & Max + i & ")")
      If Test Then
        If C = 4 Then FSO.CreateFolder FN Else FSO.GetFolder(F).Copy FN, 0
      Else
        If C = 4 Then FSO.CreateTextFile(FN & Ext) Else FSO.CopyFile F, FN & Ext, 0
      End if
    Next
  End If
Loop
