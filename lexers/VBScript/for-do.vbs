For each objService in colServiceList
RetCode = objService.StopService
If RetCode = 0 Then
libWriteLog("������ �����������.")
Else
libWriteLog("������ �� �����������. ������: " & RetCode)
End If
Next
