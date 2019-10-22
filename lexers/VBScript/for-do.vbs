For each objService in colServiceList
RetCode = objService.StopService
If RetCode = 0 Then
libWriteLog("Служба остановлена.")
Else
libWriteLog("Служба не остановлена. Ошибка: " & RetCode)
End If
Next
