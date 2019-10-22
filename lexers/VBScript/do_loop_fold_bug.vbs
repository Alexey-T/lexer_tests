    do
      If right(strTemp, 1)="0" then
        strTemp=left(strTemp, len(strTemp)-1)
      Else
        exit do
      End If
    loop until len(strTemp)=0
