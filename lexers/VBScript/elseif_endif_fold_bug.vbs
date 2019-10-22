    ' Функция проверки ПК на доступность
  Function PingStatus(RemotePC)
        On Error Resume Next
        Dim wmiquery, objwmiservice, objping, objstatus, DescriptErrorPingStatus, ErrorPingStatus

         wmiquery="Select * From win32_Pingstatus where Address='" & RemotePC & "'"

         set objwmiservice=getobject ("winmgmts:\\.\root\cimv2")
         set objping=objwmiservice.execquery(wmiquery)

         For each objstatus in objping

             If objstatus.statuscode = 0 then
                  PingStatus=true
                  'libwritelog ("ПК: " & RemotePC & " доступен")

              Else

                  PingStatus=false
                  ErrorPingStatus = objstatus.statuscode

                  If       ErrorPingStatus=11001 then
                         DescriptErrorPingStatus="Buffer Too Small"
                    elseif ErrorPingStatus=11002 then
                         DescriptErrorPingStatus="Destination Net Unreachable"
                    elseif ErrorPingStatus=11003 then
                         DescriptErrorPingStatus="Destination Host Unreachable"
                    elseif ErrorPingStatus=11004 then
                         DescriptErrorPingStatus="Destination Protocol Unreachable"
                    elseif ErrorPingStatus=11005 then
                         DescriptErrorPingStatus="Destination Port Unreachable"
                    elseif ErrorPingStatus=11006 then
                         DescriptErrorPingStatus="No Resources"
                    elseif ErrorPingStatus=11007 then
                         DescriptErrorPingStatus="Bad Option"
                    elseif ErrorPingStatus=11008 then
                         DescriptErrorPingStatus="Hardware Error"
                    elseif ErrorPingStatus=11009 then
                         DescriptErrorPingStatus="Packet Too Big"
                    elseif ErrorPingStatus=11010 then
                         DescriptErrorPingStatus="Request Timed Out"
                    elseif ErrorPingStatus=11011 then
                         DescriptErrorPingStatus="Bad Request"
                    elseif ErrorPingStatus=11012 then
                         DescriptErrorPingStatus="Bad Route"
                    elseif ErrorPingStatus=11013 then
                         DescriptErrorPingStatus="TimeToLive Expired Transit"
                    elseif ErrorPingStatus=11014 then
                         DescriptErrorPingStatus="TimeToLive Expired Reassembly"
                    elseif ErrorPingStatus=11015 then
                         DescriptErrorPingStatus="Parameter Problem"
                    elseif ErrorPingStatus=11016 then
                         DescriptErrorPingStatus="Source Quench"
                    elseif ErrorPingStatus=11017 then
                         DescriptErrorPingStatus="Option Too Big"
                    elseif ErrorPingStatus=11018 then
                         DescriptErrorPingStatus="Bad Destination"
                    elseif ErrorPingStatus=11032 then
                         DescriptErrorPingStatus="Negotiating IPSEC"
                    elseIf ErrorPingStatus=11050 then
                         DescriptErrorPingStatus="General Failure"
                    Else
                         DescriptErrorPingStatus="Неизвестная ошибка"

                  End If

                  'libwritelog ("ПК: " & RemotePC & " недоступен.")
                  'Libwritelog ("Ошибка: " & ErrorPingStatus & " Описание ошибки: " & DescriptErrorPingStatus)

             End If

         next
  End Function
   
  
  RemotePC="192.168.100.1"
  If PingStatus(RemotePC) = true then
    wscript.echo "Ping OK"   
  Else
    wscript.echo "Ping Failed"
  End If