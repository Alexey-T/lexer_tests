    oo::class create bgExecProcess {
         self variable bgExecNextNr
         self method nextNr {} {
              return [incr bgExecNextNr]
         }
         variable PIDs sysHandle logHandle
         constructor {log exec timeout wrkobj} {
            try {
               set logHandle [open $log w]
               fconfigure $logHandle -buffering line; # im Falle eines Abbruchs ist sonst aller Output verloren!
            } on error {result options} {
               return -options $options $result; # re-raise error
            }
            if {[catch {
                   bgExec                                               \
                      $exec                                             \
                      [list bgExecCB [bgExecProcess nextNr] $logHandle] \
                      bgExecCurrentInstances                            \
                      $timeout                                          \
                      [list bgExecTOut [self] $wrkobj]                  \
                      [list bgExecEOF  [self] $wrkobj]                  \
               } result options]} {
               return -options $options $result; # re-raise error
            }
            set PIDs [pid $result]
            # konnte scheitern, wenn Prozess sofort wieder weg ist? (sofort EOF?)
            # damit wir dies mitbekommen, catch wieder entfernt
            set sysHandle [twapi::get_process_handle [lindex $PIDs end]]
            $wrkobj incrBusy
         }
         destructor {
            catch {close $logHandle}
         }
         # Diese Felder konnen erst nach Erzeugen der Instanz besetzt werden;
         # Sie konnen daher auch nicht schon bereits an bgExec's Callbacks ubergeben
         # werden. Diese CBs mussen sich daher die Werte uber diese GETter holen:
         method getPIDs {} {
            return $PIDs
         }
         method getSysHandle {} {
            return $sysHandle
         }
    }
