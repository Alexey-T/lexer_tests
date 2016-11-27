    ####################################
    #
    #  SOME PROCEDURES AND NAMESPACE THAT SHOULD BE VISIBLE
    #  IN TREE STRUCTURE AND ABLE TO BE CODE FOLDED
    #   KEYWORDS (are key sensitive):
    #   <proc> and <namespace> should be
    #   visible in tree structure something like:
    #   namespaces:
    #     login
    #   procedures:
    #     numerify
    #     focusNext
    #     tooltip
    #     ...
    #
    ####################################

    proc numerify {sir} {

       if {[string first . $sir]==-1} {
          return $sir.0
       }
       return $sir
    }

    proc focusNext {w} {
       bind $w <Key-KP_Enter> {focus [tk_focusNext %W]}
       bind $w <Key-Return> {focus [tk_focusNext %W]}
    }

    proc tooltip {w help} {
       bind $w <Any-Enter> "after 100 [list tooltip_show %W [list $help]]"
       bind $w <Any-Leave> "destroy %W.tooltip"
    }


    proc replicate {char nr} {
       set res ""
       for {set i 0} {$i < $nr} {incr i} {
          set res "$res$char"
       }
       return $res
    }

    proc cursor {cum} {

       if {$cum == "wait"} {
          set forma watch
       } else {
          set forma left_ptr
       }
       
       foreach wname [winfo children .] {
          catch {$wname configure -cursor $forma}
       }

       update
       update idletasks
    }

    # debugger
    proc debug {} {

       if {![winfo exists .watchw]} {
          # ............
       }
      # ........

      uplevel #0 {
        set global_arrays {}
        foreach gv [lsort [info globals *]] {
          if {[regexp "^tcl_|.__tk|^tk_|^__tk_|^vTcl|^auto_index|^tkFocus|^tkPriv|^global_arrays" $gv]} continue;
          if {[catch {.watchw.g.t insert end "$gv" varname " [subst $$gv]\n\n"}]} {
            lappend global_arrays $gv
          }
        }
        .watchw.g.t insert end "--- GLOBAL ARRAYS\n" titles
        foreach gv $global_arrays {
          .watchw.g.t insert end "$gv " arrname "[array get $gv]\n\n"
        }
      }

      uplevel {
        set local_arrays {}
        foreach gv [lsort [info locals *]] {
          if {$gv=="local_arrays"} continue;
          if {[catch {.watchw.l.t insert end "$gv" varname " [subst $$gv]\n\n"}]} {
            lappend local_arrays $gv
          }
        }
        # ...
      }

      set gv stop
      vwait gv

      catch {
        .watchw.l.t delete 1.0 end
        wm iconify .watchw
      }

    }

    # namespace + procedures test

    namespace eval login {
      # following procedures run in login namespace   
    }

    proc login::go {} {
       global cfg dbc
       
       set cfg(nume_firma) [lindex [lindex $::cfg(firme) [.login.frame.fr.frlb.lb curselection]] 0]
       # ......
       
       if {[db::open "host=$::cfg(host) port=$::cfg(port) dbname=$::cfg(dbname) user='$::cfg(user)' password=$::cfg(passwd)"]==0} {
          return
       }
       
       # ...
    }

    proc login::Window {} {
       global cfg
       # ........
    }
