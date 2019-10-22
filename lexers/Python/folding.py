  if aaa
    aaa
    aaa
  if bbb
    bbb
    bbb
  if cc     
    def _calltip_erase():
    
        status_lock.acquire()
        
        try:
            if msg == status_msg.get(id, [None, None, 0])[1]:
                view.erase_status(id)
                status_msg[id][1] = None
                if id in status_lineno:
                    del status_lineno[id]
        finally:
            status_lock.release()

    sublime.set_timeout(_calltip_set, delay or 0)

    if msg:
        sublime.set_timeout(_calltip_erase, timeout)
