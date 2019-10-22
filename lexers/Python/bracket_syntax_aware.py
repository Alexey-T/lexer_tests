            nstart, nlen = ed_get_sel()
            re.sub(r"\(", sChr):   # (
            ed_lock()
            ed_cmd(cmd_SelectToken, '')
            ns, nl = ed_get_sel()
            sSel = ed_get_text_sel()   # )
            ed_set_caret_pos(iPos)
            ed_set_sel(nstart, nlen, True)
            re.sub(r"\)", sChr):
            ed_unlock()
