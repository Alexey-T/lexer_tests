import re
from cudatext import *

def do_conv(s):
    # escape quotes and backslashes in selected text
    s = re.sub(r'("|\'|\\)', r'\\\1', s)
    # quote the lines and add commas
    s = re.sub(r'(.*)', r"'\1',", s)
    # remove trailing comma
    s = s[:-1]
    # add the beginning and ending
    s = "[" + s + "].join('\\n');"
    return s

class Command:
    def do_copy(self):
        text = ed.get_text_sel()
        if not text: return
        text = do_conv(text)
        app_proc(PROC_SET_CLIP, text)
        msg_status('Copied JS array to clipboard')

    def do_replace(self):
        text = ed.get_text_sel()
        if not text:
            msg_status('Text not selected')
            return
        
        carets = ed.get_carets()
        if len(carets)!=1:
            msg_status('Need single caret')
            return
            
        x1, y1, x2, y2 = carets[0]
        if (y1>y2) or ((y1==y2) and (x1>x2)):
            x1, x2 = x2, x1
            y1, y2 = y2, y1
        
        text = do_conv(text)
        ed.delete(x1, y1, x2, y2)
        ed.insert(x1, y1, text)
        msg_status('Replaced sel with JS code')
        
        #Now correct selection
        lines = text.splitlines()
        last_len = len(lines[len(lines)-1])
        ed.set_caret(x1, y1, last_len, y1+len(lines)-1)
                 
        