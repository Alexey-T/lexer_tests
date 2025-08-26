# Comment text
import re
from cudatext import *

class Command:
    def do_replace(self):
        text = ed.get_text_sel()
        if not text:
            msg_status('Text not selected')
            return
        
        carets = ed.get_carets()
        if len(carets)!=1:
            msg_status('Need single caret')
            return
