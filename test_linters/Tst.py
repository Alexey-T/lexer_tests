"test"
import sw

class Command(object):
    "test" 
    def ttest(self):
        pass
    def run(self):
        stest = "Lines count: " + str(sw.ed.get_line_count())
        sw.msg_box(sw.MSG_INFO, stest)
