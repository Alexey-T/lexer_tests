#
# You'll need to compile testmenu.menu  before running this
# mcompile testmenu.menu
#
main
	
#	show menu mymenu using my_menuhandler from "testmenu.mnu"
	show menu mymenu using my_menuhandler from "testmenu"

# if you don't have this - your program will exit here!
	call a4gl_run_gui()

end main


menuhandler my_menuhandler

on mn_exit
	exit program
end menuhandler



{

Plexus AD32 examples



function test()
DEFINE
	f_cust FORM

	OPEN MENU m_first USING m_first_mh
	OPEN FORM f_cust USING f_cust_fh


end function

FORMHANDLER f_cust_fh
	...
END FORMHANDLER

MENUHANDLER m_mainmenu_mh
	BEFORE SHOW MENU
		IF NOT gv_scanner_avail THEN
			DISABLE MENUITEM mi_scan
		END IF
	ON mi_scan,key_f2
		CALL scan_folder()
	ON mi_info
		DISPLAY gv_msg TO f_infoform.ent_msg
	ON mi_retrieve,key_f3
		OPEN FORM f_retrieve USING retrieve_fh
	ON key_pageup
		IF gv_in_retrieve THEN
			CALL get_prev_page()
		END IF
END MENUHANDLER m_mainmenu_mh


DDEHANDLER
	[define statements]
	ON ddemsg_constant
		#User-written code to reply to message


END DDEHANDLER

}
