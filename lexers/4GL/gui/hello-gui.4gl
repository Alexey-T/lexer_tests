main
display "STarting"
menu "WIN Hello test"
	before menu
		hide option "Close Form"

    command "MH" "Menu handler test"
        call mh_test()


	command "Window" "Open a new window"
		display "Window"
		open window w1 at 10,30 with 3 rows,20 columns attribute(border)
		display "   Hello  World   " at 2,2 attribute(cyan,reverse)
        	sleep 2
		close window w1

	command "Form" "Open a form "
		display "Form"
        	open window w2 at 2,2 with form "form-gui" attribute(border)

        	display "This is a line in the form" to s_info[1].info_line
        	display "This is line 2 in the form" to s_info[2].info_line

		show option "Close Form"
		hide option "Form"
		enable cal

	command "Close Form" "Close the form"
		close window w2
		hide option "Close Form"
		show option "Form"


    command "MultiTab"

        call multitab()

    command "Radio form" "Open Radio form in new window"
        	open window w4 at 2,2 with form "radio" attribute(border)

            menu "Radio form"
                command "Exit" "Close this window and return to previous menu"
                    exit menu
            end menu
			close window w4


    command "Widget form" "Open Widget form in new window"
        	open window w5 at 2,2 with form "widget" attribute(border)

            menu "Widget form"
                command "Exit" "Close this window and return to previous menu"
                    exit menu
            end menu
			close window w5


	command "Exit" "Exit demo"
		display "Exit"
        exit menu
end menu

message "Exiting.."
end main


function multitab()

    menu "MultiTab"


    command "As window" "Open Multi tab form in new window"
        	open window w3 at 2,2 with form "multi" attribute(border)

            menu "MultiTab form"
                command "Exit" "Close this window and return to previous menu"
                    exit menu
            end menu
			close window w3


    command "As form" "Open Multi tab form in existing window (screen)"

			open form f3 from "multi"
			display form f3

            menu "MultiTab form"
                command "Exit" "Close this form and return to previous menu"
                    exit menu
            end menu
			close form f3
            #clear form #NOT IMPLEMENTED ERROR
            clear window screen
			clear screen
            current window is screen

    command "Exit" "Return to previous menu"
            exit menu

    end menu


end function


####################
function mh_test()
####################

#	SHOW MENU my_menu USING my_menuhandler #default = "menu.mnu"
	#SHOW MENU my_menu USING my_menuhandler FROM "myfile"

#	ENABLE MENUITEM id

#	DISABLE MENUITEM id

    #SHOW WINDOW MyWindow USING my_formhandler

end function

{
============ FIRST ERROR: ==============
./hello-gui.c: In function `aclfglmn_my_menuhandler':
./hello-gui.c:546: parse error at end of input
========================================


############################
MENUHANDLER my_menuhandler
############################

DEFINE testvar char(20)

	BEFORE SHOW MENU
		ENABLE MENUITEM mn_1 # You can use MENUITEM or MENUITEMS here
		ENABLE MENUITEMS mn_2,mn_3
		DISABLE MENUITEM mn_1,mn_3
		DISABLE MENUITEMS mn_1

	ON mn_2
		DISPLAY "Hello World"
	ON mn_3
		EXIT PROGRAM



END MENUHANDLER

}
