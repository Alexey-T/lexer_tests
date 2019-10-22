main
define env, varname char(60)

define axxx integer
define bxxx record
	a integer,
	b integer
end record
define a char(40)

#  options form line 4 	#this gives the result one would expect for a default;
                        #form is opened after menu and message lines - on line 3

{
dfgdfg comment
}

-- comment asdfasd

options help file "helpfile.hlp"
options help key F1

let axxx=1

menu "Hello test"
    command "window" "Open a new window" help 2
		#display "Window"
		open window w1 at 10,30 with 3 rows,20 columns attribute(border)
		display "   Hello  World   " at 2,2 attribute(cyan,reverse)
        sleep 2
		close window w1

    	command "prompt" "Prompt for user name"
		prompt "Enter your name :" for a
		on key(f10) 
			error  "F10 pressed"
			sleep 1
		end prompt
		display "Hello ",a clipped
		sleep 1

		
		

	command "form" "Open a form "
		#display "Form"
        	open window w2 at 2,2 with form "form" attribute(border)

        	display "This is a line in the form" to s_info[1].info_line attribute(reverse)
        	display "This is line 2 in the form" to s_info[2].info_line

        menu "Close window"
            command "OK" "This will close this window"
                exit menu
        end menu
	close window w2

    command "help"
        call showhelp(1)


    command "config"


		    let varname = "AUBITDIR"
			let env = fgl_getenv(varname)
			display "Using variable: ", varname clipped, " = ", env clipped at 5,5

			
			let env = fgl_getenv("AUBITDIR")
			display "Using string  : ","AUBITDIR = ", env clipped at 6,5


			let env = fgl_getenv("MY__impossible_TEST")
			if env is null then
				error "Env is null"
				sleep 1
			end if
			#this will display empty line, since fgl_getenv will fail to assign "env"
		    #variable because it is not defined anywhere, and set it to NULL string (bug #470960)
			display "MY_TEST = ", env clipped at 7,5 #6


	command "exit"
		error "Exit"
		#display "Exit"
        exit menu
	command key(f2)
		error "F2 pressed"
end menu
		sleep 1

#message "Exiting.."
end main



