define
    ifx_sess,
    pg_sess
        smallint

{

main
define dummy integer
    database mysqlitedb
	prepare x1 from " select test from testing"
    #declare sc1 cursor for select test from testing
	declare sc1 cursor for x1
    foreach sc1 into dummy
		display dummy
    end foreach
end main



}

###########
main
###########

define
    p_tabname,
	DSN_informix,
	DSN_postgres
		char(40)

#	database maindb

    let ifx_sess = false
    let pg_sess = false

initialize 	DSN_informix to null
initialize	DSN_postgres to null

options prompt line 4
options message line 6

menu "Hello_db test"

    command "session"
		call try_session()

	command "Access" "Access database and list some data"

        call db_select ()

	command "Window" "Show a little window"
		open window w1 at 10,30 with 3 rows,20 columns attribute(border)
		display "   Hello  World   " at 2,2 attribute(cyan,reverse)
        sleep 2
		close window w1

	command "Exit" "Exit program"
        exit menu
end menu


end main


function try_session()
     define
         p_co_name char(30)

#     OPEN SESSION s_ifmx TO DATABASE maindb as user "informix" password "ifmx"

    display "opening session - this will probably fail unless you've edited the file..."
    display "to specify the username & password!"
    sleep 2

     open session session1 to database maxdev
       as user "root" password "mypassword"


	display "setting session..."
    sleep 2

	 set session to session1

    display "selecting data..."
    sleep 2

	 select tabname into p_co_name from systables
        where tabid = 1

	 display p_co_name
    sleep 2

    display "done."

	 sleep 5
end function



################################
function db_select()
################################
define
	DSN_informix,
	DSN_postgres
		char(40),
    cnt
        integer


		menu "BD type"

            command "Informix"
					 call ifx_SESSION("maindb")


            command "PostgreSQL"

                    call pg_SESSION("ptest")


            command "PostgreSQL scroll"

					call pg_SESSION_scroll("ptest")



            command "IFX static SQL"

				if not ifx_sess then
						OPEN SESSION s_ifmx TO DATABASE maindb as user "informix" password "ifmx"
		                let ifx_sess = true
		        end if

                SET SESSION TO s_ifmx


                select count(*) into cnt
                    from systables

                error "Count was ", cnt

                select tabid into cnt
                    from systables
                        where
                            tabname = "systables"


                #This should always be "1"

                error "Systables tabis is ", cnt, "(should be 1)"

                update systables
                    set tabname = "systables"
                        where tabname = "systables"


                error "Update syccessfull"


			command "Exit"

                exit menu


        end menu


end function


########################################
function ifx_DATABASE(DSN_informix)
########################################
define
    DSN_informix,
    p_tabname
        char (40)

                message "Connectiong to DSN maindb"#, DSN_informix clipped
                sleep 2

				database maindb #DSN_informix

#				database _variable(DSN_informix)

				declare c1 cursor for select tabname from systables

		        foreach c1 into p_tabname
		            display p_tabname
		        end foreach

                sleep 1

                message ""

end function


########################################
function ifx_SESSION(DSN_informix)
########################################
define
    DSN_informix,
    p_tabname
        char (40),
    cnt
        integer

                message "Connectiong to DSN maindb"#, DSN_informix clipped
                sleep 2


#				database _variable(DSN_informix)

		if
			not ifx_sess
        then
				OPEN SESSION s_ifmx TO DATABASE maindb as user "informix" password "ifmx"
                #cause "error compiling":
				#OPEN SESSION s_ifmx TO DATABASE _variable(DSN_informix) as user "informix" password "ifmx"
                let ifx_sess = true
        end if

                SET SESSION TO s_ifmx

				declare c4 cursor for select tabname from systables

                let cnt=0

		        foreach c4 into p_tabname
		            let cnt=cnt+1
					display cnt, " ",p_tabname
		        end foreach

#                CLOSE SESSION s_ifmx

				sleep 3

                message ""

end function


#######################################
function pg_DATABASE(DSN_postgres)
#######################################

define
    DSN_postgres,
    p_tabname
        char (40)


                message "Connectiong to DSN ", DSN_postgres clipped
                sleep 2

				database ptest #DSN_postgres

				declare c2 cursor for select typname from pg_type

		        foreach c2 into p_tabname
		            display p_tabname
		        end foreach

                sleep 1

                message ""

end function


#######################################
function pg_SESSION(DSN_postgres)
#######################################
define
    DSN_postgres,
    p_tabname
        char (40)


        if
			not pg_sess
        then

                message "Connectiong to DSN ", DSN_postgres clipped, " using db name in variable"
                sleep 2

#this works fine
                OPEN SESSION s_id1 TO DATABASE ptest as user "postgres" password "pg"

#this does not:
#				OPEN SESSION s_id1 TO DATABASE DSN_postgres as user "postgres" password "pg"

#this does not compile at all
#				OPEN SESSION s_id1 TO DATABASE _variable(DSN_postgres) as user "postgres" password "pg"

                let pg_sess = true
        end if

                SET SESSION TO s_id1


                #USE SESSION s_id1 FOR
				declare c3 cursor for select typname from pg_type

		        foreach c3 into p_tabname
		            display p_tabname
		        end foreach

                sleep 1

#                CLOSE SESSION s_id1

                message ""

end function



#######################################
function pg_SESSION_scroll(DSN_postgres)
#######################################
define
    DSN_postgres,
    p_tabname
        char (40)

                message "Connectiong to DSN ", DSN_postgres clipped
                sleep 2

        if
			not pg_sess
        then
				#OPEN SESSION s_id1 TO DATABASE DSN_informix as user "informix" password "ifmx"
                OPEN SESSION s_id1 TO DATABASE ptest as user "postgres" password "pg"
                let pg_sess = true
        end if

                SET SESSION TO s_id1


                #USE SESSION s_id1 FOR
				declare c3x scroll cursor for select typname from pg_type

		        foreach c3x into p_tabname
		            display p_tabname
		        end foreach

                sleep 2

                fetch first c3x into p_tabname
		            display "First: ", p_tabname

                fetch last c3x into p_tabname
		            display "Last: ", p_tabname

                sleep 3

#                CLOSE SESSION s_id1

                message ""

end function




