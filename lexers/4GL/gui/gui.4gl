define arr array[3,3] of char(1)

main
define a integer
define f integer

let arr[1,1]=" "
let arr[1,2]=" "
let arr[1,3]=" "

let arr[2,1]=" "
let arr[2,2]=" "
let arr[2,3]=" "

let arr[3,1]=" "
let arr[3,2]=" "
let arr[3,3]=" "

open form ttt from "gui" #"ttt"
display form ttt

if a4gl_get_ui_mode()=0 then
	display "Use 1,2,3 for top row" at 4,40
	display "Use 4,5,6 for middle row" at 5,40
	display "Use 7,8,9 for bottom row" at 6,40
else
	display "GUI mode... - use Ctrl-Shift-F1 for console"
	display "Use mouse to select square" at 5,40
	let f=field_to_widget(dummy)
	call UI_GTK::field_hide(f)
end if

options input wrap
input a from dummy
	before input
	message "Before Input.."
		#if a4gl_get_ui_mode()!=0 THEN
			enable  a1,a2,a3,b1,b2,b3,c1,c2,c3
		#end if

	on key('1') call set1(1,1)
	on key('2') call set1(1,2)
	on key('3') call set1(1,3)

	on key('4') call set1(2,1)
	on key('5') call set1(2,2)
	on key('6') call set1(2,3)

	on key('7') call set1(3,1)
	on key('8') call set1(3,2)
	on key('9') call set1(3,3)
end input

message "All done" wait for key

end main


function set1(x,y)
define x,y integer

	if setit(x,y,"X") then
		call mygo()
	end if
	display " " to dummy
end function


function setit(x,y,k)
define x,y integer
define k char(1)
define r char(1)

if arr[x,y]!=" " then
	error "Already Selected" wait for key
	return 0
end if

let arr[x,y]=k

if x=1 then
	if y=1 then display arr[x,y] to a1 end if
	if y=2 then display arr[x,y]  to a2 end if
	if y=3 then display arr[x,y]  to a3 end if
end if

if x=2 then
	if y=1 then display arr[x,y]  to b1 end if
	if y=2 then display arr[x,y]  to b2 end if
	if y=3 then display arr[x,y]  to b3 end if
end if
if x=3 then
	if y=1 then display arr[x,y]  to c1 end if
	if y=2 then display arr[x,y]  to c2 end if
	if y=3 then display arr[x,y]  to c3 end if
end if

let r=check_win()

if r="X" then
	message "You Win  !!!" WAIT FOR KEY
	sleep 2
	exit program
end if

if r="O" then
	message "You Lost !!!" WAIT FOR KEY
	sleep 2
	exit program
end if

if r="-" then
	message "Its a draw" WAIT FOR KEY
	sleep 2
	exit program
end if
return 1
end function




function check_win() 
define x,y integer

	if arr[1,1]=arr[1,2] and arr[1,2]=arr[1,3] and arr[1,1]!=" " then
		return arr[1,1]
	end if

	if arr[2,1]=arr[2,2] and arr[2,2]=arr[2,3] and arr[2,1]!=" " then
		return arr[2,1]
	end if

	if arr[3,1]=arr[3,2] and arr[3,2]=arr[3,3] and arr[3,1]!=" " then
		return arr[3,1]
	end if




	if arr[1,1]=arr[2,1] and arr[2,1]=arr[3,1] and arr[1,1]!=" " then
		return arr[1,1]
	end if

	if arr[1,2]=arr[2,2] and arr[2,2]=arr[3,2] and arr[2,2]!=" " then
		return arr[1,2]
	end if

	if arr[1,3]=arr[2,3] and arr[2,3]=arr[3,3] and arr[3,3]!=" " then
		return arr[3,3]
	end if



	if arr[1,1]=arr[2,2] and arr[2,2]=arr[3,3] and arr[1,1]!=" " then
		return arr[1,1]
	end if

	if arr[1,3]=arr[2,2] and arr[2,2]=arr[3,1] and arr[2,2]!=" " then
		return arr[2,2]
	end if


for x=1 to 3
for y=1 to 3
if arr[x,y]=" " then return " " end if
end for
end for
return "-"
end function


# Hows this for AI ?!
function mygo() 
define dummy integer
define x,y integer

# Look for a win
for x=1 to 3
	for y=1 to 3
		if arr[x,y]=" " then
			LET arr[x,y]="O"

			if check_win()="O" then
				LET arr[x,y]=" "
				let dummy=setit(x,y,"O")
				return
			end if
			LET arr[x,y]=" "
		end if
	end for
end for


# Look for a block
for x=1 to 3
	for y=1 to 3
		if arr[x,y]=" " then
			LET arr[x,y]="X"

			if check_win()="X" then
				LET arr[x,y]=" "
				let dummy=setit(x,y,"O")
				return
			end if

			LET arr[x,y]=" "
		end if
	end for
end for

for x=1 to 3
	for y=1 to 3
		if arr[x,y]=" " then
				let dummy=setit(x,y,"O")
				return
		end if
	end for
end for

end function

