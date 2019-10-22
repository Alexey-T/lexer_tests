
main
define str char(13)
define x,y,c integer
define cnt integer
define zz integer
	let str="Hello  World"
	let x=1
	open window w1 at 10,31 with 4 rows,20 columns attribute(border)
	open window w2 at 14,32 with 4 rows,20 columns attribute(border,reverse)

	sleep 1
	let y=0
	while true
		set pause mode on
		current window is w1

		display y at 1,1
		
		for x=1 to length(str)
	
			let c=x+y
			let c=c mod 2
			let c=c+2
			
			#display str[x] at 1,1 attribute(cyan,reverse)
			
			display str[x] at c,4+x attribute(cyan,reverse)
			if c=2 then display " " at 3,4+x end if
			if c=3 then display " " at 2,4+x end if
		end for

        	current window is w2
 		display current year to second at 1,1 attribute(magenta)
 		set pause mode off
 		let y=y+1

#Mike, what's wrong with 4Gl's SLEEP? if you don't like it, please remember that there is no
#usleep() on Windows - please use a4gl_usleep()
code
	a4gl_usleep(500000);
endcode
	end while

end main




