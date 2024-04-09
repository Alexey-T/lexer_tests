/* Test: different ways to display messages */

/* Initialize RexxUtil */
call RxFuncAdd "SysLoadFuncs", "RexxUtil", "SysLoadFuncs"
call SysLoadFuncs
call SysCls

/* Send text directly to the console (CON) device */
text = "Different ways to display messages"
call SysCurPos 2,10
call lineout "\DEV\CON", text
text = "CON       Try to get rid of this message: even >NUL 2>&1 won't work"
call SysCurPos 4,0
call lineout "\DEV\CON", text

/* Send text tor standard error (STDERR) */
call SysCurPos 6,0
call lineout "\DEV\CON", "STDERR"
text = "This message will disappear with 2>NUL or >NUL 2>&1"
call SysCurPos 6,10
call lineout STDERR, text

/* Send text to standard output */
call SysCurPos 8,0
call lineout "\DEV\CON", "STDOUT"
text = "This message can be suppressed by >NUL but not by 2>NUL"
call SysCurPos 8,10
say text
