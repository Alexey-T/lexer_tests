/*-------------------------------------------------------
This project demonstrates ASM callable C function.
The test_C() function is called from ASM main.

By the time program control reaches the label "_main", the 218x
C headers will initialise the C runtime enviornment.

-------------------------------------------------------*/

#include <asm_sprt.h>


.extern _test_C;
.extern _sum;

.section/pm program;
.global _main;
_main:
    reset fl1;
	call _test_C;
	ax0 = dm(_sum);
	loop_here:	jump loop_here;
_main.END:
