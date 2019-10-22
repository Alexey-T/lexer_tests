

.section/pm interrupts;

_reset: JUMP start; nop; nop; nop;      	    /* 0x0000: Reset vector*/
	RTI; nop; nop; nop;							/* 0x0004: IRQ2*/
	RTI; nop; nop; nop;							/* 0x0008: IRQL1*/
	RTI; nop; nop; nop;							/* 0x000C: IRQL0*/
    RTI; nop; nop; nop;				            /* 0x0010: SPORT0 transmit*/
	RTI; nop; nop; nop;							/* 0x0014: SPORT0 receive*/
	RTI; nop; nop; nop;					        /* 0x0018: IRQE*/
	RTI; nop; nop; nop;							/* 0x001C: BDMA*/
	RTI; nop; nop; nop;							/* 0x0020: SPORT1 transmit*/
	RTI; nop; nop; nop;							/* 0x0024: SPORT1 receive*/
	nop; nop; nop; rti;							/* 0x0028: Timer*/
	RTI; nop; nop; nop;							/* 0x002C: Power down*/

.section/pm program;

start:

	imask=0x001;
	ena timer;
	
	
again:   toggle fl1;

	cntr = 0x5ff;
	
	do loop1 until ce;
	
	   cntr = 0xfff;
	   
	      do loop2 until ce;
	      nop;
	      loop2: nop;
	      
	loop1: nop;
	
	jump again;
	
		
