/****************************************************************************
 *
 * 218x_int_tab.asm : $Revision: 1.7 $
 *
 * Copyright (c) 1999-2002 Analog Devices Inc. All rights reserved.
 *
 ****************************************************************************/

/* Example Interrupt table implementation */

#ifndef __ADSP218X__
#  define  __ADSP218X__ 1
#endif

#include <signal.h>

.extern   __lib_int_table;
.extern   __lib_int_determiner;
.extern   _____system_start;


.section/code   IVreset;
.global       ___reset;
___reset:   
      JUMP  _____system_start;
      NOP; NOP; NOP;             // Pad to four instructions for lazy LDF's

.section/code IVpwrdwn;
.global ___pwrdwn, ___lib_pwdi_ctrl;
___lib_pwdi_ctrl:
___pwrdwn:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGPWRDWN;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVirq2;
.global ___irq2, ___lib_int2_ctrl;
___lib_int2_ctrl:
___irq2:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGIRQ2;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVirql1;
.global ___irql1, ___lib_intl1_ctrl;
___lib_intl1_ctrl:
___irql1:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGIRQL1;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVirql0;
.global ___irql0, ___lib_intl0_ctrl;
___lib_intl0_ctrl:
___irql0:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGIRQL0;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVsport0xmit;
.global ___sport0xmit;
___sport0xmit:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGSPORT0XMIT;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVsport0recv;
.global ___sport0recv, ___lib_sp0x_ctrl;
___lib_sp0x_ctrl:
___sport0recv:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGSPORT0RECV;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVirqe;
.global ___irqe, ___lib_inte_ctrl;
___lib_inte_ctrl:
___irqe: 

		jump _irqe_serv;
		rti;
		rti;
		rti;  
     /* DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGIRQE;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's
    */
.section/code IVbdma;
.global ___bdma, ___lib_bdma_ctrl;
___lib_bdma_ctrl:
___bdma:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGBDMA;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVirq1;
.global ___irq1, ___lib_int1_ctrl;
___lib_int1_ctrl:
___irq1:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGIRQ1;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's

.section/code IVirq0;
.global ___irq0, ___lib_int0_ctrl;
___lib_int0_ctrl:
___irq0:   
      DM(I4+=M7)=AX1;            // Save scratch DM dag
      AX1=SIGIRQ0;
      JUMP __lib_int_determiner;
      NOP;                       // Pad to four instructions for lazy LDF's


#ifdef __ADSP2189__
.section/code IVtimer89;
#else
.section/code IVtimer;
#endif
.global ___tmri, ___lib_tmri_ctrl;
___lib_tmri_ctrl:
___tmri:   

#if defined(__EZKIT_LICENSE_RESTRICTION_21xx__) || defined(__ADSP2189__)
                                 // EZ-Kit monitor program reserves 
                                 // the timer interrupt +1 and +2 locations.
                                 // Any user code placed here will be over-
                                 // written. The +3 location can be anything 
                                 // as the monitors code at +2 executes an RTI.
                                 // To call a SIGTIMER handler, must call, not
                                 // JUMP. Not recommended because the interrupt
                                 // is likely to be saturated when used at 
                                 // high baud rates.
#ifdef __ADSP2189__
                                 // operate as VisualDSP++ 2.0 code.
      .extern ___lib_ezkit_tmri_ctrl;
      CALL ___lib_ezkit_tmri_ctrl; 
#else
      NOP; 
#endif
      NOP; NOP;                  
      RTI;
#else

      //DM(I4+=M7)=AX1;            // Save scratch DM dag
      //AX1=SIGTIMER;
      //JUMP __lib_int_determiner;
      //NOP;                       // Pad to four instructions for lazy LDF's
      NOP;
      NOP; NOP;                  
      RTI;
      
#endif

// end of file
