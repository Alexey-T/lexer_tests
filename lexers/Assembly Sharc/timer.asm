
.section/pm program;
.global _TIMER_ISR;

_TIMER_ISR:

           toggle FL1;
           rts;
_TIMER_ISR.END:           
