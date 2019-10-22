
.section/pm program;
.global _irqe_serv;

_irqe_serv: 

cntr = 5;
do repeat until ce;
           toggle fl1;
            cntr=0xffff;
            do here until ce;
              cntr = 0x5ff;
              do here1 until ce;
              here1: nop;
            
       here: nop;
repeat: nop;       
           
       
            
          rti;
          
   _irqe_serv.end:
