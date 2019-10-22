#include <asm_sprt.h>


.extern _val1;
.extern _val2;
.extern _result;

.global _addasm;
.section/pm program;

 _addasm:
      leaf_entry;  // please refer the compiler manual     
 		  ax0 = dm(_val1);  // to know about these macros
 		  ay0 = dm(_val2);
          ar=ax0 + ay0;
          dm(_result) = ar;
       
    leaf_exit; 
 _addasm.end:       
