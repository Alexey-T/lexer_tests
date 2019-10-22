#include <asm_sprt.h>

.extern _result;
.extern _val1;
.extern _val2;

.section/pm program;
.global _subasm;
_subasm:
    leaf_entry;           // See the Compiler manual for more details on these assembly constructs.
        ax0 = dm(_val2);
        ay0 = dm(_val1);        
        ar= ax0 - ay0;        
        dm(_result)=ar;
     
        
    leaf_exit; 
 _subasm.end: 
        
