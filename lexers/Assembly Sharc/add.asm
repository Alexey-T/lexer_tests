
#include"def2181.h"

.section/pm program;

.global add_func;

add_func:

AX1 = I4;  // I4 is the stack pointer & arguments are stored in stack

I6 = AX1;  // Retrive pointer to argument list

M5 =1;
M3 =1;

MODIFY(I6,M5);

AX0 = DM(I6,M5);  // Retrive 1st argument
AY0 = DM(I6,M5);  // Retrive 2nd argument

MR0 = DM(I6,M5);  // Retrive 3rd argument- pointer to result array

I0 = MR0;         // I0 points to result array

AR = AX0 + AY0;

DM(I0,M3) = AR;   // Store add result

AR = AX0 - AY0;

DM(I0,M3) = AR;   // Store sub result

MX0 = AX0;
MY0 = AY0;
 
MR = MX0*MY0(UU);

DM(I0,M3) = MR0;   // Store MULTIPLY result

RTS;
add_func.END:

