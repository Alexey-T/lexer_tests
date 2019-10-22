// Parameterizable Frequency Divider
//
// DESCRIPTION:
//
// Produces an output pulse at a lower frequency than the master clock. Suitable
// for driving a clock enable input of a register or counter (or any other device)
// at a lower frequency than the master clock. The module uses parameters to define
// the frequency division divisor and the width of the internal clock pulse counter.
//
//
// INPUT SIGNALS:
//
// Clock	- Master clock
// Reset - Master reset, active high, asynchronous (clears internal counter and
//		forces 'Out' to low state)
//
// OUTPUT SIGNALS:
//
// Out - Output pulse, one master clock period duration, active high, synchronized
//		to rising edge of master clock
//
//
// THEORY OF OPERATION:
//
// An internal up-counter is initialized to zero, then counts the rising clock 
// edges of the master clock input. When the count reaches 'Divisor - 1', the 
// output is pulsed high and the counter is reset to zero before counting again.
//
// PARAMETERS:
//
// Divisor = master clock frequency divided by desired output frequency 
//		(round to nearest integer). 'Divisor' must be at least 2.
//
// Bits = log base 2 of Divisor (use next highest integer)
//
//
// INSTANTIATION TEMPLATE:
/*
Frequency_Divider ReferenceID (
	.i_Clock( master clock ),
	.i_Reset( master reset, active high ),
	.o_Out( connection to clock enable of other device(s) )
);
defparam ReferenceID.Divisor = 10;
defparam ReferenceID.Bits = 4;
*/
//
// Ed Doering
// 29 Feb 2004
//

module Frequency_Divider (i_Clock,i_Reset,o_Out);

input i_Clock;
input i_Reset;
output o_Out;

parameter Divisor = 4;
parameter Bits = 2;

reg [Bits-1 : 0]	r_Count;
reg o_Out;

always @ (posedge i_Clock or posedge i_Reset)
	if (i_Reset) begin
		r_Count <= 0;
		o_Out <= 0;
	end
	else
	if (r_Count != Divisor - 1) begin
		r_Count <= r_Count + 1;
		o_Out <= 0;
	end
	else begin
		r_Count <= 0;
		o_Out <= 1;
	end

endmodule