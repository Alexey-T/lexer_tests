//===== Finite State Machine Example =====

//----- Testbench -----

// Timescale: one time unit = 1ns (e.g., delay specification of #42 means 42ns of time), and
// simulator resolution is 0.1 ns
`timescale 1ns / 100ps

module FSM_Example_TESTBENCH;

// Input stimulus:
reg	i$Clock;
reg	i$Reset;
reg	i$A;
reg	i$B;
reg	i$C;

// Output connections:
wire	o$D;
wire	o$E;
wire	o$F;


//Instantiate the DUT (device under test):
FSM_Example DUT (
	// Inputs:
	.i$Clock ( i$Clock ),
	.i$Reset ( i$Reset ),
	.i$A ( i$A ),
	.i$B ( i$B ),
	.i$C ( i$C ),

	// Outputs:
	.o$D ( o$D ),
	.o$E ( o$E ),
	.o$F ( o$F )
);

	// Specify input stimulus:

initial begin

	// Initial values for input stimulus:
	i$Clock = 1'b0;
	i$Reset = 1'b1;
	i$A = 1'b0;
	i$B = 1'b0;
	i$C = 1'b0;

	// Take out of reset
	#10 i$Reset = 0;

	// Wait until state s3, wait another
	// period, then assert C
	wait (DUT.r$State==DUT.p$s3) #10 i$C = 1;

	// Deassert C
	#10 i$C = 0;

	// Assert B, then deassert
	#10 i$B = 1;
	#10 i$B = 0;

	// Assert C
	#10 i$C = 1;

	// Wait until state s3, wait another
	// period, then assert B
	wait (DUT.r$State==DUT.p$s3) #10 i$B = 1;

	// Wait until state s0 then stop
	wait (DUT.r$State==DUT.p$s0) #10 $finish;

end

	// Template for master clock. Uncomment and modify signal name as needed.
	// Remember to set the initial value of 'Clock' in the 'initial' block above.
always #5 i$Clock = ~i$Clock;


endmodule