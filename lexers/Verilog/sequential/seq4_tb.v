//----- Testbench -----

// Timescale: one time unit = 1ns (e.g., delay specification of #42 means 42ns of time), and
// simulator resolution is 0.1 ns
`timescale 1ns / 100ps

module Counter4_TESTBENCH;

// Input stimulus:
reg	i$Clock;
reg	i$Reset;
reg	i$InializeCount;
reg	i$CountUp;
reg	i$EnableCount;
reg	[3:0]	i$N;

// Output connections:
wire	[7:0]	o$Count;
wire	o$AtMidpoint;


//Instantiate the DUT (device under test):
Counter4 DUT (
	// Inputs:
	.i$Clock ( i$Clock ),
	.i$Reset ( i$Reset ),
	.i$InializeCount ( i$InializeCount ),
	.i$CountUp ( i$CountUp ),
	.i$EnableCount ( i$EnableCount ),
	.i$N ( i$N ),

	// Outputs:
	.o$Count ( o$Count ),
	.o$AtMidpoint ( o$AtMidpoint )
);

	// Specify input stimulus:

initial begin

	// Initial values for input stimulus:
	i$Clock = 1'b0;
	i$Reset = 1'b1;
	i$InializeCount = 1'b0;
	i$CountUp = 1'b0;
	i$EnableCount = 1'b0;
	i$N = 4'b0;

	// Take out of reset
	#10 i$Reset = 0;

	// Enable the counter
	#30 i$EnableCount = 1;
	i$N = 3;

	// Try counting counting up
	#40 i$CountUp = 1;

	// Initialize the count
	#40 i$InializeCount = 1;



	//
	//--- INSERT YOUR INPUT STIMULUS DESCRIPTION HERE ---
	//

	#100 $finish;
end

	// Template for master clock. Uncomment and modify signal name as needed.
	// Remember to set the initial value of 'Clock' in the 'initial' block above.
always #5 i$Clock = ~i$Clock;


endmodule