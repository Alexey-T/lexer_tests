//==============================================================================
// EE462: Advanced Digital Systems (F2003)
// 
// Microcoded State Machine -- Implementation
//
//
// The synthesizable module implements the hardware and example ASM chart from
// the "Microcoded State Machine" tutorial by L. Wilson, available in PDF form
// at http://engineering.dartmouth.edu/~engs031/hansen/demos/MicrocodedSM.pdf
//
// The testbench exercises each possible path through the ASM chart.
//
//==============================================================================

module MicrocodedStateMachine (
	Clock, Reset,	// System resources
	A, B, C, D,	// Primary inputs
	X, Y, Z		// Primary outputs
);

input Clock, Reset;
input A, B, C, D;
output X, Y, Z;

reg X, Y, Z;

reg [2:0] State;

wire [2:0] CNState;

wire [2:0] InputSelect;
wire [7:0] Inputs;

wire LoadCNState, PolarityBit, SelectedInput;

reg [9:0] ROM [0:7];
wire [9:0] ROMWord;


//==============================================================================
// State register (counter)
//==============================================================================

always @ (posedge Clock or posedge Reset)
	if (Reset)
		State <= 0;
	else
		State <= (LoadCNState == 0) ? CNState : State + 1;

//==============================================================================
// Input testing logic
//==============================================================================

// Input testing logic
assign Inputs = {4'b0,D,C,B,A};	// Adjust zero fill so "Inputs" width is 
				// always a power of 2
assign SelectedInput = Inputs[InputSelect];
assign LoadCNState = SelectedInput ^ PolarityBit;

//==============================================================================
// ROM
//==============================================================================

assign ROMWord = ROM[State];
assign {InputSelect,PolarityBit,CNState,X,Y,Z} = ROMWord;
initial begin
	ROM[3'b000] <= { 10'b_111_1_xxx_100 };
	ROM[3'b001] <= { 10'b_000_1_001_000 };
	ROM[3'b010] <= { 10'b_001_0_110_000 };
	ROM[3'b011] <= { 10'b_010_1_101_110 };
	ROM[3'b100] <= { 10'b_111_1_xxx_010 };
	ROM[3'b101] <= { 10'b_111_0_000_010 };
	ROM[3'b110] <= { 10'b_011_0_110_101 };
	ROM[3'b111] <= { 10'b_111_1_xxx_001 };
end

//==============================================================================
// End of synthesizable module
//==============================================================================
endmodule

