//==============================================================================
// Testbench module for microcoded state machine
//==============================================================================

module MicrocodedStateMachine_TB;

reg Clock, Reset;
reg A, B, C, D;
wire X, Y, Z;

MicrocodedStateMachine DUT (
	.Clock(Clock),
	.Reset(Reset),
	.A(A), .B(B), .C(C), .D(D),
	.X(X), .Y(Y), .Z(Z)
);

initial begin
Clock = 0;
Reset = 1;
A = 1;	// begin by testing loopback in top center 
B = 1; 	// take left path first time through
C = 0; 	// pass through
D = 1;	// pass through first time when doing the right-side path

#10 Reset = 0;	// take out of reset

#20 A = 0;	// finish testing top-center loopback

wait (DUT.State == 3'b000) C = 1;	// test skip-over path

#20 wait (DUT.State == 3'b000) B = 0;	// test right-side path

#20 wait (DUT.State == 3'b000) D = 0;	// test loopback path on right side

#50 $finish;
end

always #5 Clock = ~Clock; 

// Display simulator time, and register and signal values on falling edge of clock 
initial $display("t > S : ABCD : XYZ");
always @ (negedge Clock)
	$display("%3d > %3b : %b%b%b%b : %b%b%b", $stime, DUT.State, A,B,C,D, X,Y,Z);


//==============================================================================
// End of testbench module
//==============================================================================
endmodule
