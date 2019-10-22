// Parameterizable Frequency Divider -- Testbench
//
// Ed Doering
// 29 Feb 2004
//

module Frequency_Divider_TB;

reg Clock;
reg Reset;
wire Out;

// Instantiate the device
Frequency_Divider DUT (
	.i_Clock ( Clock ),
	.i_Reset ( Reset ),
	.o_Out ( Out )
);
defparam DUT.Divisor = 10;
defparam DUT.Bits = 4;

// Emulate the master clock oscillator
always #5 Clock = ~Clock;

// Emulate the power-on reset, and initialize the clock
initial begin
	Clock = 0;
	Reset = 1;
	#10 Reset = 0;
end

// Evaluate the response
integer TotalCycles,PreviousCount,TwoPulses;
initial begin TotalCycles=0; PreviousCount=0; TwoPulses=0; end
always @ (posedge Clock)
	TotalCycles = TotalCycles+1;
	
always @ (negedge Clock)
	if (Out == 1) begin
		TwoPulses = TwoPulses + 1;
		if (TwoPulses == 2) begin
			$display("%d clock cycles counted between two successive output pulses",
				TotalCycles-PreviousCount);
			$display("%d cycels expected",DUT.Divisor);
			if (TotalCycles-PreviousCount == DUT.Divisor)
				$display("CORRECT OPERATION VERIFIED.");
			else
				$display("INCORRECT OPERATION DETECTED!!");
			#10 $stop;
		end
		PreviousCount = TotalCycles;
	end

endmodule