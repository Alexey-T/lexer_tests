// 'function' example

// Synthesizable module for a bit counter
// (accepts an 8-bit input, and reports the number of input bits
// that are set to 1)

module BitCounter (inbyte, numones);

input [7:0] inbyte;
output [3:0] numones;

reg [3:0] numones;


	// Create the output bus using a function call

always @ (inbyte)
	numones <= CountTheOnes( inbyte );


	// Declare the function

function [3:0] CountTheOnes;

	input [7:0] value;

	integer k, acc;

	begin
		acc = 0;
		for (k=0; k<8; k=k+1)
			if (value[k])
				acc = acc + 1;

		CountTheOnes = acc;
	end
endfunction

endmodule
