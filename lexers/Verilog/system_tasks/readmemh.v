// Illustrates how to initialize a memory array from data
// stored as hexadecimal values in a data file
//
// Simulate this file directly to see the results
//
// Note: The data file must reside in the same directory
// as the .v file for the module

module readmemh_demo;

// Declare memory array that is twelve words of 32-bits each
reg [31:0] Mem [0:11];

// Fill the memory with values taken from a data file
initial $readmemh("data.txt",Mem);

// Display the contents of memory
integer k;
initial begin
	$display("Contents of Mem after reading data file:");
	for (k=0; k<12; k=k+1) $display("%d:%h",k,Mem[k]);
end

endmodule
