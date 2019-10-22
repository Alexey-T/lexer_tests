// Testbench for 'function' example

module BitCount_TB;

reg [7:0] inbyte;
wire [3:0] numones;

BitCounter DUT (
	.inbyte(inbyte),
	.numones(numones)
);

initial begin
	inbyte = 8'b0000_0000;
#10	inbyte = 8'b0000_0001;
#10	inbyte = 8'b0000_0011;
#10	inbyte = 8'b1000_0011;
#10	inbyte = 8'b1100_0011;
#10	inbyte = 8'b1100_0111;
#10	inbyte = 8'b1110_0111;
#10	inbyte = 8'b1110_1111;
#10	inbyte = 8'b1111_1111;
#10	$finish;
end

endmodule

