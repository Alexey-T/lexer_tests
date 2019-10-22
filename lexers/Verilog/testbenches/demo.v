// Synthesizable circuit (the "device-under-test")

module Demo (a,b);
	input a;
	output b;

	assign b = ~a;

endmodule