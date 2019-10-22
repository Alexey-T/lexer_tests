// Testbench for "Demo" module

module Demo_TB;
	reg astim;	// stimulus for port "a"
	wire bmon;	// connection to monitor port "b"

	// Instantiate the device-under-test
	Demo DUT (
		.a(astim),
		.b(bmon)
	);

	// Apply input stimulus
	initial begin
		astim = 0;
	#10	astim = 1;
	#30	astim = 0;
	#20	$finish;
	end

endmodule