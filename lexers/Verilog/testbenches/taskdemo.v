module TaskDemo;

// Illustrates use of a task in a testbench.
//
// The task "CycleB" causes the signal "b" to
// oscillate for a specified number of pulses.
// For example, "CycleB(3)" will cause "b"
// to pulse three times.

reg a,b;

initial begin
	a = 0;
	CycleB ( 7 );

	#30 a = 1;
	CycleB ( 3 );

	#40 a = 0;
	CycleB ( 5 );

	#20 a = 1;

	#50 $finish;
end

task CycleB;
	input [3:0] cnt;

	integer k;

	begin
		for (k=0; k<cnt; k=k+1) begin
			#5 b=1;
			#5 b=0;
		end
	end
endtask

endmodule

