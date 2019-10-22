module CommentaryDemo;

//
// You can include commentary in your testbench waveform output to
// facilitate interpretation of the timing diagram.
//
// NOTE: In Silos, you need to set the radix of 'Comment' to 'ASCII',
// otherwise you will just see a long hexadecimal number.
//

// Set the maximum number of characters in the comment string
parameter MaxChars = 16;

// Create the "comment" identifier (must declare the total
// number of bits... eight bits per character)
reg [8*MaxChars-1 : 0] Comment;

// Declare a stimulus signal
reg a;


initial begin

	// Set comment to a string constant (enclosed in double quotes)
	// each time you want the comment to change in the waveform display
	Comment = "Initialize";
	a = 0;

#30	Comment = "Next Step";
	a = 1;

#30	Comment = "Finish Up";
	a = 0;

#50	$finish;

end

endmodule
