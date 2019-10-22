package code
{
	/*****************************************
	 * Drawing2 :
	 * Demonstrates timeline nesting using movie clips.
	 * -------------------
	 * See 2_timelines.fla
	 ****************************************/
	 
	import flash.display.MovieClip;
	
	public class Drawing2 extends MovieClip
	{
		//*************************
		// Constructor:
		
		public function Drawing2()
		{
			// The following code associates the various timelines
			// so that you see the differences in timeline coordinate space
			// as each small rectangle is dragged around the Stage...
			
			// Left controller
			leftController.dragButton.target = squareGroup.outerSquare;
			
			// Middle controller
			midController.dragButton.target = squareGroup.outerSquare.midSquare;
			
			// Right controller
			rightController.dragButton.target = squareGroup.outerSquare.midSquare.smallSquare.dragButton;
			
			// Bottom left controller
			squareGroup.outerSquare.midSquare.smallSquare.dragButton.target = squareGroup.outerSquare.midSquare2.smallSquare.dragButton;
		}
	}
}