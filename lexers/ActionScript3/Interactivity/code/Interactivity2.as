package code
{
	/*****************************************
	 * Interactivity2 :
	 * Demonstrates movement controlled by sliders.
	 * -------------------
	 * See 2_sliders.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	
	public class Interactivity2 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var xspeed:Number = 7;
		public var yspeed:Number = 7;
		
		//*************************
		// Constructor:
		
		public function Interactivity2()
		{
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Calculate rotation and speed based on position of sliders
			xspeed = horiz_slider.value;
			yspeed = vert_slider.value;
			
			beetle.rotation = Math.atan2(yspeed,xspeed)/(Math.PI/180)+90;
			
			// Move beetle
			beetle.x += xspeed;
			beetle.y += yspeed;
			
			// Loop to opposite side of the masked area 
			// when the beetle travels off-screen...
			if( beetle.y < 0 ){
				beetle.y = 195;
			}
			if( beetle.y > 195 ){
				beetle.y = 0;
			}
			if( beetle.x < 35 ){
				beetle.x = 465;
			}
			if( beetle.x > 465 ){
				beetle.x = 35;
			}
		}
	}
}