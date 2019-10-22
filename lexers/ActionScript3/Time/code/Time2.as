package code
{
	/*****************************************
	 * Time2 :
	 * Demonstrates using the Date object to create a clock.
	 * -------------------
	 * See 2_clock.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	
	public class Time2 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var now:Date;
		
		//*************************
		// Constructor:
		
		public function Time2()
		{
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			now = new Date();
			
			// Rotate clock hands and shadows
			hourHand_mc.rotation = now.getHours()*30+(now.getMinutes()/2);
			hourHandShadow_mc.rotation = now.getHours()*30+(now.getMinutes()/2);
			minuteHand_mc.rotation = now.getMinutes()*6+(now.getSeconds()/10);
			minuteHandShadow_mc.rotation = now.getMinutes()*6+(now.getSeconds()/10);
			secondHand_mc.rotation = now.getSeconds()*6;
			secondHandShadow_mc.rotation = now.getSeconds()*6;
		}
	}
}