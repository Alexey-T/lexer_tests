package code
{
	/*****************************************
	 * Media3 :
	 * Demonstrates using scriptable masking.
	 * -------------------
	 * See 3_imagemasking.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.SimpleButton;
	import flash.display.MovieClip;
	
	public class Media3 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var enterFrameActive:Boolean = false;
		
		//*************************
		// Constructor:
		
		public function Media3()
		{
			// Respond to click events
			complex_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			dual_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			drag_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			no_mask.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			
			// Draw a frame later
			addEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Initialization:
		
		protected function draw(event:Event):void
		{
			angel.visible = false;
			circle.visible = false;
			scenery.visible = false;
			stripes1.visible = false;
			stripes2.visible = false;
			complex_mask.visible = false;
			complex_maskee.visible = false;
			
			// Stop all clips
			complex_mask.stop();
			complex_maskee.stop();
			stripes1.stop();
			stripes2.stop();
			
			// Clean up
			removeEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Event Handling:
		
		protected function clickHandler(event:MouseEvent):void
		{
			switch( event.target )
			{
				case complex_btn:
					
					// Set visibility of Masks and Maskees
					complex_mask.visible = true;
					complex_maskee.visible = true;
					circle.visible = false;
					angel.visible = false;
					scenery.visible = false;
					
					// stop dragging and move button indicator
					depressed_btn.y = complex_btn.y;
					
					// set the mask for each maskee
					complex_maskee.mask = complex_mask;
					angel.mask = null;
					scenery.mask = null;
					
					// Make the complex mask and maskee begin to play
					complex_maskee.gotoAndPlay(1);
					complex_mask.gotoAndPlay(1);
					
					// Sets the stripe masks to stop on their first frame
					stripes1.gotoAndStop(1);
					stripes2.gotoAndStop(1);
					break;
					
				case dual_btn:
					
					// Set visibility of Masks and Maskees
					angel.visible = true;
					scenery.visible = true;
					complex_mask.visible = false;
					circle.visible = false;
					complex_maskee.visible = false;
					
					// stop dragging and move button indicator
					depressed_btn.y = dual_btn.y;
					
					// setMask for each maskee
					scenery.mask = stripes1;
					angel.mask = stripes2;
					complex_maskee.mask = null;
					
					// Make the stripe masks begin to play
					stripes1.gotoAndPlay(1);
					stripes2.gotoAndPlay(1);
					
					// Sets the complex mask and complex maskee to stop on their first frame
					complex_maskee.gotoAndStop(1);
					complex_mask.gotoAndStop(1);
					break;
					
				case drag_btn:
					
					// Set visibility of Masks and Maskees
					scenery.visible = true;
					angel.visible = true;
					complex_mask.visible = false;
					circle.visible = true;
					stripes1.visible = false;
					stripes2.visible = false;
					complex_maskee.visible = false;
					
					// start dragging and move button indicator
					depressed_btn.y = drag_btn.y;
					
					// setMask for each maskee
					angel.mask = circle;
					complex_maskee.mask = null;
					scenery.mask = null;
					
					// Tells movie clips to stop on their first frame
					complex_maskee.gotoAndStop(1);
					complex_mask.gotoAndStop(1);
					stripes1.gotoAndStop(1);
					stripes2.gotoAndStop(1);
	
					// Update screen every frame
					addEventListener(Event.ENTER_FRAME,enterFrameHandler);
					break;
					
				case no_mask:
				
					// Set visibility of Masks and Maskees to false
					complex_mask.visible = false;
					stripes1.visible = false;
					stripes2.visible = false;
					circle.visible = false;
					angel.visible = false;
					scenery.visible = false;
					complex_maskee.visible = false;
					
					// stop dragging and move button indicator
					depressed_btn.y = no_mask.y;
					
					// setMask for each mask set to null
					complex_maskee.mask = null;
					angel.mask = null;
					scenery.mask = null;
					
					// Tells movie clips to stop on their first frame
					complex_maskee.gotoAndStop(1);
					complex_mask.gotoAndStop(1);
					stripes1.gotoAndStop(1);
					stripes2.gotoAndStop(1);
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			circle.rotation += 5;
			circle.x = mouseX;
			circle.y = mouseY;
		}
	}
}