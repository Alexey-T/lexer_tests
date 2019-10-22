package code
{
	/*****************************************
	 * Interactivity7 :
	 * Demonstrates directional background scrolling.
	 * -------------------
	 * See 7_scrolling.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	import flash.display.DisplayObject;
	
	public class Interactivity7 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var initx:Number = 0;
		public var inity:Number = 0;
		public var stars:Array = new Array();
		
		//*************************
		// Constructor:
		
		public function Interactivity7()
		{
			initx = starField.x;
			inity = starField.y;
			
			// Respond to mouse events
			toggle_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			generate_btn.addEventListener(MouseEvent.CLICK,clickHandler);

			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Move starField based on the 
			// positions of the faders.
			starField.x += horiz_slider.value;
			starField.y += vert_slider.value;
				
			// Loop the starField
			if( starField.x > initx + 220 ){
				starField.x = initx;
			}
			if( starField.x < initx ){
				starField.x = initx + 220;
			}
			if( starField.y > inity + 215 ){
				starField.y = inity;
			}
			if( starField.y < inity ){
				starField.y = inity + 215;
			}
		}
		
		protected function clickHandler( event:MouseEvent ):void
		{
			switch( event.target )
			{
				case generate_btn:
					
					clearStars();
					
					// Draw star field
					for(var i:Number=1; i<=300; i+=4) 
					{
						for(var a:Number=0; a<=3; a++) 
						{
							stars[(i+a)] = new Star();
							
							if( i%7 == 0 ) {
								stars[(i+a)].scaleX = Math.random()*2;
								stars[(i+a)].scaleY = Math.random()*2;
							}
							starField.addChild(stars[(i+a)]);
						}
						stars[i].x = Math.floor(Math.random()*220);
						stars[i].y = Math.floor(Math.random()*215);
						
						stars[(i+1)].x = stars[i].x-220;
						stars[(i+1)].y = stars[i].y;
						stars[(i+2)].x = stars[i].x-220;
						stars[(i+2)].y = stars[i].y-215;
						stars[(i+3)].x = stars[i].x;
						stars[(i+3)].y = stars[i].y-215;
					}
					break;
				
				case toggle_btn:
				
					// Show or hide lines
					starField.centerlines.visible = !starField.centerlines.visible;
					break;
			}
		}
		
		//*************************
		// Public methods:
		
		public function clearStars():void
		{
			if( stars.length > 0 )
			{
				removeChild(starField);
				
				starField = new StarContainer();
				starField.x = initx;
				starField.y = inity;
				starField.mask = mask_mc;
				
				addChild(starField);
			}
			stars = new Array();
		}
	}
}