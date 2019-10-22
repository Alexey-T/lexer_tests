package code
{
	/*****************************************
	 * Interactivity1 :
	 * Demonstrates movement controlled by buttons.
	 * -------------------
	 * See 1_buttons.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	
	public class Interactivity1 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var up:Boolean = false;
		public var down:Boolean = false;
		public var left:Boolean = false;
		public var right:Boolean = false;
		
		// Animation
		public var mode:Boolean = true;
		public var speed:Number = 7;
		
		//*************************
		// Constructor:
		
		public function Interactivity1()
		{
			// Respond to mouse events
			up_btn.addEventListener(MouseEvent.MOUSE_DOWN,pressHandler,false,0,true);
			down_btn.addEventListener(MouseEvent.MOUSE_DOWN,pressHandler,false,0,true);
			left_btn.addEventListener(MouseEvent.MOUSE_DOWN,pressHandler,false,0,true);
			right_btn.addEventListener(MouseEvent.MOUSE_DOWN,pressHandler,false,0,true);
			up_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			down_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			left_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			right_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			toggle_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Toggle visibility of the rotate mode icon
			curve.visible = !mode;
			
			// If mode is true, the beetle always 
			// travels in a straight line.
			if( mode ) 
			{
				if( up ){
					beetle.y -= speed;
					beetle.rotation = 0;
				}
				if( down ){
					beetle.y += speed;
					beetle.rotation = 180;
				}
				if( right ){ 
					beetle.x += speed;
					beetle.rotation = 90;
				}
				if( left ){
					beetle.x -= speed;
					beetle.rotation = 270;
				}
			}
			// If mode is false the beetle turns until 
			// it reaches its target direction.
			else{
				if( up ){
					go(0);
				}
				if( down ){
					if( beetle.rotation >= 0 ){
						go(180);
					}
					if( beetle.rotation < 0 ){
						go(-180);
					}
				}
				if( right ){
					if( beetle.rotation >= 0 ){
						go(90);
					}
					if( beetle.rotation < 0 ){
						go(-270);
					}
				}
				if( left ) {
					if( beetle.rotation > 0 ){
						go(270);
					}
					if( beetle.rotation <= 0 ){
						go(-90);
					}
				}
			}
			// Loop to opposite side of the masked area 
			// when the beetle travels off-screen.
			if( beetle.y < 0 ){
				beetle.y = 231;
			}
			if( beetle.y > 231 ){
				beetle.y = 0;
			}
			if( beetle.x < 231 ){
				beetle.x = 465;
			}
			if( beetle.x > 465 ){
				beetle.x = 231;
			}
		}
		
		protected function pressHandler(event:MouseEvent):void
		{
			switch( event.target )
			{
				case up_btn:
					up = true;
					break;
					
				case down_btn:
					down = true;
					break;
					
				case left_btn:
					left = true;
					break;
					
				case right_btn:
					right = true;
					break;
			}
		}
		
		protected function clickHandler(event:MouseEvent):void
		{
			switch( event.target )
			{
				case up_btn:
					up = false;
					break;
					
				case down_btn:
					down = false;
					break;
					
				case left_btn:
					left = false;
					break;
					
				case right_btn:
					right = false;
					break;
					
				case toggle_btn:
					mode = !mode;
					break;
			}
		}
		
		//*************************
		// Public methods:
		
		public function go( degree ) 
		{
			beetle.y -= speed*Math.cos(beetle.rotation*(Math.PI/180));
			beetle.x += speed*Math.sin(beetle.rotation*(Math.PI/180));
				
			if( beetle.rotation < degree ){
				beetle.rotation += 10;
			}
			if( beetle.rotation > degree ){
				beetle.rotation -= 10;
			}
		}
	}
}