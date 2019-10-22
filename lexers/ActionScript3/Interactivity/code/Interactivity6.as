package code
{
	/*****************************************
	 * Interactivity6 :
	 * Demonstrates thrust using keyboard presses to control movement.
	 * -------------------
	 * See 6_thrust.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.KeyboardEvent;
	import flash.display.MovieClip;
	import flash.ui.Keyboard;
	
	public class Interactivity6 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var up:Boolean = false;
		public var down:Boolean = false;
		public var left:Boolean = false;
		public var right:Boolean = false;
		
		// Animation
		public var thrust:Number = 1;
		public var decay:Number = .97; 
		public var maxSpeed:Number = 15;
		public var xSpeed:Number = 0;
		public var ySpeed:Number = 0;
		public var speed:Number = 0;
		
		//*************************
		// Constructor:
		
		public function Interactivity6()
		{
			// Listen to keyboard presses
			stage.addEventListener(KeyboardEvent.KEY_DOWN,keyPressHandler);
			stage.addEventListener(KeyboardEvent.KEY_UP,keyReleaseHandler);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Rotate right or left
			if( right ) {
				beetle.rotation += 10;
			}
			if( left ) {
				beetle.rotation -= 10;
			}
			if( up ) {
				// Calculate speed and trajectory based on rotation
				xSpeed += thrust*Math.sin(beetle.rotation*(Math.PI/180));
				ySpeed += thrust*Math.cos(beetle.rotation*(Math.PI/180));
				beetle.flames.visible = true;
			} 
			else{
				// Deccelerate when Up Arrow key is released
				xSpeed *= decay;
				ySpeed *= decay;
				beetle.flames.visible = false;
			}
			
			// Maintain speed limit
			speed = Math.sqrt((xSpeed*xSpeed)+(ySpeed*ySpeed));
			if( speed > maxSpeed ){
				xSpeed *= maxSpeed/speed;
				ySpeed *= maxSpeed/speed;
			}
			
			// Move beetle based on calculations above
			beetle.y -= ySpeed;
			beetle.x += xSpeed;
			
			// Loop to opposite side of the stage 
			// when the beetle travels off-screen.
			if( beetle.y < 0 ){
				beetle.y = 232;
			}
			if( beetle.y > 232 ){
				beetle.y = 0;
			}
			if( beetle.x < 0 ){
				beetle.x = 465;
			}
			if( beetle.x > 465 ){
				beetle.x = 0;
			}
		}
		protected function keyPressHandler(event:KeyboardEvent):void
		{
			switch( event.keyCode )
			{
				case Keyboard.UP:
					up = true;
					break;
					
				case Keyboard.DOWN:
					down = true;
					break;
					
				case Keyboard.LEFT:
					left = true;
					break;
					
				case Keyboard.RIGHT:
					right = true;
					break;
			}
		}
		
		protected function keyReleaseHandler(event:KeyboardEvent):void
		{
			switch( event.keyCode )
			{
				case Keyboard.UP:
					up = false;
					break;
					
				case Keyboard.DOWN:
					down = false;
					break;
					
				case Keyboard.LEFT:
					left = false;
					break;
					
				case Keyboard.RIGHT:
					right = false;
					break;
			}
		}
	}
}