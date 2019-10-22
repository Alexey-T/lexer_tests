package code
{
	/*****************************************
	 * Game1 :
	 * Demonstrates a maze game and user interface.
	 * -------------------
	 * See 1_maze.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.events.KeyboardEvent;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	import flash.ui.Keyboard;
	
	public class Game1 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var up:Boolean = false;
		public var down:Boolean = false;
		public var left:Boolean = false;
		public var right:Boolean = false;
		public var started:Boolean = false;
		
		//*************************
		// Constructor:
		
		public function Game1()
		{
			// Respond to mouse events
			start_btn.addEventListener(MouseEvent.CLICK,clickHandler);
																	  
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
			// Detect if edges of the player square 
			// are colliding with the maze walls.
			if( down ) {
				square.y += 1;
				if( maze.walls.hitTestPoint(square.x,square.y+square.height,true) ){
					square.y -= 1;
				}
				if( maze.walls.hitTestPoint(square.x+square.width,square.y+square.height,true) ){
					square.y -= 1;
				}
			}
			if( up ) {
				square.y -= 1;
				if( maze.walls.hitTestPoint(square.x,square.y,true) ){
					square.y += 1;
				}
				if( maze.walls.hitTestPoint(square.x+square.width,square.y,true) ){
					square.y += 1;
				}
			}
			if( left ) {
				square.x -= 1;
				if( maze.walls.hitTestPoint(square.x,square.y,true) ){
					square.x += 1;
				}
				if( maze.walls.hitTestPoint(square.x,square.y+height,true) ){
					square.x += 1;
				}
			}
			if( right ) {
				square.x += 1;
				if( maze.walls.hitTestPoint(square.x+square.width,square.y,true) ){
					square.x -= 1;
				}
				if( maze.walls.hitTestPoint(square.x+square.width,square.y+height,true) ){
					square.x -= 1;
				}
			}
			// Check to see if the player won
			if( goal.hitTestObject(square) ) {
				gotoAndStop("youwin");
			}
			// Or lost...
			if( opponent.currentFrame == opponent.totalFrames ){
				gotoAndStop("youloose");
			}
		}
	
		protected function clickHandler(event:MouseEvent):void
		{
			started = true;
			start_btn.visible = false;
			panel_mc.visible = false;
			opponent.gotoAndPlay(2);
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