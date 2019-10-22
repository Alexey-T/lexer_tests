package code.amoebaclasses
{
	/*****************************************
	 * Shot :
	 * Creates a shooting element in the game.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.utils.getTimer;
	
	public class Shot extends MovieClip
	{
		//*************************
		// Properties:
		
		public var shotTime:Number = 0;
		public var xSpeed:Number = 0;
		public var ySpeed:Number = 0;
		
		// Reference to shot
		public var id:String;
		
		// Reference to game
		public var owner;
		
		//*************************
		// Constructor:
		
		public function Shot()
		{
			// Wait to be started...
		}
		
		//*************************
		// Lifecycle...
		
		public function live( ref:*, i:String ):void
		{
			// Set game level reference
			owner = ref;
			id = i;
			
			// Each new shot originates at 
			// the ship's center
			x = owner.ship.x;
			y = owner.ship.y;
			
			// Determine when each shot is fired
			shotTime = getTimer();
			
			// Add ship's velocity to the shot's velocity
			xSpeed = owner.shotXspeed+owner.ship.xSpeed;
			ySpeed = owner.shotYspeed+owner.ship.ySpeed;
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		public function die():void
		{
			removeEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Loop to opposite side of stage 
			// when shot goes offscreen
			owner.wrapAround(this);
			
			// Move shot
			x += xSpeed;
			y -= ySpeed;
			
			// Shot disappears after 1.5 seconds 
			// if it doesn't hit anything
			if( getTimer()-shotTime > 1500 ){
				owner.removeShot(id);
			}
		}
	}
}