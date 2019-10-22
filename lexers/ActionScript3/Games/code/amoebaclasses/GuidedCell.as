package code.amoebaclasses
{
	/*****************************************
	 * GuidedCell :
	 * Creates an obstacle object in the game.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.utils.getTimer;
	
	public class GuidedCell extends MovieClip
	{
		//*************************
		// Properties:
		
		public var speed:Number = 5;
		
		// Reference to game
		public var owner;
		public var id;
		
		//*************************
		// Constructor:
		
		public function GuidedCell()
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
			if( owner.ship.dead ){
				owner.removeCell(id);
				owner.explosion(this);
				return;
			}
			rotation += 10;
			
			// Move seeker
			var deltaX:Number = x-owner.ship.x;
			var deltaY:Number = y-owner.ship.y;
			var angle:Number = -Math.atan2(deltaX, deltaY);
			var ySpeed:Number = speed*Math.cos(angle);
			var xSpeed:Number = speed*Math.sin(angle);
			
			y -= ySpeed;
			x += xSpeed;
			
			// Check for collisions
			owner.collisions(this);
		}
		
		//*************************
		// Public methods:
		
		public function hit():void
		{
			x += 20;
			
			die();
			owner.explosion(this);
			owner.removeCell(id);
		}
	}
}