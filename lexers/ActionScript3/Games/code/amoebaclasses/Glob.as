package code.amoebaclasses
{
	/*****************************************
	 * Glob :
	 * Creates an obstacle object in the game.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	
	public class Glob extends MovieClip
	{
		//*************************
		// Properties:
		
		public var speed:Number = 0;
		public var xSpeed:Number = 0;
		public var ySpeed:Number = 0;
		public var rotateSpeed:Number = 0;
		
		// Reference to game
		public var owner;
		public var id;
		
		//*************************
		// Constructor:
		
		public function Glob()
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
			
			// Set random values
			var angle:Number = Math.random()*(2*Math.PI);
			speed = Math.random()*5+1;
			xSpeed = Math.sin(angle)*speed;
			ySpeed = Math.cos(angle)*speed;
			rotateSpeed = Math.random()*8-4;
			
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
			// Check for collisions
			owner.collisions(this);
			
			// Loop to opposite side of stage 
			// when cell goes offscreen
			owner.wrapAround(this);
			
			// Move and rotate cell
			x += xSpeed;
			y += ySpeed;
			rotation += rotateSpeed;
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