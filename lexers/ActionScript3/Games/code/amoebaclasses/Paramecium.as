package code.amoebaclasses
{
	/*****************************************
	 * Paramecium :
	 * Creates an obstacle object in the game.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.utils.getTimer;
	import flash.text.TextField;
	import flash.display.MovieClip;
	
	public class Paramecium extends MovieClip
	{
		//*************************
		// Properties:
		
		public var moveTime:Number = 0;
		public var direction:Number = 0;
		public var speed:Number = 0;
		public var up:Boolean = false;
		public var down:Boolean = false;
		public var left:Boolean = false;
		public var right:Boolean = false;
		
		// Game index for 
		// this instance...
		public var id:String;
		
		// Game level
		public var owner;
		
		//*************************
		// Constructor:
		
		public function Paramecium()
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
			
			// Set properties at start up
			var angle:Number = Math.random()*360;
			x = Math.sin(angle*(Math.PI/180))*(owner.radius2+10)+owner.centerx;
			y = Math.cos(angle*(Math.PI/180))*(owner.radius2+10)+owner.centery;
			moveTime = getTimer();
			speed = Math.random()*4+4;
			direction = Math.round(Math.random()*9);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		public function die():void
		{
			// do stop routine...
			removeEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Handle movement
			if( up && left ){
				go(-45);
			}else if( up && right ){
				go(45);
			}else if( down && left ){
				go(-135);
			}else if( down && right ){
				go(135);
			}else if( right && !down && !up ){
				go(90);
			}else if( left && !down && !up ){
				go(-90);
			}else if( down && !right && !left ){
				go(180);
			}else if( up && !right && !left ){
				go(0);
			}
			// Calculate next move
			if( direction == 0 ){
				up = true;
				down = false;
				right = false;
				left = false;
			}
			if( direction == 1 ){
				up = false;
				down = true;
				right = false;
				left = false;
			}
			if( direction == 2 ){
				up = false;
				down = false;
				right = true;
				left = false;
			}
			if( direction == 3 ){
				up = false;
				down = false;
				right = false;
				left = true;
			}
			if( direction == 4 ){
				up = true;
				down = false;
				right = true;
				left = false;
			}
			if( direction == 5 ){
				up = true;
				down = false;
				right = false;
				left = true;
			}
			if( direction == 6 ){
				up = false;
				down = true;
				right = true;
				left = false;
			}
			if( direction == 7 ){
				up = false;
				down = true;
				right = false;
				left = true;
			}
			if( direction == 8 ){
				up = false;
				down = false;
				right = false;
				left = false;
			}
			
			// Check for collisions
			owner.collisions(this);
			
			// Handle offscreen movement
			owner.wrapAround(this);
			 
			// Change vertical direction every 2-4 seconds
			if((getTimer()-moveTime)>(Math.random()*2000+2000))
			{
				direction = Math.round(Math.random()*9);
				moveTime = getTimer();
			}
		}
		
		//*************************
		// Public methods:
		
		public function hit():void
		{
			x += 20;
			
			// If paramecium collides with any shot, 
			// it explodes and disappears
			owner.explosion(this);
			owner.removeCell(id);
		}
		
		public function go( degree ):void
		{
			y -= speed*Math.cos(rotation*(Math.PI/180));
			x += speed*Math.sin(rotation*(Math.PI/180));
			
			if( rotation < degree ){
				rotation += 15;
			}
			if( rotation > degree ){
				rotation -= 15;
			}
		}
	}
}