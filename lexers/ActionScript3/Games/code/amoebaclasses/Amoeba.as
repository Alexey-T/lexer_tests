package code.amoebaclasses
{
	/*****************************************
	 * Amoeba :
	 * Creates an obstacle object in the game.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.utils.getTimer;
	import flash.media.Sound;
	import flash.media.SoundChannel;
	import flash.media.SoundTransform;
	
	public class Amoeba extends MovieClip
	{
		//*************************
		// Properties:
		
		public var moveTime:Number = 0;
		public var appearTime:Number = 0;
		public var pause:Number = 0;
		public var xSpeed:Number = 0;
		public var ySpeed:Number = 0;
		public var rotateSpeed:Number = 0;
		public var xDirection:Number = 0;
		public var yDirection:Number = 0;
		public var amoebaSnd:Sound;
		public var amoebaSndChannel:SoundChannel;
		public var soundStartable:Boolean = true;
		
		// Reference to game
		public var owner;
		public var id;
		
		//*************************
		// Constructor:
		
		public function Amoeba()
		{
			// Wait to be started...
		}
		
		public function live( ref:*, i:String ):void
		{
			// Set game level reference
			owner = ref;
			id = i;
			
			// Set properties at start
			moveTime = getTimer();
			appearTime = getTimer();
			pause = Math.random()*7000+7000;
			rotateSpeed = Math.random()*8-4;
			ySpeed = 4;
			
			// Initialize sound
			amoebaSnd = new saucerSound();
			amoebaSndChannel = amoebaSnd.play();
			soundStartable = true;
					
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			if(!visible) 
			{
				// Stop sound and rest x position 
				// when amoeba is invisible
				amoebaSndChannel.stop();
				soundStartable = true;
				
				// Determine if amoeba will 
				// travel left or right
				xDirection = Math.random()*2;
				
				// Set speed
				if( xDirection == 0 ){
					xSpeed = 4;
				}
				if( xDirection == 1 ){
					xSpeed = -4;
				}
			}
			
			// Amoeba appears after pausing for the length 
			// of time generated in the Death function
			if( getTimer() - appearTime > pause ) {
				visible = true;
			}
			
			// Set state if visible
			if( visible ) 
			{
				// Check for collisions
				owner.collisions(this);
				
				// Loop to opposite side of stage 
				// when cell goes offscreen
				owner.wrapAround(this);
				
				// Play amoeba sound when amoeba is				
				// onscreen only if game is in progress
				if( owner.started && soundStartable ) 
				{
					var adjustVol:SoundTransform = new SoundTransform();
					adjustVol.volume = .5;
					amoebaSndChannel = amoebaSnd.play();
					amoebaSndChannel.soundTransform = adjustVol;
					soundStartable = false;
				}
			
				// Stop looping across stage after 30 seconds
				if( getTimer()-appearTime > 30000 && (x<0 || x>640)) {
					owner.death(this);
				}
				 
				// Move amoeba
				x += xSpeed;
				y += ySpeed;
				rotation += rotateSpeed;
			}
			
			// Change vertical direction every 2-4 seconds
			if((getTimer()-moveTime)>(Math.random()*2000+2000)) 
			{
				yDirection = Math.random()*3;
				rotateSpeed = Math.random()*8-4;
				moveTime = getTimer();
				
				if( yDirection == 0 ){
					ySpeed = 4;
				}
				if( yDirection == 1 ){
					ySpeed = -4;
				}
				if( yDirection == 2 ){
					ySpeed = 0;
				}
			}
		}
		
		//*************************
		// Public methods:
		
		// If amoeba collides with a shot other than 
		// its own, it explodes and points are scored
		
		public function hit():void
		{
			owner.splitAmoeba(this);
			owner.death(this);
		}
	}
}