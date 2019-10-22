package code.amoebaclasses
{
	/*****************************************
	 * Ship :
	 * Creates a spaceship element in the game.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.ui.Keyboard;
	import flash.media.Sound;
	import flash.media.SoundChannel;
	import flash.media.SoundTransform;
	import flash.events.KeyboardEvent;
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.utils.getTimer;
	
	public class Ship extends MovieClip
	{
		//*************************
		// Properties:
		
		public var thrust:Number = 1;
		public var decay:Number = .98;
		public var speed:Number = 0;
		public var xSpeed:Number = 0;
		public var ySpeed:Number = 0;
		public var maxSpeed:Number = 15;
		public var xThrustPercent:Number = 0;
		public var yThrustPercent:Number = 0;
		
		// Flags
		public var dead:Boolean = false;
		public var fadeIn:Boolean = false;
		public var upkeydown:Boolean = false;
		public var leftkeydown:Boolean = false;
		public var rightkeydown:Boolean = false;
		
		// Sounds
		public var thrustSnd:Sound;
		public var thrustSndChannel:SoundChannel;
		public var soundStartable:Boolean = true;
		
		// Time
		public var pauseTime:Number = 0;
		public var deathTime:Number = 0;
		
		// Reference to game
		public var owner;
		
		//*************************
		// Constructor:
		
		public function Ship()
		{
			// Ship state
			alpha = 0;
		}
		
		//*************************
		// Lifecycle...
		
		public function live( ref:* ):void
		{
			// Set game level reference
			owner = ref;
			
			// Initialize sounds
			thrustSnd = new thrustSound();
			
			// Respond to keyboard presses
			stage.addEventListener(KeyboardEvent.KEY_DOWN,keyPressHandler);
			stage.addEventListener(KeyboardEvent.KEY_UP,keyReleaseHandler);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function keyPressHandler(event:KeyboardEvent):void
		{
			switch( event.keyCode )
			{
				case Keyboard.UP:
					upkeydown = true;
					break;
					
				case Keyboard.LEFT:
					leftkeydown = true;
					break;
					
				case Keyboard.RIGHT:
					rightkeydown = true;
					break;
				
				case Keyboard.SPACE:
					
					// Fire using space key
					if(!dead && alpha > .9){
						owner.shoot(this);
					}
					break;
			}
		}
		
		protected function keyReleaseHandler(event:KeyboardEvent):void
		{
			switch( event.keyCode )
			{
				case Keyboard.UP:
					upkeydown = false;
					break;
					
				case Keyboard.LEFT:
					leftkeydown = false;
					break;
					
				case Keyboard.RIGHT:
					rightkeydown = false;
					break;
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			// Loop to opposite side of stage 
			// when ship goes offscreen
			owner.wrapAround(this);
			
			// Ship fade in at start of game or after death
			if( alpha < 1 && fadeIn ){
				flames.visible = false;
				alpha += .1;
			}else{
				fadeIn = false;
			}
		
			// Navigation controls
			if(!dead) 
			{
				if( upkeydown ) 
				{
					xSpeed += thrust*xThrustPercent;
					ySpeed += thrust*yThrustPercent;
					flames.visible = true;
					if( soundStartable ){
						thrustSndChannel = thrustSnd.play(0,99999999);
						soundStartable = false;
					}
				}else{
					xSpeed *= decay;
					ySpeed *= decay;
					if( thrustSndChannel != null ){
						thrustSndChannel.stop();
						soundStartable = true;
					}
					flames.visible = false;
				}
				if( rightkeydown ) {
					rotation += 10;
				}
				if( leftkeydown ) {
					rotation -= 10;
				}
			}
			
			// Calculate how much thrust to apply to 
			// x and y based on the rotation of the ship
			xThrustPercent = Math.sin(rotation*(Math.PI/180));
			yThrustPercent = Math.cos(rotation*(Math.PI/180));
			
			// Maintain speed limit
			speed = Math.sqrt((xSpeed*xSpeed)+(ySpeed*ySpeed));
			
			if( speed > maxSpeed ){
				xSpeed *= maxSpeed/speed;
				ySpeed *= maxSpeed/speed;
			}
			
			// Move ship based on calculations above if ship is visible
			y -= ySpeed;
			x += xSpeed;
		
			if( dead ){
				xSpeed = 0;
				ySpeed = 0;
			}
		}
		
		//****************************
		// Public methods:
		
		public function clear():void
		{
			if( thrustSndChannel != null ){
				thrustSndChannel.stop();
				soundStartable = true;
			}
		}
	}
}