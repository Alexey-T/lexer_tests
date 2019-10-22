package code
{
	/*****************************************
	 * Game2 (Amoebas) :
	 * Demonstrates a spaceship vs amoebas game
	 * involving collision and shooting. This class
	 * wraps the player layer where most of the game
	 * functionality lies. The user interface, game
	 * controls, and elements that need to appear
	 * above the players go here...
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.text.TextField;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	
	public class Game2 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var bonusLifeScore:Number = 2000;
		public var score:Number = 0;
		public var lives:Number = 0;
		public var level:Number = 6;
		
		// Flags
		public var buttonFadeOut:Boolean = false;
		
		//*************************
		// Constructor:
		
		public function Game2()
		{
			// Initialize player level
			players.radius = players.ship.body.height/2;
			players.radius2 = circle.width/2;
			players.centerx = circle.x;
			players.centery = circle.y;
			players.live(this);
			
			// Respond to button events
			start_btn.addEventListener(MouseEvent.CLICK,clickHandler);

			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function clickHandler( event:MouseEvent ):void
		{
			switch( event.target )
			{
				case start_btn:
				
					initiate();
					break;
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			// Add bonus life every 2000 points
			if( score > bonusLifeScore ){
				bonusLifeScore += 2000;
				lives += 1;
			}
			
			// Button fade in and out
			with( start_btn ){
				if( lives == 0 )
				{
					if( alpha < 1 ){
						alpha += .1;
						mouseEnabled = true;
					}
				}
				if( alpha > 0 && buttonFadeOut ) {
					alpha -= .1;
					mouseEnabled = false;
				}else{
					buttonFadeOut = false;
				}
			}
			
			// Update text
			score_txt.text = String(score);
			lives_txt.text = String(lives);
		}
		
		//*************************
		// Public methods:
		
		public function initiate():void
		{
			// Set defaults
			score = 0;
			level = 2;
			lives = 3;
			buttonFadeOut = true;
			
			// Set players
			players.initiate();
		}
	}
}