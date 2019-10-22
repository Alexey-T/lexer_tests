package code
{
	/*****************************************
	 * Media4 :
	 * Demonstrates loading external sounds.
	 * -------------------
	 * See 4_soundloading.fla
	 ****************************************/
	 
	import flash.display.*;
	import flash.media.Sound;
	import flash.media.SoundChannel;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.geom.Rectangle; 
	import flash.net.*;
	
	public class Media4 extends MovieClip
	{
		//*************************
		// Sound properties
			
		public var mp3Index:Number = 1;
		public var mp3Players:Array = new Array();	
		public var mp3Channel:SoundChannel;	
		public var mp3Position:Number = 0;	
		
		// Flags
		public var looping:Boolean = false;
		public var playing:Boolean = false;
		public var advance:Boolean = false;
		public var goback:Boolean = false;
		public var dragging:Boolean = false;
		
		// Assets
		public var playhilite:MovieClip;
		public var loophilite:MovieClip;
		
		//*************************
		// Constructor:
		
		public function Media4()
		{
			// Respond to mouse events
			stop_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			play_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			rewind_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			stepback_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			stepback_btn.addEventListener(MouseEvent.MOUSE_DOWN,pressHandler);
			stepforward_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			stepforward_btn.addEventListener(MouseEvent.MOUSE_DOWN,pressHandler);
			fastforward_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			loop_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			fader_btn.addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
			
			// Respond to change events
			sound_stepper.addEventListener(Event.CHANGE,changeHandler);
			
			// The stage handles drag release and releaseOutside events
			stage.addEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);
			
			// Update every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
			
			// Load
			loadSound();
		}
		
		//************************* 
		// Event Handling:
		
		// Sound loaded
		protected function loadHandler(event:Event):void
		{
			playSound(0);
		}
		
		// Sound playback completed
		protected function completeHandler(event:Event):void
		{
			if( looping ){
				playSound(0);
			}else{
				playing = false;
				playhilite.visible = false;
				mp3Channel.removeEventListener(Event.SOUND_COMPLETE,completeHandler);
			}
		}
		
		// Scrub drag
		protected function dragPressHandler(event:MouseEvent):void
		{
			// Create a rectangle to constrain the drag
			var rx:Number = track_mc.x + 1;
			var ry:Number = fader_btn.y;
			var rw:Number = track_mc.width - fader_btn.width - 1;
			var rh:Number = 0;
			var rect:Rectangle = new Rectangle(rx, ry, rw, rh);
			
			// Drag
			dragging = true;
			fader_btn.startDrag(false,rect);
		}
		
		// Scrub release
		protected function dragReleaseHandler(event:MouseEvent):void
		{
			if( dragging )
			{
				// Stop drag
				fader_btn.stopDrag();
					
				// Seek
				mp3Position = ((fader_btn.x-track_mc.x)/(track_mc.width-fader_btn.width))*mp3Players[mp3Index].length;
				stopSound();
				playSound(mp3Position);
				dragging = false;
			}
		}
		
		// Seek press
		protected function pressHandler(event:MouseEvent):void
		{	
			switch( event.target )
			{
				case stepback_btn:
					
					goback = true;
					break;
					
				case stepforward_btn:
					
					advance = true;
					break;
			}
		}
		
		// Button release
		protected function clickHandler(event:MouseEvent):void
		{
			switch( event.target )
			{
				case stop_btn:
					
					if( playing ){
						mp3Position = mp3Channel.position;
					}
					stopSound();
					break;
					
				case play_btn:
				
					if(!playing){
						playSound(mp3Position);	
					}
					break;
					
				case rewind_btn:
					
					mp3Position = 0;
					if( playing ){
						stopSound();
					}
					fader_btn.x = track_mc.x + 1;
					break;
					
				case stepback_btn:
					
					goback = false;
					break;
					
				case stepforward_btn:
					
					advance = false;
					break;
					
				case fastforward_btn:
					
					mp3Position = mp3Players[mp3Index].length;
					if( playing ){
						stopSound();
					}
					fader_btn.x = track_mc.x + (track_mc.width - fader_btn.width);
					break;
					
				case loop_btn:
					
					looping = !looping;
					
					if( looping ){
						if( loophilite == null )
						{
							loophilite = new LoopHiliteSymbol();
							loophilite.x = loop_btn.x;
							loophilite.y = loop_btn.y + 1;
							loophilite.mouseEnabled = false;
							addChild(loophilite);
						}else{
							loophilite.visible = true;
						}
					}else{
						loophilite.visible = false;
					}
					break;
			}
		}
		
		// Update every frame
		protected function enterFrameHandler(event:Event):void
		{
			if( mp3Channel == null ){
				return;
			}
			// Seek forward and back
			if( advance )
			{
				var fwdPos:Number = mp3Channel.position+100;
				if(	fwdPos < mp3Players[mp3Index].length ) {
					if( playing ) {
						mp3Position = fwdPos;
						stopSound();
						playSound(mp3Position);
					}
					else if( fader_btn.x <= (track_mc.x + (track_mc.width-fader_btn.width))){
						fader_btn.x += 1;
						mp3Position = ((fader_btn.x/(track_mc.width-fader_btn.width))*mp3Players[mp3Index].length);
					}
				}
			}
			else if( goback ) 
			{
				if( playing ) {
					mp3Position = mp3Channel.position-100;
					stopSound();
					playSound(mp3Position);
				} 
				else if( fader_btn.x > track_mc.x ) {
					fader_btn.x -= 1;
					mp3Position = ((fader_btn.x/track_mc.width)*mp3Players[mp3Index].length);
				}
			}
			// Move fader when playing
			if(!dragging && playing){
				fader_btn.x = track_mc.x+((mp3Channel.position/mp3Players[mp3Index].length)*(track_mc.width-fader_btn.width));
			}
		}
		
		protected function changeHandler(event:Event):void
		{
			mp3Index = sound_stepper.value;
			mp3Position = 0;
			
			// Load/play new sound...
			stopSound();
			loadSound();
		}
		
		//*************************
		// Public methods:
		
		public function getPath():String
		{
			return ("sounds/sound"+mp3Index+".mp3");
		}
		
		public function loadSound():void
		{
			if( mp3Players[mp3Index] == undefined ){
				mp3Players[mp3Index] = new Sound(new URLRequest(getPath()));
				mp3Players[mp3Index].addEventListener(Event.COMPLETE,loadHandler);
			}else{
				playSound(0);
			}
		}
		
		public function playSound(startTime:Number):void
		{
			if( playhilite == null ){
				playhilite = new PlayHiliteSymbol();
				playhilite.x = play_btn.x + 1;
				playhilite.y = play_btn.y + 1;
				playhilite.mouseEnabled = false;
				addChild(playhilite);
			}else{
				playhilite.visible = true;
			}
			playing = true;
			mp3Channel = mp3Players[mp3Index].play(startTime);
			mp3Channel.addEventListener(Event.SOUND_COMPLETE,completeHandler);
		}
		
		public function stopSound():void
		{
			playing = false;
			playhilite.visible = false;
			mp3Channel.stop();
			mp3Channel.removeEventListener(Event.SOUND_COMPLETE,completeHandler);
		}
	}
}