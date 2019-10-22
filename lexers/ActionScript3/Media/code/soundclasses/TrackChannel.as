package code.soundclasses
{
	/*****************************************
	 * TrackChannel :
	 * Individual track controls.
	 * -------------------
	 * See 5_soundmixing.fla
	 ****************************************/
	
	import flash.display.*;
	import flash.text.TextField;
	import flash.media.Sound;
	import flash.media.SoundChannel;
	import flash.media.SoundTransform;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.geom.Rectangle; 
	import code.soundclasses.CustomSoundEvent;
	
	public class TrackChannel extends MovieClip
	{
		//*************************
		// Properties
		
		public var level:Number = 100;
		public var soundIndex:Number = 0;;
		public var soundArray:Array;
		public var activeSound:Sound;
		public var activeChannel:SoundChannel;
		public var activeTransform:SoundTransform;
		public var pressX:Number = 0;
		public var pressRotation:Number = 0;
		public var panLevel:Number = 0;
	
		// Flags
		public var panning:Boolean = false;
		public var dragging:Boolean = false;
		public var mute:Boolean = false;
		public var solo:Boolean = false;
		
		// Mixer
		public var owner;
		
		//*************************
		// Constructor:
		
		public function TrackChannel()
		{
			super();
			
			// Default transform
			activeTransform = new SoundTransform((level/100),(panLevel/100));
			
			// Hide indicator icon
			muteIndicator.visible = false;
			muteIndicator.mouseEnabled = false;
			soloIndicator.visible = false;
			soloIndicator.mouseEnabled = false;
			
			// Set channel name field
			channel_txt.text = name;
			
			// Respond to mouse events
			mute_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			solo_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			clip1_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			clip2_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			clip3_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			clip4_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			pan_btn.addEventListener(MouseEvent.MOUSE_DOWN,panPressHandler);
			volume_btn.addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
			volume_btn.enabled = !mute;
			
			// The stage handles release and releaseOutside events
			stage.addEventListener(MouseEvent.MOUSE_UP,panReleaseHandler);
			stage.addEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);
			
			// Update screen...
			addEventListener(Event.ENTER_FRAME,draw);
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Initialization:
		
		protected function draw(event:Event):void
		{
			// Set stage components a frame later
			pan_ti.text = panLevel.toString();
			level_ti.text = level.toString();
			
			// Delete listener
			removeEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Event Handling:
		
		// Pan knob
		protected function panPressHandler(event:MouseEvent):void
		{
			if( pan_btn.enabled )
			{
				// Rotate
				pressX = root.mouseX;
				pressRotation = pan_btn.rotation;
				panning = true;
			}
		}
		
		// Pan release
		protected function panReleaseHandler(event:MouseEvent):void
		{
			if( panning ){
				panning = false;
				stopDrag();
			}
		}
		
		// Volume slider
		protected function dragPressHandler(event:MouseEvent):void
		{
			if( volume_btn.enabled )
			{
				// Create a rectangle to constrain the drag
				var rx:Number = track_mc.x + 2;
				var ry:Number = track_mc.y;
				var rw:Number = 0;
				var rh:Number = track_mc.height;
				var rect:Rectangle = new Rectangle(rx, ry, rw, rh);
				
				// Drag
				dragging = true;
				volume_btn.startDrag(false,rect);
			}
		}
		
		// Slider release
		protected function dragReleaseHandler(event:MouseEvent):void
		{
			if( dragging ){
				dragging = false;
				stopDrag();
			}
		}
		
		// Update every frame...
		protected function enterFrameHandler(event:Event):void
		{
			// Update slider volume changes
			if( dragging )
			{
				level = 100 - Math.ceil(((volume_btn.y - track_mc.y) / track_mc.height)*100);
				level_ti.text = level.toString();
			}
			// Update knob pan changes
			else if( panning )
			{
				var pivot = (root.mouseX - pressX)*(2 + pressRotation);
				pan_btn.rotation = pivot;
				
				if( pivot < -135 ) {
					pan_btn.rotation = -135;
				}
				if( pivot > 135 ) {
					pan_btn.rotation = 135;
				}
				panLevel = pan_btn.rotation/1.35;
				pan_ti.text = Math.round(panLevel).toString();
			}
			
			// Apply transformations
			if( dragging || panning ){
				update();
			}
		}
		
		// Handle button clicks
		protected function clickHandler(event:MouseEvent):void
		{
			switch( event.target )
			{
				case mute_btn:
				case muteIndicator:
					
					// Toggle
					muteTrack(!mute);
					break;
					
				case solo_btn:
				case soloIndicator:
					
					solo = !solo;
					soloIndicator.visible = solo;
					
					// Dispatch event...
					dispatchEvent(new CustomSoundEvent("TRACK_SOLO",String(level)));
					break;
					
				case clip1_btn:
					
					// Update after loop
					soundIndex = 0;
					clipIndicator.x = clip1_btn.x;
					clipIndicator.y = clip1_btn.y;
					break;
					
				case clip2_btn:
					
					// Update after loop
					soundIndex = 1;
					clipIndicator.x = clip2_btn.x;
					clipIndicator.y = clip2_btn.y;
					break;
					
				case clip3_btn:
					
					// Update after loop
					soundIndex = 2;
					clipIndicator.x = clip3_btn.x;
					clipIndicator.y = clip3_btn.y;
					break;
					
				case clip4_btn:
				
					// Update after loop
					soundIndex = 3;
					clipIndicator.x = clip4_btn.x;
					clipIndicator.y = clip4_btn.y;
					break;
			}
		}
		
		protected function completeHandler(event:Event):void
		{
			// Loop
			playTrack();
		}
		
		//*************************
		// Public methods:
		
		public function setTracks( snds:Array, par:* ):void
		{
			owner = par;
			soundArray = snds;
			soundIndex = 0;
		}
		
		public function playTrack():void
		{
			stopTrack();
			activeSound = new soundArray[soundIndex]();
			activeChannel = activeSound.play();
			activeChannel.soundTransform = activeTransform;
			activeChannel.addEventListener(Event.SOUND_COMPLETE,completeHandler);
		}
		
		public function stopTrack():void
		{
			if( activeChannel != null ){
				activeChannel.removeEventListener(Event.SOUND_COMPLETE,completeHandler);
				activeChannel.stop();
			}
		}
		
		public function muteTrack(b:Boolean):void
		{
			mute = b;
			mute_btn.enabled = !mute;
			muteIndicator.visible = mute;
			solo_btn.enabled = !mute;
			
			// Update handle position
			var yPos:Number = track_mc.y + track_mc.height
			volume_btn.enabled = !mute;
			volume_btn.y = mute ? yPos : yPos - (track_mc.height*(level/100));
			
			// Update text
			level_ti.text = mute ? "0" : level.toString();
			
			// Apply volume change
			activeTransform = new SoundTransform();
			activeTransform.volume = b ? 0 : (level/100)*(owner.mp3Level/100);
			activeTransform.pan = panLevel/100;
			activeChannel.soundTransform = activeTransform;
		}
		
		public function update():void
		{
			activeTransform = new SoundTransform((level/100)*(owner.mp3Level/100),(panLevel/100));
			if( activeChannel != null ){
				activeChannel.soundTransform = activeTransform;
			}
		}
	}
}