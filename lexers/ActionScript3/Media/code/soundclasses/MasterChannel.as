package code.soundclasses
{
	/*****************************************
	 * MasterChannel :
	 * Master controls for all tracks.
	 * -------------------
	 * See 5_soundmixing.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.geom.Rectangle;
	import flash.text.TextField;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	import code.soundclasses.CustomSoundEvent;
	
	public class MasterChannel extends MovieClip
	{
		//*************************
		// Sound properties
		
		public var level:Number = 100;
	
		// Flags
		public var dragging:Boolean = false;
		public var mute:Boolean = false;
		
		//*************************
		// Constructor:
		
		public function MasterChannel()
		{
			// Hide indicator icon
			muteIndicator.visible = false;
			muteIndicator.mouseEnabled = false;
			muteIndicator.addEventListener(MouseEvent.CLICK,clickHandler);
			
			// Set channel name field
			channel_txt.text = name;
			
			// Respond to mouse events
			mute_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			play_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			stop_btn.addEventListener(MouseEvent.CLICK,clickHandler);
			volume_btn.addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
			volume_btn.addEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);
			volume_btn.enabled = !mute;
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,draw);
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Initialization:
		
		protected function draw(event:Event):void
		{
			// Set stage components a frame later
			level_ti.text = level.toString();
			
			// Delete listener
			removeEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Event Handling:
		
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
			volume_btn.stopDrag();
			dragging = false;
		}
		
		protected function clickHandler(event:MouseEvent):void
		{
			switch( event.target )
			{
				case mute_btn:
				case muteIndicator:
					
					// Toggle
					muteTrack(!mute);
					
					// Dispatch event...
					dispatchEvent(new CustomSoundEvent("MASTER_MUTE",String(level)));
					break;
					
				case play_btn:
					
					// Dispatch event...
					dispatchEvent(new CustomSoundEvent("MASTER_PLAY",String(level)));
					break;
					
				case stop_btn:
					
					// Dispatch event...
					dispatchEvent(new CustomSoundEvent("MASTER_STOP",String(level)));
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			if( dragging )
			{
				level = 100 - Math.ceil(((volume_btn.y - track_mc.y) / track_mc.height)*100);
				level_ti.text = level.toString();
				
				// dispatch event...
				dispatchEvent(new CustomSoundEvent("MASTER_VOLUME",String(level)));
			}
		}
		
		//*************************
		// Public methods:
		
		public function muteTrack(b:Boolean):void
		{
			mute = b;
			muteIndicator.visible = mute;
			
			// Update text
			level_ti.text = mute ? "0" : level.toString();
			
			// Update handle
			var yPos:Number = track_mc.y + track_mc.height
			volume_btn.enabled = !mute;
			volume_btn.y = mute ? yPos : yPos - (track_mc.height*(level/100));
		}
	}
}