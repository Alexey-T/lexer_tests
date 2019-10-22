package code
{
	/*****************************************
	 * Media5 :
	 * Demonstrates using multiple sound channels
	 * used to create a mixing board.
	 * -------------------
	 * See 5_soundmixing.fla
	 ****************************************/
	 
	import flash.display.*;
	import flash.events.Event;
	import code.soundclasses.*;
	
	public class Media5 extends MovieClip
	{
		//*************************
		// Sound properties
			
		public var mp3s:Array = ["BASS","SYNTH","KICK","SNARE","CYMBAL","CLAP"];
		public var mp3Level:Number = 100;	
		
		// Flags
		public var playing:Boolean = false;
	
		//*************************
		// Constructor:
		
		public function Media5()
		{
			// Initialize tracks...
			MASTER.addEventListener("CustomSoundEvent",handleSoundEvent);
			
			BASS.addEventListener("CustomSoundEvent",handleSoundEvent);
			BASS.setTracks([bass1,bass2,bass3,bass4],this);
			
			SYNTH.addEventListener("CustomSoundEvent",handleSoundEvent);
			SYNTH.setTracks([synth1,synth2,synth3,synth4],this);
			
			KICK.addEventListener("CustomSoundEvent",handleSoundEvent);
			KICK.setTracks([kick1,kick2,kick3,kick4],this);
			
			SNARE.addEventListener("CustomSoundEvent",handleSoundEvent);
			SNARE.setTracks([snare1,snare2,snare3,snare4],this);
			
			CYMBAL.addEventListener("CustomSoundEvent",handleSoundEvent);
			CYMBAL.setTracks([cymbal1,cymbal2,cymbal3,cymbal4],this);
			
			CLAP.addEventListener("CustomSoundEvent",handleSoundEvent);
			CLAP.setTracks([clap1,clap2,clap3,clap4],this);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function handleSoundEvent(event:CustomSoundEvent):void
		{
			var len:Number = mp3s.length;
			var n:Number = 0; 
			var clip;
			
			switch( event.id )
			{
				case "TRACK_SOLO":
					
					// Mute all tracks but one
					for(n=0; n<len; n++){
						clip = getChildByName(mp3s[n]);
						if( clip != event.target ){
							clip.muteTrack(event.target.solo);
						}
					}
					break;
					
				case "MASTER_PLAY":
					
					// Play all tracks
					for(n=0; n<len; n++){
						clip = getChildByName(mp3s[n]);
						clip.playTrack();
					}
					playing = true;
					break;
					
				case "MASTER_STOP":
					
					// Stop all tracks
					for(n=0; n<len; n++){
						clip = getChildByName(mp3s[n]);
						clip.stopTrack();
					}
					playing = false;
					break;
					
				case "MASTER_MUTE":
					
					// Mute all tracks
					for(n=0; n<len; n++){
						clip = getChildByName(mp3s[n]);
						clip.muteTrack(event.target.mute);
					}
					break;
					
				case "MASTER_VOLUME":
				
					// Update level
					mp3Level = event.target.level;
					for(n=0; n<len; n++){
						clip = getChildByName(mp3s[n]);
						clip.update();
					}
					break;
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			if( playing )
			{
				// Update level display...
				var lftAmp:Number = 0;
				var rtAmp:Number = 0;
				var len:Number = mp3s.length;
				for(var n:Number=0; n<len; n++)
				{
					var clip = getChildByName(mp3s[n]);
					lftAmp = Math.max(lftAmp,clip.activeChannel.leftPeak);
					rtAmp = Math.max(rtAmp,clip.activeChannel.rightPeak);
				}
				leftPeak_mc.scaleY = lftAmp;
				rightPeak_mc.scaleY = rtAmp;
			}
			else{
				leftPeak_mc.scaleY = 0;
				rightPeak_mc.scaleY = 0;
			}
		}
	}
}