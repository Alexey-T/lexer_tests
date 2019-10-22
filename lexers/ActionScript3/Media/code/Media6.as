package code
{
	/*****************************************
	 * Media6 :
	 * Demonstrates using sound generation in Flash Player 10
	 * to sample a source MP3 file and dynamically change the
	 * pitch of sound playing in the application.
	 * -------------------
	 * See 6_soundgeneration.fla
	 ****************************************/
	 
	import flash.events.*;
	import flash.display.*;
	import flash.media.Sound;
	import flash.media.SoundChannel;
	import flash.utils.ByteArray;
	import flash.net.*;
	
	public class Media6 extends MovieClip
	{
		//*************************
		// Sound properties:
			
		public var mp3Channel:SoundChannel;	// Channel containing dynamic sound
		public var mp3Player:Sound;			// Sound generated from source audio
		public var mp3Sample:Sound;			// MP3 file containing source audio
		public var mp3Samples:ByteArray;	// ByteArray containing samples at a position
		public var mp3Position:Number = 0;	
		public var frameCount:Number = 0;
		
		// Assets
		public var wave:Sprite;
	
		//*************************
		// Constructor:
		
		public function Media6()
		{
			// Wave clip
			wave = new Sprite();
			wave.x = 20;
			wave.y = 90;
			addChild(wave);
			setChildIndex(border_mc,numChildren-1);
			
			// Load sample MP3...
			mp3Samples = new ByteArray();
			mp3Sample = new Sound(new URLRequest("sounds/sound1.mp3"));
			mp3Sample.addEventListener(Event.COMPLETE, loadHandler);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		private function enterFrameHandler(event:Event):void
		{
			// Draw every other frame
			if(++frameCount % 2 != 1){ 
				return; 
			}
			mp3Samples.position = 0;
			
			if( mp3Samples.bytesAvailable )
			{
				// Remove the old wave
				wave.graphics.clear();
				wave.graphics.lineStyle(1.5, 0xaaccee);
				
				// Draw the new wave
				var len:Number = mp3Samples.length;
				for(var c:int = 0; c<len; c++)
				{
					try{
						var left:Number = mp3Samples.readFloat();
						var right:Number = mp3Samples.readFloat();
						var lineX:Number = c/(len/8/(stage.stageWidth-40));
						var lineY:Number = left*75;
						wave.graphics.lineTo(lineX, lineY);
					}catch(e){
						break;
					}
				}		
			}
		}
		
		// Sound loaded
		private function loadHandler(event:Event):void
		{
			// Create a sound and dynamically popupate it 
			// with samples from the source MP3 file
			mp3Player = new Sound();
			mp3Player.addEventListener(SampleDataEvent.SAMPLE_DATA, sampleDataHandler);
			
			// Start the sound...
			mp3Channel = mp3Player.play();
			mp3Channel.addEventListener(Event.SOUND_COMPLETE, completeHandler);
		}
		
		// Sample requested
		private function sampleDataHandler(event:SampleDataEvent):void
		{
			// Extract a sample from the source MP3 file
			var sampleLength:uint = 4096;
			mp3Samples = new ByteArray();
			mp3Position += mp3Sample.extract(mp3Samples, sampleLength, mp3Position);
			
			// Use the shiftBytes function to manipulate the sound
			event.data.writeBytes(shiftBytes(mp3Samples));
		}
		
		// Sound completed
		private function completeHandler(event:Event):void
		{
			// Seamlessly loop sound from start...
			playSound(0);
		}
		
		//*************************
		// Private methods:
		
		/**
		* This method takes a byte array (the bytes parameter) containing sound data. 
		* It returns a modified byte array, with sound samples (each representing two 
		* floating point values) removed to adjust to the pitch shift factor (the value of 
		* the pitchShiftFactor property).
		*
		* The method uses two numbers, skipCount and skipRate, to determine how frequently 
		* to remove sound samples from the byte array. The skipRate number is based on the pitch
		* shift factor (the pitchShiftFactor property). If the factor is 2.0, skipRate is set to
		* 2.0, and every second sound sample is removed. If the factor is 1.5 (3/2), skipRate is
		* set to 3.0, and every third sound sample is removed.  If the factor is 1.333 (4/3), 
		* skipRate is set to 4.0, and every fourth sound sample is removed. Removing samples
		* causes the pitch (frequency) of the sound to shift higher.
		* http://www.adobe.com/devnet/flash/articles/dynamic_sound_generation/
		*/
		private function shiftBytes(bytes:ByteArray):ByteArray
		{
			var skipCount:Number = 0;
			var skipRate:Number = 1 + (1 / (pitchShiftFactor - 1));
			var returnBytes:ByteArray = new ByteArray();
			bytes.position = 0;
			
			while(bytes.bytesAvailable > 0)
			{
				skipCount++;
				if (skipCount <= skipRate){
					returnBytes.writeFloat(bytes.readFloat());
					returnBytes.writeFloat(bytes.readFloat());
				}else{
					bytes.position += 8;
					skipCount = skipCount - skipRate;
				}
			}
			return returnBytes;
		}
		
		//*************************
		// Public methods:
		
		public function playSound(startTime:Number):void
		{
			mp3Position = startTime;
			mp3Channel = mp3Player.play(startTime);
			mp3Channel.addEventListener(Event.SOUND_COMPLETE, completeHandler);
		}
		
		public function get pitchShiftFactor():Number
		{
			return (pitchShiftFactor_sl.value/100);
		}
	}
}