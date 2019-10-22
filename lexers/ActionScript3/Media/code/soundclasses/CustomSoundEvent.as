package code.soundclasses
{
	/***********************************
	* CustomSoundEvent class:
	* Generic event wrapper for sound event
	* notification...
	***********************************/
	
	import flash.events.Event;
	
	public class CustomSoundEvent extends Event
	{       
		/**
         * The name of the new Event type.
         */
        public static const CUSTOM_SOUND_EVENT:String = "CustomSoundEvent";

		//*****************************
		// Common Properties:
		
		public var id			:String;
		public var message		:String;
	 
	 	//******************************
		// Constructor:
		
		public function CustomSoundEvent( id:String, message:String ):void
		{
			super(CUSTOM_SOUND_EVENT);
			
			this.id = id;
			this.message = message;
		}
	 
	 	//******************************
		// Overrides:
		
		public override function clone():Event
		{
			return new CustomSoundEvent(id,message);
		}
		
		public override function toString():String
		{
			return formatToString("CustomSoundEvent", "id", "message");
		}
	} 
}