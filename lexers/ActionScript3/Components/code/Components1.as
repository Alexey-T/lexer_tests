package code
{
	/*****************************************
	 * Components1 :
	 * Demonstrates using the FLVPlayback component and built-in
	 * skins to load and control a Flash video (FLV).
	 * -------------------
	 * See 1_video.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.text.*;
	import fl.video.*;
	
	public class Components1 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var loading_txt:TextField;
		public var loading_styles:StyleSheet;
		
		//*************************
		// Constructor:
		
		public function Components1()
		{
			// Show a loading message
			loading_styles = new StyleSheet();
			loading_styles.setStyle("p",{fontFamily:"Arial",fontSize:10,color:"#cccccc"});
			loading_txt = new TextField();
			loading_txt.styleSheet = loading_styles;
			loading_txt.htmlText = "<p>Loading...</p>";
			loading_txt.x = (stage.stageWidth-loading_txt.width)/2;
			loading_txt.y = (stage.stageHeight-20)/2;
			
			addChild(loading_txt);
			
			// Respond to video player events
			flvDisplay.addEventListener(VideoEvent.READY,readyHandler);
			flvDisplay.addEventListener(VideoEvent.COMPLETE,completeHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function readyHandler(event:VideoEvent):void
		{
			// Clear text...
			loading_txt.visible = false;
		}
		
		protected function completeHandler(event:VideoEvent):void
		{
			// Loop
			flvDisplay.play();
		}
	}
}