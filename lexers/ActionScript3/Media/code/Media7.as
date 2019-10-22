package code
{
	/*****************************************
	 * Media7 :
	 * Demonstrates the using CSS with HTML text
	 * in a Classic text field.
	 * -------------------
	 * See Media7.fla
	 ****************************************/
	
	import code.cssclasses.CSSContainer;
	import flash.display.MovieClip;
	import flash.events.Event;
	import flash.events.IOErrorEvent;
	import flash.text.AntiAliasType;
	import flash.text.TextField;
	import flash.text.TextFieldAutoSize;
	
	public class Media7 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var css:CSSContainer;
		public var cssURL:String = "helloworld.css";
		public var formattedText:String;
		
		// Assets
		public var textField:TextField;
	
		//*************************
		// Constructor:
		
		public function Media7()
		{
			// Load style sheet
			css = new CSSContainer();
			css.addEventListener(Event.COMPLETE, onCSSLoadComplete);	
			css.addEventListener(IOErrorEvent.IO_ERROR, onCSSLoadError);
			css.load(cssURL);
		}
		
		//*************************
		// Event Handling:
		
		private function onCSSLoadComplete(event:Event):void
		{
			// Create an html formatted text string
			formattedText = "<body>";
			formattedText += "<p><span class='headlineStyle'>Hello, World!</span></p>";
			formattedText += "<p>This example demonstrates how to use <span class='boldStyle'>HTML</span> text and <span class='boldStyle'>CSS</span> to format a Classic text field.</p>";
			formattedText += "<p><a href='http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/text/TextField.html?allClasses=1#htmlText' target='_self'>Click here for more details.</a></p>";
			formattedText += "</body>";
			
			// Create the TLF text field 
			textField = new TextField();
			textField.autoSize = TextFieldAutoSize.LEFT;
			textField.antiAliasType = AntiAliasType.ADVANCED;
			textField.multiline = true;
			textField.wordWrap = true;
			textField.embedFonts = true;
			textField.styleSheet = css.styleSheet;
			textField.htmlText = formattedText;
			textField.width = 400;
			textField.x = (stage.stageWidth - textField.width)/2;
			textField.y = (stage.stageHeight - textField.height)/2;

			addChild(textField);
		}
		
		private function onCSSLoadError(event:IOErrorEvent):void
		{
			trace("ERROR: File failed to load.");
		}
	}
}