package code
{
	/*****************************************
	 * Media8 :
	 * Demonstrates the using CSS with HTML text
	 * in a TLF text field.
	 * -------------------
	 * See Media8.fla
	 ****************************************/
	
	import code.cssclasses.CSSContainer;
	import code.cssclasses.CSSFormatResolver;
	import fl.text.TLFTextField;
	import flash.display.MovieClip;
	import flash.events.Event;
	import flash.events.IOErrorEvent;
	import flash.text.AntiAliasType;
	import flash.text.TextFieldAutoSize;
	
	public class Media8 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var css:CSSContainer;
		public var cssURL:String = "helloworld.css";
		public var formattedText:String;
		
		// Assets
		public var textField:TLFTextField;
	
		//*************************
		// Constructor:
		
		public function Media8()
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
			// Create html formatted text string
			formattedText = "<body>";
			formattedText += "<p><span class='headlineStyle'>Hello, World!</span></p>";
			formattedText += "<p>This example demonstrates how to use <span class='boldStyle'>HTML</span> text and <span class='boldStyle'>CSS</span> to format a TLF text field.</p>";
			formattedText += "<p><a href='http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/fl/text/TLFTextField.html?allClasses=1#htmlText' target='_self'>Click here for more details.</a></p>";
			formattedText += "</body>";
			
			// Create the TLF text field 
			textField = new TLFTextField();
			textField.autoSize = TextFieldAutoSize.LEFT;
			textField.antiAliasType = AntiAliasType.ADVANCED;
			textField.multiline = true;
			textField.wordWrap = true;
			textField.embedFonts = true;
			textField.htmlText = formattedText;
			textField.width = 400;
			
			addChild(textField);
			
			// Update text source and formatting...
			textField.htmlText = formattedText;
			textField.textFlow.formatResolver = new CSSFormatResolver(css.styleSheet);
			textField.textFlow.flowComposer.updateAllControllers();
			
			// Position after formatting...
			textField.x = (stage.stageWidth - textField.width) / 2;
			textField.y = (stage.stageHeight - textField.height) / 2;
		}
		
		private function onCSSLoadError(event:IOErrorEvent):void
		{
			trace("ERROR: File failed to load.");
		}
	}
}