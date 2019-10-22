package code
{
	/*****************************************
	 * Media9 :
	 * Demonstrates the using TLF markup in an XML 
	 * file and CSS to format text in a TLF text field.
	 * -------------------
	 * See Media9.fla
	 ****************************************/
	
	import code.cssclasses.CSSContainer;
	import code.cssclasses.CSSFormatResolver;
	import fl.text.TLFTextField;
	import flash.display.MovieClip;
	import flash.events.Event;
	import flash.events.IOErrorEvent;
	import flash.net.URLLoader;
	import flash.net.URLRequest;
	import flash.text.AntiAliasType;
	import flash.text.TextFieldAutoSize;
	
	public class Media9 extends MovieClip
	{
		//*************************
		// Properties:
			
		public var css:CSSContainer;
		public var cssURL:String = "helloworld.css";
		public var textLoader:URLLoader;
		public var textMarkup:String;
		public var textURL:String = "helloworld.xml";
		
		// Assets
		public var textField:TLFTextField;
	
		//*************************
		// Constructor:
		
		public function Media9()
		{
			// Load the TLF markup text file
			textLoader = new URLLoader();
			textLoader.addEventListener(Event.COMPLETE, onXMLLoadComplete);	
			textLoader.addEventListener(IOErrorEvent.IO_ERROR, onLoadError);
			textLoader.load(new URLRequest(textURL));
		}
		
		//*************************
		// Event Handling:
		
		private function onXMLLoadComplete(event:Event):void
		{
			textMarkup = textLoader.data; // Leave markup as String / don't convert to XML.
			
			// Next load style sheet...
			css = new CSSContainer();
			css.addEventListener(Event.COMPLETE, onCSSLoadComplete);	
			css.addEventListener(IOErrorEvent.IO_ERROR, onLoadError);
			css.load(cssURL);
		}
		
		private function onCSSLoadComplete(event:Event):void
		{
			// Create the TLF text field 
			textField = new TLFTextField();
			textField.autoSize = TextFieldAutoSize.RIGHT;
			textField.antiAliasType = AntiAliasType.ADVANCED;
			textField.multiline = true;
			textField.wordWrap = true;
			textField.embedFonts = true;
			textField.width = 400;
			
			addChild(textField);
			
			// Update text source and formatting...
			textField.tlfMarkup = textMarkup;
			textField.textFlow.formatResolver = new CSSFormatResolver(css.styleSheet);
			textField.textFlow.flowComposer.updateAllControllers();
			
			// Position after formatting...
			textField.x = (stage.stageWidth - textField.width) / 2;
			textField.y = (stage.stageHeight - textField.height) / 2;
		}
		
		private function onLoadError(event:IOErrorEvent):void
		{
			trace("ERROR: File failed to load.");
		}
	}
}