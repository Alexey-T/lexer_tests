package code.cssclasses
{
	/*****************************************
	 * CSSContainer :
	 * Creates a loader and container for a StyleSheet object. 
	 * You can use the getStyle or setStyle methods to manipulate
	 * the style sheet or simply return a reference to it using 
	 * the styleSheet property.
	 ****************************************/
	
	import flash.events.Event;
	import flash.events.IOErrorEvent;
	import flash.events.EventDispatcher;
	import flash.text.StyleSheet;
	import flash.net.URLLoader;
	import flash.net.URLRequest; 
	
	public class CSSContainer extends EventDispatcher
	{
		//*************************
		// Properties:
		
		public var loader:URLLoader;
		public var styleSheet:StyleSheet;
		
		// Public
		public var loaded:Boolean = false;
		public var url:String;
	
		//*************************
		// Constructor:
		
		public function CSSContainer():void 
		{
			// Create style sheet object
			styleSheet = new StyleSheet();
			
			// Create css loader
			loader = new URLLoader();
			loader.addEventListener(Event.COMPLETE, onLoadComplete);	
			loader.addEventListener(IOErrorEvent.IO_ERROR, onLoadError);
		}
		
		//*************************
		// Events:
		
		private function onLoadComplete(event:Event):void
		{	
			loaded = true;
			
			// Parse styles...
			styleSheet.parseCSS(event.target.data);
			
			// Relay event...
			dispatchEvent(event.clone());
		}
		
		private function onLoadError(event:IOErrorEvent):void
		{
			// Relay event...
			dispatchEvent(event.clone());
		}
		
		//*************************
		// Methods:
		
		// Load a CSS file from a url
		public function load( cssurl:String ):void
		{
			url = cssurl;
			loaded = false;
			loader.load(new URLRequest(cssurl));
		}
		
		// Return a style object (class, id, or selector)
		public function getStyle( styleName:String ):Object
		{
			return styleSheet.getStyle(styleName);
		}
		
		// Set a style object (class, id, or selector)
		public function setStyle( styleName:String, styleObj:Object ):void
		{
			styleSheet.setStyle(styleName, styleObj);
		}
	}
}