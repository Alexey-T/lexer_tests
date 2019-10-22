package code
{
	/*****************************************
	 * MediaSample2 :
	 * Demonstrates loading an external images.
	 * -------------------
	 * See 2_loadimages.fla
	 ****************************************/
	 
	import flash.events.*;
	import flash.display.*;
	import flash.net.*;
	
	public class Media2 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var source:*;
		public var loader:Loader;
		public var loaderIndex:Number = 1;
		
		//*************************
		// Constructor:
		
		public function Media2()
		{
			// Load image
			loader = new Loader();
			loader.contentLoaderInfo.addEventListener(Event.INIT,initHandler);
			addChild(loader);
			
			// Respond to stepper changes
			photo_stepper.addEventListener(Event.CHANGE,changeHandler);
			
			// Load first image...
			loadImage();
			
			// Set bevel depth
			setChildIndex(imageBevel_mc,numChildren-1);
		}
		
		//*************************
		// Event Handling:
		
		protected function initHandler(event:Event):void
		{
			source = loader.content;
			source.alpha = 0;
			source.x = workarea_mc.x;
			source.y = workarea_mc.y;
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		protected function changeHandler(event:Event):void
		{
			loaderIndex = photo_stepper.value;
			loadImage();
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			if( source.alpha < 1 ){
				source.alpha += .2;
			}else{
				removeEventListener(Event.ENTER_FRAME,enterFrameHandler);
			}
		}
		
		//*************************
		// Public methods:
		
		public function getPath():String
		{
			return ("images/image"+loaderIndex+".jpg");
		}
		
		public function loadImage():void
		{
			loader.load(new URLRequest(getPath()));
		}
	}
}