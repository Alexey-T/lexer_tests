package code
{
	/*****************************************
	 * Drawing7 :
	 * Demonstrates using the Tween and TransionMananger
	 * classes to create transition effects.
	 * -------------------
	 * See 7_transitions.fla
	 ****************************************/
	
	import fl.transitions.*;
	import fl.transitions.easing.*;
	import flash.events.MouseEvent;
	import flash.display.MovieClip;
	
	public class Drawing7 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var tween:Tween;
		
		//*************************
		// Constructor:
		
		public function Drawing7()
		{
			// Reset when clicked
			fade_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			blinds_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			fly_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			iris_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			dissolve_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			squeeze_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			wipe_btn.addEventListener(MouseEvent.CLICK, clickHandler);
			zoom_btn.addEventListener(MouseEvent.CLICK, clickHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function clickHandler(event:MouseEvent):void
		{
			switch( event.currentTarget.name )
			{
				case "fade_btn":
					
					// Fade transition using the Tween class.
					// You can use the Tween class to animate 
					// any property of the movie clip instance.
					if( tween ){
						tween.stop();
					}
					tween = new Tween(image_mc, "alpha", Strong.easeOut, 0, 1, 2, true);
					break;
					
				case "blinds_btn":
					
					// Blinds transition using the TransitionManager.
					TransitionManager.start(image_mc, {type:Blinds, direction:Transition.IN, duration:2, easing:None.easeNone, numStrips:10, dimension:0});
					break;
					
				case "fly_btn":
					// Fly transition using the TransitionManager.
					 TransitionManager.start(image_mc, {type:Fly, direction:Transition.IN, duration:3, easing:Elastic.easeOut, startPoint:9}); 
					break;
					
				case "iris_btn":
					
					// Iris transition using the TransitionManager.
					 TransitionManager.start(image_mc, {type:Iris, direction:Transition.IN, duration:2, easing:Strong.easeOut, startPoint:5, shape:Iris.CIRCLE}); 
					break;
					
				case "dissolve_btn":
					
					// Pixel dissolve transition using the TransitionManager.
					 TransitionManager.start(image_mc, {type:PixelDissolve, direction:Transition.IN, duration:2, easing:Regular.easeIn, xSections:10, ySections:10});
					break;
					
				case "squeeze_btn":
					
					// Squeeze transition using the TransitionManager.
					 TransitionManager.start(image_mc, {type:Squeeze, direction:Transition.IN, duration:2, easing:Elastic.easeOut, dimension:1});
					break;
					
				case "wipe_btn":
					
					// Wipe transition using the TransitionManager.
					 TransitionManager.start(image_mc, {type:Wipe, direction:Transition.IN, duration:2, easing:None.easeNone, startPoint:1}); 
					break;
					
				case "zoom_btn":
					
					// Zoom transition using the TransitionManager.
					 TransitionManager.start(image_mc, {type:Zoom, direction:Transition.IN, duration:2, easing:Elastic.easeOut});
					break;
			}
		}
	}
}