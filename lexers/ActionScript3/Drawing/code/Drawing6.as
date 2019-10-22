package code
{
	/*****************************************
	 * DrawingSample6 :
	 * Demonstrates animating a pattern created with the
	 * Logo symbol in the Library and the Deco Tool.
	 * -------------------
	 * See 6_patterns.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.geom.PerspectiveProjection;
	import flash.geom.Point;
	import fl.events.SliderEvent;
	
	public class Drawing6 extends MovieClip
	{
		//*************************
		// Constructor:
		
		public function Drawing6()
		{
			// Set default field of view
			var pp:PerspectiveProjection = new PerspectiveProjection();
			pp.projectionCenter = new Point(container_mc.x, container_mc.y);
			pp.fieldOfView = 100;
			
			// Apply projector and rotation to container clip
			container_mc.transform.perspectiveProjection = pp;
			container_mc.rotationY = 45;
			
			// Setup components
			fieldOfView_sl.addEventListener(SliderEvent.CHANGE, sliderChangeHandler);
			rotationY_sl.addEventListener(SliderEvent.CHANGE, sliderChangeHandler);
			z_sl.addEventListener(SliderEvent.CHANGE, sliderChangeHandler);
			
			// Animate the pattern at the frame rate...
			addEventListener(Event.ENTER_FRAME, enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Rotate the pattern movie clip inside the container clip
			container_mc.pattern_mc.rotationZ += speed_sl.value;
		}
		
		protected function sliderChangeHandler(event:SliderEvent):void
		{
			switch( event.currentTarget.name )
			{
				case "fieldOfView_sl":
					
					// Update projector
					var pp:PerspectiveProjection = new PerspectiveProjection();
					pp.projectionCenter = new Point(container_mc.x, container_mc.y);
					pp.fieldOfView = event.currentTarget.value;
					
					// Apply projector to container clip
					container_mc.transform.perspectiveProjection = pp;
					break;
					
				case "rotationY_sl":
						
					// Apply rotation to container clip
					container_mc.rotationY = event.currentTarget.value;
					break;
					
				case "z_sl":
				
					// Move the container in z coordinate space
					container_mc.z = event.currentTarget.value;
					break;
			}
		}
	}
}