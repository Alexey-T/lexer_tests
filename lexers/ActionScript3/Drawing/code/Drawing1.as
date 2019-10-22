package code
{
	/*****************************************
	 * Drawing1 :
	 * Demonstrates coordinate space & math functions.
	 * -------------------
	 * See 1_coordinates.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	import flash.geom.Rectangle;
	
	public class Drawing1 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var startX:Number;
		public var startY:Number;
		public var dragging:Boolean = false;
		
		//*************************
		// Constructor:
		
		public function Drawing1()
		{
			// Find center of drag area
			startX = puck_mc.x;
			startY = puck_mc.y;
			
			// Respond to mouse events
			reset_btn.addEventListener(MouseEvent.CLICK,resetHandler);
			puck_mc.addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
			
			// The stage handles drag release and releaseOutside events
			stage.addEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			var dx = puck_mc.x - startX;
			var dy = puck_mc.y - startY;
			
			// Calculate distance from starting point
			distance_ti.text = String(getDistance(dx,dy));
			
			// Calculate the angle from the starting point in radians
			radians_ti.text = String(getRadians(dx,dy));
			
			// Convert radians to degrees
			degrees_ti.text = String(getDegrees(getRadians(dx,dy)));
			
			// Show x and y 
			x_ti.text = String(dx);
			y_ti.text = String(dy);
			
			// Make circle continuously rotate
			puck_mc.rotation += 10;
		}
		
		protected function resetHandler(event:MouseEvent):void
		{
			// Send to start position
			puck_mc.x = startX;
			puck_mc.y = startY;
		}
		
		protected function dragPressHandler(event:MouseEvent):void
		{
			// Create a rectangle to constrain the drag
			var rx:Number = workarea_mc.x + puck_mc.width/2;
			var ry:Number = workarea_mc.y + puck_mc.height/2;
			var rw:Number = workarea_mc.width - puck_mc.width;
			var rh:Number = workarea_mc.height - puck_mc.height;
			var rect:Rectangle = new Rectangle(rx, ry, rw, rh);
			
			// Drag it!
			dragging = true;
			puck_mc.startDrag(false,rect);
		}
		
		protected function dragReleaseHandler(event:MouseEvent):void
		{
			// Stop!
			if( dragging ){
				dragging = false;
				puck_mc.stopDrag();
			}
		}
		
		//*************************
		// Math functions:
		
		public function getDistance( delta_x:Number, delta_y:Number ):Number
		{
			return Math.sqrt((delta_x*delta_x)+(delta_y*delta_y));
		}
		
		public function getRadians( delta_x:Number, delta_y:Number ):Number
		{
			var r:Number = Math.atan2(delta_y, delta_x);
			if( delta_y < 0 ){
				r += (2*Math.PI);
			}
			return r;
		}
		
		public function getDegrees( radians:Number ):Number
		{
			return Math.floor(radians/(Math.PI/180));
		}
	}
}