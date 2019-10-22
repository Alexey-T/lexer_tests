package code
{
	/*****************************************
	 * Drawing2 :
	 * Demonstrates timeline nesting using movie clips.
	 * -------------------
	 * See 2_timelines.fla
	 ****************************************/
	 
	import flash.events.*;
	import flash.display.*;
	import flash.geom.Rectangle;
	
	public class DraggableClip extends Sprite
	{
		//*************************
		// Properties:
		
		private var _dragging:Boolean = false;
		private var _offset:Number = 15;
		private var _startX:Number = 0;
		private var _startY:Number = 0;
		
		// Reference to timeline in display list...
		private var _target:DisplayObject;
		
		//*************************
		// Constructor:
		
		public function DraggableClip()
		{
			_startX = x;
			_startY = y;
			
			// Update screen every frame
			addEventListener(MouseEvent.MOUSE_DOWN, dragPressHandler);
			
			// The stage handles drag release and releaseOutside events
			stage.addEventListener(MouseEvent.MOUSE_UP, dragReleaseHandler);
		}
		
		//*************************
		// Event Handling:
		
		private function dragPressHandler(event:MouseEvent):void
		{
			// Create a rectangle to constrain the drag
			var rx:Number = _startX - _offset;
			var ry:Number = _startY - _offset;
			var rw:Number = (_startX + _offset) - rx;
			var rh:Number = (_startY + _offset) - ry;
			var rect:Rectangle = new Rectangle(rx, ry, rw, rh);
			
			// Start drag
			_dragging = true;
			startDrag(false, rect);
			addEventListener(Event.ENTER_FRAME, enterFrameHandler);
		}
		
		private function dragReleaseHandler(event:MouseEvent):void
		{
			// Stop drag
			if( _dragging )
			{
				stopDrag();
				removeEventListener(Event.ENTER_FRAME, enterFrameHandler);
			}
		}
		
		private function enterFrameHandler(event:Event):void
		{
			// Update the location of the related target clip.
			// This demonstrates the differences in coordinate
			// space in the different timelines.
			if( _target != null ){
				_target.x = x;
				_target.y = y;
			}
		}
		
		//*************************
		// Event Handling:
		
		public function set target(t:DisplayObject):void
		{
			_target = t;
		}
		
		public function get target():DisplayObject
		{
			return _target;
		}
	}
}