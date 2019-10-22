package code
{
	/*****************************************
	 * Interactivity5 :
	 * Demonstrates movement controlled by clicking on the stage.
	 * -------------------
	 * See 5_click.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.MovieClip;
	import flash.display.SimpleButton;
	
	public class Interactivity5 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var turnMode:Boolean = false;
		public var clickMode:Boolean = true;
		public var clickSpot_x:Number = 0;
		public var clickSpot_y:Number = 0;
		
		// Animation
		public var speed:Number = 7;
		
		//*************************
		// Constructor:
		
		public function Interactivity5()
		{
			clickSpot_x = beetle.x;
			clickSpot_y = beetle.y;
			
			curve.mouseEnabled = false;
			pointer.mouseEnabled = false;
			
			// Respond to mouse events
			follow_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			rotate_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			
			// Capture clicks on the stage
			stage.addEventListener(MouseEvent.MOUSE_DOWN,pointerHandler);
	
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function pointerHandler(event:MouseEvent):void
		{
			if( clickMode && root.mouseX > 125 && root.mouseX < 465 ){
				clickSpot_x = root.mouseX;
				clickSpot_y = root.mouseY;
				puck_mc.x = clickSpot_x;
				puck_mc.y = clickSpot_y;
			}
		}
		
		protected function clickHandler(event:MouseEvent):void
		{
			if( event.target == follow_btn ){
				clickMode = !clickMode;
			}else{
				turnMode = !turnMode;
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			// Toggle button icon visibility
			curve.visible = turnMode;
			pointer.visible = !clickMode;
			puck_mc.visible = clickMode;
			
			// Deterimine whether target spot is the 
			// clicked spot or the mouse pointer.
			var gotoSpotX:Number;
			var gotoSpotY:Number;
			
			if( clickMode ){
				gotoSpotX = clickSpot_x;
				gotoSpotY = clickSpot_y;
				puck_mc.rotation += 20;
			} else{
				gotoSpotX = root.mouseX;
				gotoSpotY = root.mouseY;
			}
			
			// Calculate angle of current 
			// position to target position.
			var delta_x:Number = beetle.x - gotoSpotX;
			var delta_y:Number = beetle.y - gotoSpotY;
			var targetRotation:Number = -Math.atan2(delta_x, delta_y)/(Math.PI/180);
			
			// Calculate the two methods of rotation
			if( turnMode ){
				if( beetle.rotation < targetRotation ){
					beetle.rotation += 10;
				}
				if( beetle.rotation > targetRotation ){
					beetle.rotation -= 10;
				}
			}else{
				beetle.rotation = targetRotation;
			}
			
			// Move beetle toward the target and 
			// stop when it gets there.
			if( Math.sqrt((delta_x*delta_x)+(delta_y*delta_y)) > speed )
			{
				beetle.y -= speed*Math.cos(beetle.rotation*(Math.PI/180));
				beetle.x += speed*Math.sin(beetle.rotation*(Math.PI/180));
			}
			
			// Loop to opposite side of the masked 
			// area when the beetle travels off-screen.
			if( beetle.y < 0 ){
				beetle.y = 232;
			}
			if( beetle.y > 232 ){
				beetle.y = 0;
			}
			if( beetle.x < 125 ){
				beetle.x = 465;
			}
			if( beetle.x > 465 ){
				beetle.x = 125;
			}
		}
	}
}