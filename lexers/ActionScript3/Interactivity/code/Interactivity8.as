package code
{
	/*****************************************
	 * Interactivity8 :
	 * Demonstrates movement built with inverse kinematics.
	 * -------------------
	 * See 8_inverse_kinematics.fla
	 ****************************************/
	 
	import flash.events.*;
	import flash.display.*;
	import flash.ui.Keyboard;
	import flash.geom.Point;
	import fl.ik.*;
	
	public class Interactivity8 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var left:Boolean = false;
		public var right:Boolean = false;
		public var hookOffset:Number = 40;
		
		// Assets
		public var wire:Sprite;
		
		//*************************
		// Constructor:
		
		public function Interactivity8()
		{
			// Create wire
			wire = new Sprite();
			addChild(wire);
			
			// Listen to keyboard presses
			stage.addEventListener(KeyboardEvent.KEY_DOWN, keyPressHandler);
			stage.addEventListener(KeyboardEvent.KEY_UP, keyReleaseHandler);
			
			// Animate hook in relation to armature
			addEventListener(Event.ENTER_FRAME, enterFrameHandler);
			
			// Start in middle of animation
			gotoAndStop(35);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Update hook position
			hookOffset = Math.min(hookOffset,(390-(ikNode_3.y+ikNode_3.height))); 
			hook_mc.y = ikNode_3.y + ikNode_3.height + hookOffset;
			
			// Move crane if left or right keys are down
			if( left ){
				hook_mc.x = ikNode_3.x+3;
				hook_mc.rotation = -20;
				gotoAndStop(currentFrame-1);
			}else if( right ){
				hook_mc.x = ikNode_3.x-3;
				hook_mc.rotation = 10;
				gotoAndStop(currentFrame+1);
			}else{
				hook_mc.rotation = 0;
				hook_mc.x = ikNode_3.x;
			}
			
			// Draw wire
			wire.graphics.clear();
			wire.graphics.lineStyle(2,0x000000);
			wire.graphics.moveTo(ikNode_3.x+2,ikNode_3.y+ikNode_3.height-10);
			wire.graphics.lineTo(hook_mc.x+2,hook_mc.y);
		}
		
		protected function keyPressHandler(event:KeyboardEvent):void
		{
			switch( event.keyCode )
			{
				case Keyboard.UP:
					
					// Raise hook
					hookOffset = Math.max(hookOffset-10, 10); 
					break;
					
				case Keyboard.DOWN:
					
					// Drop the hook
					hookOffset = Math.min(hookOffset+10,(390-(ikNode_3.y+ikNode_3.height))); 
					break;
					
				case Keyboard.LEFT:
					
					// Move crane to the left
					left = true;
					break;
					
				case Keyboard.RIGHT:
				
					// Move crane to the right
					right = true;
					break;
			}
		}
		
		protected function keyReleaseHandler(event:KeyboardEvent):void
		{
			// Reset flags
			left = false;
			right = false;
		}
	}
}