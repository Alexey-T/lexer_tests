package code
{
	/*****************************************
	 * Drawing3 :
	 * Demonstrates using the drawing API to 
	 * dynamically create graphics.
	 * -------------------
	 * See 3_graphics.fla
	 ****************************************/
	 
	import flash.events.*;
	import flash.display.*;
	import flash.ui.Keyboard;
	import flash.geom.Rectangle;
	import fl.events.SliderEvent;
	
	public class Drawing3 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var handles:Array;
		public var points:Array;
		public var pointsTotal:Number = 6;
		public var radius:Number = 100;
		public var angle:Number = 0;
		public var angle2:Number = 0;
		public var xPos:Number;
		public var yPos:Number;
		
		// Color offsets
		public var c1:Number = 0;		// R
		public var c2:Number = 128;		// G
		public var c3:Number = 255;		// B
		public var c4:Number = 100;		// alpha
		
		// Line color offsets
		public var lc1:Number = 0;		// R
		public var lc2:Number = 0;		// G
		public var lc3:Number = 0;		// B
		public var lc4:Number = 100;	// alpha
		
		// Flags
		public var mode:Boolean = true;
		public var dragging:Boolean = true;
		public var colorMode:Boolean = true;
		
		// Containers
		public var drawArea:Sprite;
		public var tangentArea:Sprite;
		public var drawingMask:MovieClip;
		
		//*************************
		// Constructor:
		
		public function Drawing3()
		{
			// Respond to mouse events
			redraw_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			linemode_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			colormode_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			twirlfwd_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			twirlbck_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			
			// Respond to drag events
			ptradius_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			handleradius_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			red_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			green_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			blue_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			opacity_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			
			// Respond to change events
			input_stepper.addEventListener(Event.CHANGE,changeHandler);
	
			// Respond to frame events
			addEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Initialization:
		
		public function draw(event:Event):void
		{
			// Container for drawing
			tangentArea = new Sprite();
			drawArea = new Sprite();
			addChild(drawArea);
			addChild(tangentArea);
			
			// Set icons
			colorModeIndicator.mouseEnabled = false;
			modeIndicator.mouseEnabled = false;
			
			// Initialize points
			xPos = workarea_mc.x + (workarea_mc.width/2);
			yPos = workarea_mc.y + (workarea_mc.height/2);
			initialize();
			
			// Handle listeners...
			removeEventListener(Event.ENTER_FRAME,draw);
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Public methods:
		
		public function initialize():void
		{
			clear();
			pointsTotal = input_stepper.value;
			angle = 0;
			
			for(var i:Number=0; i<pointsTotal; i++) 
			{
				// Place handle
				handles[i] = new Handle();
				handles[i].x = Math.sin((angle+(360/(pointsTotal*2)))*(Math.PI/180))*radius+xPos;
				handles[i].y = Math.cos((angle+(360/(pointsTotal*2)))*(Math.PI/180))*radius+yPos;
				handles[i].addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
				addChild(handles[i]);
				
				// Place center point
				points[i] = new CenterPoint();
				points[i].x = (Math.sin(angle*(Math.PI/180))*radius/2)+xPos;
				points[i].y = (Math.cos(angle*(Math.PI/180))*radius/2)+yPos;
				points[i].addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
				addChild(points[i]);
				
				// Increment angle
				angle += 360/pointsTotal;
			}
			// The stage handles release and releaseOutside events
			stage.addEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);
		}
		
		public function clear():void
		{
			if( handles != null )
			{
				var len:Number = handles.length;
				for(var i:Number=0; i < len; i++){
					removeChild(handles[i]);
					removeChild(points[i]);
					delete handles[i];
					delete points[i];
				}
			}
			points = new Array();
			handles = new Array();
		}
		
		//*************************
		// Event Handling:
		
		protected function dragPressHandler( event:MouseEvent ):void
		{
			// Create a rectangle to constrain the drag
			var rx:Number = workarea_mc.x + event.target.width/2;
			var ry:Number = workarea_mc.y + event.target.height/2;
			var rw:Number = workarea_mc.width - event.target.width;
			var rh:Number = workarea_mc.height - event.target.height;
			var rect:Rectangle = new Rectangle(rx, ry, rw, rh);
			
			// Drag
			dragging = true;
			event.target.startDrag(false,rect);
		}
		
		protected function dragReleaseHandler( event:MouseEvent ):void
		{
			if( dragging ){
				dragging = false;
				stopDrag();
			}
		}
		
		protected function changeHandler( event:Event ):void
		{
			initialize();
		}
		
		protected function clickHandler( event:MouseEvent ):void
		{
			switch( event.target )
			{
				case redraw_btn:
					
					// Reset variables
					c1 = 0;
					c2 = 128;
					c3 = 255;
					c4 = 100;
					lc1 = 0;
					lc2 = 0;
					lc3 = 0;
					lc4 = 100;
					mode = true;
					colorMode = true;
					
					// Reset components
					ptradius_slider.value = 50;
					handleradius_slider.value = 100;
					red_slider.value = c1;
					green_slider.value = c2;
					blue_slider.value = c3;
					opacity_slider.value = c4 * 100;
					input_stepper.value = 6;
					weight_stepper.value = 1;
					modeIndicator.gotoAndStop(1);
					colorModeIndicator.gotoAndStop(1);
					twirlbck_btn.enabled = mode;
					twirlbck_btn.alpha = mode ? 1 : 0.5;
					twirlfwd_btn.enabled = mode;
					twirlfwd_btn.alpha = mode ? 1 : 0.5;
					handleradius_slider.enabled = mode;
					handleradius_slider.alpha = mode ? 1 : 0.5;
					
					// Draw screen
					initialize();
					break;
					
				case linemode_btn:
				
					// Adjust curve mode
					mode = !mode;
					modeIndicator.gotoAndStop((mode ? 1 : 2));
					
					// Show or hide handles
					for (var i:Number=0; i<pointsTotal; i++) {
						handles[i].visible = mode;
					}
					tangentArea.visible = mode;
					
					// Control state
					twirlbck_btn.enabled = mode;
					twirlbck_btn.alpha = mode ? 1 : 0.5;
					twirlfwd_btn.enabled = mode;
					twirlfwd_btn.alpha = mode ? 1 : 0.5;
					handleradius_slider.enabled = mode;
					handleradius_slider.alpha = mode ? 1 : 0.5;
					break;
					
				case colormode_btn:
					
					// Adjust color mode
					colorMode = !colorMode;
					
					if( colorMode ){
						red_slider.value = c1;
						green_slider.value = c2;
						blue_slider.value = c3;
						opacity_slider.value = c4 * 100;
					}else{
						red_slider.value = lc1;
						green_slider.value = lc2;
						blue_slider.value = lc3;
						opacity_slider.value = lc4 * 100;
					}
					colorModeIndicator.gotoAndStop((colorMode ? 1 : 2));
					break;
					
				case twirlbck_btn:
			
					// Adjust for twirling
					for(i=0; i<pointsTotal; i++){
						angle += .5;
						handles[i].x = Math.sin((angle+(360/(pointsTotal*2)))*(Math.PI/180))*handleradius_slider.value+xPos;
						handles[i].y = Math.cos((angle+(360/(pointsTotal*2)))*(Math.PI/180))*handleradius_slider.value+yPos;
						angle += 360/pointsTotal;
					}
					break;
					
				case twirlfwd_btn:
				
					// Adjust for twirling
					for(i=0; i<pointsTotal; i++){
						angle -= .5;
						handles[i].x = Math.sin((angle+(360/(pointsTotal*2)))*(Math.PI/180))*handleradius_slider.value+xPos;
						handles[i].y = Math.cos((angle+(360/(pointsTotal*2)))*(Math.PI/180))*handleradius_slider.value+yPos;
						angle += 360/pointsTotal;
					}
					break;
			}
		}
		
		protected function sliderHandler( event:SliderEvent ):void
		{
			switch( event.target )
			{
				case ptradius_slider:
				
					// Adjust point radius
					for(var i:Number=0; i<pointsTotal; i++){
						points[i].x = Math.sin(angle2*(Math.PI/180))*ptradius_slider.value+xPos;
						points[i].y = Math.cos(angle2*(Math.PI/180))*ptradius_slider.value+yPos;
						angle2 += 360/pointsTotal;
					}
					break;
					
				case handleradius_slider:
			
					// Adjust handle radius
					for(i=0; i<pointsTotal; i++){
						handles[i].x = Math.sin((angle+(360/(pointsTotal*2)))*(Math.PI/180))*handleradius_slider.value+xPos;
						handles[i].y = Math.cos((angle+(360/(pointsTotal*2)))*(Math.PI/180))*handleradius_slider.value+yPos;
						angle += 360/pointsTotal;
					}
					break;
					
				case red_slider:
					
					// Adjust red color
					if( colorMode ){
						c1 = red_slider.value;
					}else{
						lc1 = red_slider.value;
					}
					break;
					
				case green_slider:
				
					// Adjust green color
					if( colorMode ){
						c2 = green_slider.value;
					}else{
						lc2 = green_slider.value;
					}
					break;
					
				case blue_slider:
				
					// Adjust blue color
					if( colorMode ){
						c3 = blue_slider.value;
					}else{
						lc3 = blue_slider.value;
					}
					break;
					
				case opacity_slider:
				
					// Adjust color opacity
					if( colorMode ){
						c4 = opacity_slider.value/100;
					}else{
						lc4 = opacity_slider.value/100;
					}
					break;
			}
		}
		
		protected function enterFrameHandler(event:Event):void
		{
			// Draw tangent lines 
			tangentArea.graphics.clear();
			tangentArea.graphics.lineStyle(0,0x000000,100);
			
			for(var i:Number=0; i<pointsTotal; i++){
				tangentArea.graphics.moveTo(handles[i].x, handles[i].y);
				tangentArea.graphics.lineTo(points[i].x, points[i].y);
			}
			
			// Get hex colors
			var RGB:Number = (c1 << 16 | c2 << 8 | c3);
			var lineRGB:Number = (lc1 << 16 | lc2 << 8 | lc3);
			
			// Draw shape
			drawArea.graphics.clear();
			if( weight_stepper.value > 0 ){
				drawArea.graphics.lineStyle(weight_stepper.value, lineRGB, lc4);
			}
			drawArea.graphics.beginFill(RGB,c4);
			
			for(i=0; i<pointsTotal; i++) 
			{
				if( i == 0 ){
					drawArea.graphics.moveTo(points[i].x, points[i].y);
				}
				angle += 360/pointsTotal;
				
				if( mode ) 
				{
					if( i<pointsTotal-1 ){
						drawArea.graphics.curveTo(handles[i].x, handles[i].y, points[i+1].x, points[i+1].y);
					} else {
						drawArea.graphics.curveTo(handles[i].x, handles[i].y, points[0].x, points[0].y);
					}
				}
				else{
					if( i<pointsTotal-1 ) {
						drawArea.graphics.lineTo(points[i+1].x, points[i+1].y);
					} else {
						drawArea.graphics.lineTo(points[0].x, points[0].y);
					}
				}
			}
			drawArea.graphics.endFill();
		}
	}
}