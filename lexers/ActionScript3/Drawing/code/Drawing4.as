package code
{
	/*****************************************
	 * DrawingSample4 :
	 * Demonstrates dynamic color transformations.
	 * -------------------
	 * See 4_color.fla
	 ****************************************/
	 
	import fl.events.SliderEvent;
	import flash.events.Event;
	import flash.events.MouseEvent;
	import flash.display.SimpleButton;
	import flash.display.MovieClip;
	import flash.geom.ColorTransform;
	
	public class Drawing4 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var activeSwatch:MovieClip;
		
		// Color offsets
		public var c1:Number = 0;	// R
		public var c2:Number = 51;	// G
		public var c3:Number = 51;	// B
		
		//*************************
		// Constructor:
		
		public function Drawing4()
		{
			// Respond to mouse events
			swatch1_btn.addEventListener(MouseEvent.CLICK,swatchHandler,false,0,false);
			swatch2_btn.addEventListener(MouseEvent.CLICK,swatchHandler,false,0,false);
			swatch3_btn.addEventListener(MouseEvent.CLICK,swatchHandler,false,0,false);
			swatch4_btn.addEventListener(MouseEvent.CLICK,swatchHandler,false,0,false);
			swatch5_btn.addEventListener(MouseEvent.CLICK,swatchHandler,false,0,false);
			swatch6_btn.addEventListener(MouseEvent.CLICK,swatchHandler,false,0,false);
			apply_btn.addEventListener(MouseEvent.CLICK,clickHandler,false,0,true);
			previewBox_btn.addEventListener(MouseEvent.MOUSE_DOWN,dragPressHandler);
			
			// Respond to drag events
			red_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			red_slider.addEventListener(Event.CHANGE,sliderHandler);
			green_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			green_slider.addEventListener(Event.CHANGE,sliderHandler);
			blue_slider.addEventListener(SliderEvent.THUMB_DRAG,sliderHandler);
			blue_slider.addEventListener(Event.CHANGE,sliderHandler);
			
			// Respond to textfield events
			hexc1_txt.addEventListener(Event.CHANGE,changeHexHandler);
			hexc2_txt.addEventListener(Event.CHANGE,changeHexHandler);
			hexc3_txt.addEventListener(Event.CHANGE,changeHexHandler);
			c1_txt.addEventListener(Event.CHANGE,changeRGBHandler);
			c2_txt.addEventListener(Event.CHANGE,changeRGBHandler);
			c3_txt.addEventListener(Event.CHANGE,changeRGBHandler);
			
			// Draw a frame later
			addEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Initialization:
		
		protected function draw(event:Event):void
		{
			// Set color transformations
			swatch1_btn.chipfill.transform.colorTransform = new ColorTransform(0,0,0,1,127,0,97);
			swatch2_btn.chipfill.transform.colorTransform = new ColorTransform(0,0,0,1,130,0,0);
			swatch3_btn.chipfill.transform.colorTransform = new ColorTransform(0,0,0,1,10,106,0);
			swatch4_btn.chipfill.transform.colorTransform = new ColorTransform(0,0,0,1,0,51,51);
			swatch5_btn.chipfill.transform.colorTransform = new ColorTransform(0,0,0,1,0,51,102)
			swatch6_btn.chipfill.transform.colorTransform = new ColorTransform(0,0,0,1,127,127,127);
			previewBox_btn.transform.colorTransform = new ColorTransform(0,0,0,1,c1,c2,c3);
			
			// Handle listeners...
			removeEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Event Handling:
		
		protected function dragPressHandler( event:MouseEvent ):void
		{
			activeSwatch = new Swatch();
			activeSwatch.transform.colorTransform = new ColorTransform(0,0,0,1,c1,c2,c3);
			activeSwatch.addEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);			
			activeSwatch.startDrag(true);
			addChild(activeSwatch);
		}
		
		protected function dragReleaseHandler( event:MouseEvent ):void
		{
			if( activeSwatch.hitTestObject(car) ){
				car.transform.colorTransform = new ColorTransform(0,0,0,1,c1,c2,c3);
			}
			activeSwatch.stopDrag();
			activeSwatch.removeEventListener(MouseEvent.MOUSE_UP,dragReleaseHandler);			
			removeChild(activeSwatch);
		}
		
		protected function clickHandler(event:MouseEvent):void
		{
			car.transform.colorTransform = new ColorTransform(0,0,0,1,c1,c2,c3);
		}
		
		protected function changeRGBHandler(event:Event):void
		{
			c1 = Number(c1_txt.text);
			c2 = Number(c2_txt.text);
			c3 = Number(c3_txt.text);
			
			if(!(c1>=0)){
				c1 = 0;
			}
			if(!(c2>=0)){
				c2 = 0;
			}
			if(!(c3>=0)){
				c3 = 0;
			}
			hexc1_txt.text = c1.toString(16);
			hexc2_txt.text = c2.toString(16);
			hexc3_txt.text = c3.toString(16);
			
			updateSliders();
		}
		
		protected function changeHexHandler(event:Event):void
		{
			c1 = parseInt("0x"+hexc1_txt.text);
			c2 = parseInt("0x"+hexc2_txt.text);
			c3 = parseInt("0x"+hexc3_txt.text);
			
			if(!(c1>=0)){
				c1 = 0;
			}
			if(!(c2>=0)){
				c2 = 0;
			}
			if(!(c3>=0)){
				c3 = 0;
			}
			c1_txt.text = c1.toString();
			c2_txt.text = c2.toString();
			c3_txt.text = c3.toString();
			
			updateSliders();
		}
		
		protected function sliderHandler( event:SliderEvent ):void
		{
			switch( event.target )
			{
				case red_slider:
					
					c1 = red_slider.value;
					break;
					
				case green_slider:
				
					c2 = green_slider.value;
					break;
					
				case blue_slider:
				
					c3 = blue_slider.value;
					break;
			}
			updateFields();
		}
		
		protected function swatchHandler(event:MouseEvent):void
		{
			switch( event.target.parent )
			{
				case swatch1_btn:
					
					c1 = 127;
					c2 = 0;
					c3 = 97;
					break;
					
				case swatch2_btn:
				
					c1 = 130;
					c2 = 0;
					c3 = 0;
					break;
					
				case swatch3_btn:
				
					c1 = 10;
					c2 = 106;
					c3 = 0;
					break;
					
				case swatch4_btn:
					
					c1 = 0;
					c2 = 51;
					c3 = 51;
					break;
					
				case swatch5_btn:
				
					c1 = 0;
					c2 = 51;
					c3 = 102;
					break;
					
				case swatch6_btn:
				
					c1 = 127;
					c2 = 127;
					c3 = 127;
					break;
			}
			updateFields();
			updateSliders();
		}
		
		//*************************
		// Utils:
		
		protected function updateFields():void
		{
			hexc1_txt.text = c1.toString(16);
			hexc2_txt.text = c2.toString(16);
			hexc3_txt.text = c3.toString(16);
			
			c1_txt.text = c1.toString();
			c2_txt.text = c2.toString();
			c3_txt.text = c3.toString();
			
			previewBox_btn.transform.colorTransform = new ColorTransform(0,0,0,1,c1,c2,c3);
		}
		
		protected function updateSliders():void
		{
			red_slider.value = c1;
			green_slider.value = c2;
			blue_slider.value = c3;
			
			previewBox_btn.transform.colorTransform = new ColorTransform(0,0,0,1,c1,c2,c3);
		}
	}
}