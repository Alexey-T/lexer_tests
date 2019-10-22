package code
{
	/*****************************************
	 * ArcadeButton.as :
	 * Demonstrates a custom button component
	 * whose color can be changed at authortime
	 * using component parameters.
	 * -------------------
	 * See 2_custom.fla
	 * See ArcadeButton_LivePreview.fla
	 * See ArcadeButton_Source.fla
	 ****************************************/
	 
	import flash.display.*;
	import flash.geom.ColorTransform;
	
	public class ArcadeButton extends Sprite
	{
		//******************
		// Properties:
		
		private var _rgb:Number = 0xCCFF00;
		
		// Assets
		private var _color:MovieClip;
		private var _btn:SimpleButton;
		
		//******************
		// Constructor:
		
		public function ArcadeButton():void
		{
			// Color area
			_color = new ArcadeButtonSkin_Hit();
			addChild(_color);
			
			// Internal button
			_btn = new SimpleButton();
			_btn.upState = new ArcadeButtonSkin_Up();
			_btn.overState = new ArcadeButtonSkin_Ov();
			_btn.downState = new ArcadeButtonSkin_Dn();
			_btn.hitTestState = new ArcadeButtonSkin_Hit();
			_btn.useHandCursor = true;
			addChild(_btn);
		}
		
		//******************
		// Component parameters:
		
		// The Inspectable metadata tag defines an authortime
		// component parameter visible in the Component Inspector...
		
		[Inspectable(type="Color", defaultValue=0x0099FF)]
		public function get rgb():Number
		{
			return _rgb;
		}
		
		public function set rgb(n:Number):void
		{
			_rgb = n;
			
			// Update background color...
			var tr:ColorTransform = new ColorTransform();
			tr.color = _rgb;
			_color.transform.colorTransform = tr;
		}
	}
}