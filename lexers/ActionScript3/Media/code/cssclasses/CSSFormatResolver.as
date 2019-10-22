package code.cssclasses
{
	/*****************************************
	 * CSSFormatResolver :
	 * Creates a TLF formatResolver capable of parsing
	 * CSS stylesheet formatting with HTML text or TLF
	 * markup text.
	 * -------------------
	 * This code was adapted from the following OpenSource document on 8/31/2010:
	 * http://opensource.adobe.com/svn/opensource/flex/sdk/sandbox/viatropos/trunk/frameworks/projects/spark/src/mx/core/TLFTextField.as
	 ****************************************/
	
	import flash.text.StyleSheet;
	import flash.text.engine.*;
	import flash.utils.Dictionary;
	
	import flashx.textLayout.elements.DivElement;
	import flashx.textLayout.elements.FlowElement;
	import flashx.textLayout.elements.FlowGroupElement;
	import flashx.textLayout.elements.IFormatResolver;
	import flashx.textLayout.elements.LinkElement;
	import flashx.textLayout.elements.ParagraphElement;
	import flashx.textLayout.elements.SpanElement;
	import flashx.textLayout.elements.TextFlow;
	import flashx.textLayout.formats.LeadingModel;
	import flashx.textLayout.formats.ITextLayoutFormat;
	import flashx.textLayout.formats.TextLayoutFormatValueHolder;
	
	public class CSSFormatResolver implements IFormatResolver
	{
		//*************************
		// Properties:
		
		private var styleSheet:StyleSheet;
		private var styleFormatCache:Dictionary;
		
		//*************************
		// Constructor:
		
		public function CSSFormatResolver( ss:StyleSheet ):void
		{
			styleSheet = ss;
			
			// cache results
			styleFormatCache = new Dictionary(true);
		}
		
		//*************************
		// Utils:
	 
		private function addStyleAttributes(attr:TextLayoutFormatValueHolder, styleSelector:String):TextLayoutFormatValueHolder
	 	{
	 		var cssStyle:Object = styleSheet.getStyle(styleSelector);
	 		if( cssStyle )
	 		{ 			
				// Create TextLayoutFormatValueHolder object if needed
	 			if( attr == null ){
	 				attr = new TextLayoutFormatValueHolder();
				}
				// Check to see which properties to return
				for( var prop:String in cssStyle )
				{
					// Map CSS property names to TextLayoutFormat equivalents
					switch( prop )
					{
						case "leading":
							attr.leadingModel = LeadingModel.ASCENT_DESCENT_UP;
							attr.lineHeight = cssStyle[prop];
							break;
							
						case "letterSpacing":
							attr.trackingRight = cssStyle[prop];
							break;
							
						case "marginLeft":
							attr.paragraphStartIndent = cssStyle[prop];
							break;
							
						case "marginRight":
							attr.paragraphEndIndent = cssStyle[prop]; 
							break;
							
						case "kerning":
							// Convert from true/false to on/off
							if( prop == "true" ){
								attr.kerning = Kerning.ON;
							}else {              
								attr.kerning = Kerning.OFF;
							}
							break;
							
						default:
							// Same name, same values
							attr[prop] = cssStyle[prop];
					}
				}
	 		}
	 		return attr;
	 	}
		
		//*************************
		// Methods:
	 
		// Calculate the style for a particular element.
		
	 	public function resolveFormat(elem:Object):ITextLayoutFormat
	 	{
			// Use cached value if it exists
	 		var attr:TextLayoutFormatValueHolder = styleFormatCache[elem];
	 		if( attr ){
	 			return attr;
			}
			// Map flow elements to their HTML equivalents,
			if( elem is FlowElement )
			{
				if( elem is TextFlow ){
					attr = addStyleAttributes(attr, "body");  
				}
				else if( elem is ParagraphElement ){
					attr = addStyleAttributes(attr, "p");
				}
				else if( elem is DivElement ){
					attr = addStyleAttributes(attr, "div");
				}
				else if( elem is SpanElement ){
					attr = addStyleAttributes(attr, "span");
				}
				// Apply class selector over any format from above.
				if( elem.styleName ){
					attr = addStyleAttributes(attr, "." + elem.styleName);
				}
				styleFormatCache[elem] = attr;
			}    
			// Or if elem is ContainerController inherit via the container.
	 		return attr;
	 	}
 		
 		// Calculate the user style for a particular element not supported in TLF.
		
 		public function resolveUserFormat( elem:Object, userStyle:String ):*
 		{
			var flowElem:FlowElement = elem as FlowElement;
			var attr:TextLayoutFormatValueHolder;
			
			// Support non-tlf styles
			if( flowElem )
			{
				if( flowElem.styleName )
				{
					attr = addStyleAttributes(null, "." + flowElem.styleName);
				}
				else if( flowElem is LinkElement )
				{
					if( userStyle == "linkNormalFormat" ){
						attr = addStyleAttributes(null, "a");
						attr = addStyleAttributes(attr, "a:link");
					}
					else if( userStyle == "linkHoverFormat" ){
						attr = addStyleAttributes(null, "a:hover");
					}
					else if( userStyle == "linkActiveFormat" ){
						attr = addStyleAttributes(null, "a:active");
					}
				}else{
					attr = addStyleAttributes(null, userStyle);
				}            
			}
			return (attr ? attr : undefined);
 		}
 		
 		// Completely clear the cache.   
		
 		public function invalidateAll( tf:TextFlow ):void
 		{
 			styleFormatCache = new Dictionary(true);	
 		}
 		
 		// The style of one element is invalidated. 
		
 		public function invalidate( target:Object ):void
 		{
 			delete styleFormatCache[target];
			
			// Delete style recursively...
 			var blockElem:FlowGroupElement = target as FlowGroupElement;
 			if( blockElem )
 			{
	 			for(var idx:int = 0; idx < blockElem.numChildren; idx++){
	 				invalidate(blockElem.getChildAt(idx));
				}
	 		}
 		}
 		 	
	 	// Format resolvers are sharable between TextFlows.
		
		public function getResolverForNewFlow( oldFlow:TextFlow, newFlow:TextFlow ):IFormatResolver
	 	{ 
			return this; 
		}
	}
}