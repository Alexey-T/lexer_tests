package code
{
	/*****************************************
	 * Media1 :
	 * Demonstrates using text dynamically.
	 * -------------------
	 * See 1_dynamictext.fla
	 ****************************************/
	 
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.text.TextField;
	
	public class Media1 extends MovieClip
	{
		//*************************
		// Properties:
		
		public var salary:Number = 0;
		public var deductions:Number = 0;
		public var four01k:Number = 0;
		public var espp:Number = 0;
		public var gross:Number = 0;
		public var amount:Number = 0;
		
		//*************************
		// Constructor:
		
		public function Media1()
		{
			// Respond to change events
			salary_ti.addEventListener(Event.CHANGE,changeHandler);
			deductions_ti.addEventListener(Event.CHANGE,changeHandler);
			four01k_ti.addEventListener(Event.CHANGE,changeHandler);
			espp_ti.addEventListener(Event.CHANGE,changeHandler);
			married_radio.addEventListener(Event.CHANGE,changeHandler);
			single_radio.addEventListener(Event.CHANGE,changeHandler);
			
			// Draw a frame later
			addEventListener(Event.ENTER_FRAME,draw);
		}
		
		//*************************
		// Event Handling:
		
		protected function draw(event:Event):void
		{
			calculatePayCheck();
			removeEventListener(Event.ENTER_FRAME,draw);
		}
		
		protected function changeHandler(event:Event):void
		{
			calculatePayCheck();
		}
		
		//*************************
		// Utils:
		
		protected function calculatePayCheck():void
		{
			// Get input
			salary = Number(salary_ti.text);
			deductions = Number(deductions_ti.text);
			four01k = Number(four01k_ti.text);
			espp = Math.min(Number(espp_ti.text),15);
			gross = (Math.round((salary/24-Math.min(0,Number(four01k_txt.text)))*100))/100;
			
			// Set fields
			sdi_txt.text = String((Math.round((gross*.009)*100))/100);
			medicare_txt.text = String((Math.round((gross*.0145)*100))/100);
			ss_txt.text = String((Math.round((gross*.062)*100))/100);
			four01k_txt.text = String((Math.round((salary/24*(four01k*.01))*100))/100);
			espp_txt.text = String((Math.round((gross*(espp*.01))*100))/100);
			
			// Calculate paycheck...
			var withholdingAllowance:Number = 120.83;
			var fedwithholdamount:Number = deductions*withholdingAllowance;
			var adjustedgross:Number = gross-fedwithholdamount;
			var fedbase:Number = 0;
			var fedthresh:Number = 0;
			var fedpercent:Number = 0;
			
			if( single_radio.selected ) 
			{
				if( adjustedgross < 110 ){
					fedbase = 0;
					fedthresh = 0;
					fedpercent = 0;
				}
				if( adjustedgross > 110 && adjustedgross <= 1196 ){
					fedbase = 0;
					fedthresh = 110;
					fedpercent = .15;
				}
				if( adjustedgross > 1196 && adjustedgross <= 2592 ){
					fedbase = 162.9;
					fedthresh = 1196;
					fedpercent = .28;
				}
				if( adjustedgross > 2592 && adjustedgross <= 5767 ){
					fedbase = 553.78;
					fedthresh = 2592;
					fedpercent = .31;
				}
				if( adjustedgross > 5767 && adjustedgross <= 12458 ){
					fedbase = 1538.03;
					fedthresh = 5767;
					fedpercent = .36;
				}
				if( adjustedgross > 12458 ){
					fedbase = 3946.79;
					fedthresh = 12458;
					fedpercent = .396;
				}
			}
			else{
				if( adjustedgross < 269 ){
					fedbase = 0;
					fedthresh = 0;
					fedpercent = 0;
				}
				if( adjustedgross > 269 && adjustedgross <= 2079 ) {
					fedbase = 0;
					fedthresh = 269;
					fedpercent = .15;
				}
				if( adjustedgross > 2079 && adjustedgross <= 4383 ){
					fedbase = 271.5;
					fedthresh = 2079;
					fedpercent = .28;
				}
				if( adjustedgross > 4383 && adjustedgross <= 7133 ){
					fedbase = 916.62;
					fedthresh = 4383;
					fedpercent = .31;
				}
				if( adjustedgross > 7133 && adjustedgross <= 12585 ){
					fedbase = 1769.12;
					fedthresh = 7133;
					fedpercent = .36;
				}
				if( adjustedgross > 12585 ){
					fedbase = 3731.84;
					fedthresh = 12585;
					fedpercent = .396;
				}
			}
			// Set field
			federal_txt.text = String((Math.round((((adjustedgross-fedthresh)*fedpercent)+fedbase)*100))/100);
			
			var statebase:Number = (Math.round(((gross-2400)*.093)*100))/100;
			var statededuct:Number = deductions*3.13;
			var stateincrement:Number = 0;
			var statewithholding:Number = 0;
			
			if( statewithholding < 0 ){
				statewithholding = 0;
			}
			if( gross >= 2400 ){
				state_txt.text = String(statebase+statewithholding);
			} else {
				state_txt.text = String(statewithholding);
			}
			if( gross < 300 ){
				statewithholding = 0;
			}
			if( gross >= 300 && gross < 500 )
			{
				stateincrement = Math.floor((gross-300)/20)+1;
				
				if( gross >= 300 && gross < 340 ){
					statewithholding = 1.93+((stateincrement-1)*.20)-statededuct;
				}
				if( gross >= 340 && gross < 500 ){
					statewithholding = 2.39+((stateincrement-3)*.40)-statededuct;
				}
			}
			if( gross >= 500 && gross < 1620 )
			{
				stateincrement = Math.floor((gross-500)/40)+1;
				
				if( gross >= 500 && gross < 660 ){
					statewithholding = 5.79+((stateincrement-1)*.80)-statededuct;
				}
				if( gross >= 660 && gross < 980 ){
					statewithholding = 9.47+((stateincrement-5)*1.60)-statededuct;
				}
				if( gross >= 980 && gross < 1300 ){
					statewithholding = 22.9+((stateincrement-13)*2.40)-statededuct;
				}
				if( gross >= 1300 && gross < 1620 ){
					statewithholding = 42.54+((stateincrement-21)*3.20)-statededuct;
				}
			}
			if( gross >= 1620 && gross < 1660 ){
				statewithholding = 68.53-statededuct;
			}
			else if( gross >= 1660 && gross < 1700 ){
				statewithholding = 72.25-statededuct;
			}
			else if( gross >= 1700 && gross < 1750 ){
				statewithholding = 76.43-statededuct;
			}
			else if( gross >= 1750 && gross < 1800 ){
				statewithholding = 81.08-statededuct;
			}
			else if( gross >= 1800 && gross < 1850 ){
				statewithholding = 85.73-statededuct;
			}
			else if( gross >= 1850 && gross < 1900 ){
				statewithholding = 90.38-statededuct;
			}
			else if( gross >= 1900 && gross < 1950 ){
				statewithholding = 95.03-statededuct;
			}
			else if( gross >= 1950 && gross < 2000 ){
				statewithholding = 99.68-statededuct;
			}
			else if( gross >= 2000 && gross < 2100 ){
				statewithholding = 106.66-statededuct;
			}
			else if( gross >= 2100 && gross < 2200){
				statewithholding = 115.96-statededuct;
			}
			else if( gross >= 2200 && gross < 2300 ){
				statewithholding = 125.26-statededuct;
			}
			else if( gross >= 2300 ){
				statewithholding = 134.56-statededuct;
			}
			// Value
			amount = gross-(Number(sdi_txt.text)+Number(medicare_txt.text)+Number(ss_txt.text)+Number(federal_txt.text)+Number(state_txt.text))-Number(espp_txt.text);
			
			// Set field
			paycheck_txt.text = String(Math.round(amount*100)/100);
		}
	}
}