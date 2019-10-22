package code.amoebaclasses
{
	/*****************************************
	 * InteractiveLayer :
	 * This layer separates the player area from the 
	 * user interface so that players and obstacles can 
	 * be generated dynamically while appearing below
	 * the user interface. The InteractiveLayer contains
	 * the majority of the game functionality.
	 * -------------------
	 * See 2_amoebas.fla
	 ****************************************/
	 
	import flash.media.Sound;
	import flash.media.SoundChannel;
	import flash.media.SoundTransform;
	import flash.events.Event;
	import flash.display.MovieClip;
	import flash.utils.getTimer;
	
	public class InteractiveLayer extends MovieClip
	{
		//*************************
		// Properties:
		
		public var radius:Number = 0;
		public var radius2:Number = 0;
		public var centerx:Number = 0;
		public var centery:Number = 0;
		public var totalCells:Number = 0;
		public var cellNumber:Number = 1;
		public var shotNumber:Number = 1000;
		public var totalShots:Number = 1;
		public var maxShots:Number = 4;
		public var shotSpeed:Number = 17;
		public var shotXspeed:Number = 17;
		public var shotYspeed:Number = 17;
		public var xThrustPercent:Number = 0;
		public var yThrustPercent:Number = 0;
		public var started:Boolean = false;
		
		// Sounds
		public var shotSnd:Sound;
		public var shotSndChannel:SoundChannel;
		public var soundStartable:Boolean = true;
		
		// Assets
		public var cells:Object = new Object();
		public var shots:Object = new Object();
		
		// Reference to game
		public var owner;
		public var amoebaCell;
		
		//*************************
		// Constructor:
		
		public function InteractiveLayer()
		{
			// Wait to be started...
		}
		
		//*************************
		// Lifecycle...
		
		public function live( ref:* ):void
		{
			// Set game level reference
			owner = ref;
			
			// Initialize ship
			ship.live(this);
			
			// Initialize Amoeba
			amoebaCell = new Amoeba();
			amoebaCell.live(this,"amoebaCell");
			addChild(amoebaCell);
			
			// Initialize sounds
			shotSnd = new shootSound();
			
			// Update screen every frame
			addEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		public function die():void
		{
			removeEventListener(Event.ENTER_FRAME,enterFrameHandler);
		}
		
		//*************************
		// Event Handling:
		
		protected function enterFrameHandler(event:Event):void
		{
			// Generate cells and increase level 
			// when all cells are destroyed
			if( totalCells == 0 && started )
			{
				owner.level++;
				cellNumber = 1;
			}
			if( cellNumber <= owner.level )
			{
				var id:String = "_"+cellNumber;
				cells[id] = new Paramecium();
				cells[id].live(this,id);
				
				addChild(cells[id]);
				
				totalCells++;
				cellNumber++;
				started = true;
			}
			
			// After death, wait until center area is 
			// clear for 3 seconds before reincarnation
			if( ship.dead && owner.lives > 0 )
			{
				ship.pauseTime = getTimer() - ship.deathTime;
				if( ship.pauseTime > 3000 && owner.lives > 0 )
				{
					ship.dead = false;
					ship.fadeIn = true;
				}
			}
		}
		
		//*************************
		// Public methods:
		
		public function initiate():void
		{
			// Clean up
			for(var i in cells){
				if( cells[i] != null ){
					removeChild(cells[i]);
				}
			}
			cells = new Object();
			shots = new Object();
			
			// Reset values
			cellNumber = 1;
			totalCells = 0;
			totalShots = 0;
			
			// Reset ship
			ship.rotation = 0;
			ship.fadeIn = true;
			
			// Reset amoeba
			death(amoebaCell);
		}
		
		public function shoot( whichObject ):void
		{
			if( totalShots < maxShots )
			{
				xThrustPercent = Math.sin(whichObject.rotation*(Math.PI/180));
				yThrustPercent = Math.cos(whichObject.rotation*(Math.PI/180));
				
				shotYspeed = shotSpeed*yThrustPercent;
				shotXspeed = shotSpeed*xThrustPercent;
				shotNumber += 1000;
				totalShots++;
				
				var id:String = "_"+shotNumber;
				shots[id] = new Shot();
				shots[id].live(this,id);
				
				addChild(shots[id]);
				shotSndChannel = shotSnd.play();
			}
		}
		
		public function wrapAround( whichObject ):void
		{
			var delta_x:Number = centerx-whichObject.x;
			var delta_y:Number = centery-whichObject.y;
			var angle:Number = Math.atan2(delta_y, delta_x);
			var distance:Number = Math.sqrt((delta_x*delta_x)+(delta_y*delta_y));
			if( distance > radius2 ){
				whichObject.x = (Math.cos(angle)*(radius2))+centerx;
				whichObject.y = (Math.sin(angle)*(radius2))+centery;
			}
		}
		
		public function collisions( whichObject ):void
		{
			// Collisions with ship
			if( whichObject.hitTestPoint((ship.x+4),(ship.y+4),true) && ship.alpha>.9 ){
				whichObject.hit();
				death(ship);
			}
			// Collisions with shots
			for(var n in shots) 
			{
				if( shots[n] != null )
				{
					if( whichObject.hitTestPoint(shots[n].x,shots[n].y,true) ){
						whichObject.hit();
						
						if( whichObject is GuidedCell ){ 
							owner.score += 150;
						}else if( whichObject is Amoeba ){
							owner.score += 75;
						}else if( whichObject is Paramecium ){
							owner.score += 50;
						}else if( whichObject is Glob ){
							owner.score += 25;
						}
						removeShot(n);
					}
				}
			}
		}
		
		public function explosion( whichObject ):void
		{
			var e:Explosion = new Explosion();
			e.x = whichObject.x;
			e.y = whichObject.y;
			
			// Show explosion
			addChild(e);
		}
		
		public function death( whichShip ):void
		{
			if( owner.lives != 0 && whichShip.alpha > .9 )
			{
				explosion( whichShip );
			}
			if( whichShip == ship ) 
			{
				ship.deathTime = getTimer();
				
				if(!ship.dead){
					owner.lives--;
				}
				ship.clear();
				ship.dead = true;
				ship.alpha = 0;
				ship.x = 302;
				ship.y = 295;
			} 
			else{
				whichShip.appearTime = getTimer();
				whichShip.pauseTime = (Math.random()*7000)+7000;
				whichShip.visible = false;
			}
		}
		
		public function splitAmoeba( target ):void
		{
			var id:String;
			for(var i:Number=1; i<=2; i++) 
			{
				cellNumber++;
				totalCells++;
				
				id = "_"+cellNumber;
				cells[id] = new Glob();
				cells[id].live(this,id);
				cells[id].x = target.x;
				cells[id].y = target.y;
				
				addChild(cells[id]);
			}
			if( ship.alpha > 0 )
			{
				cellNumber++;
				totalCells++;
				
				id = "_"+cellNumber;
				cells[id] = new GuidedCell();
				cells[id].live(this,id);
				cells[id].x = target.x;
				cells[id].y = target.y;
				
				addChild(cells[id]);
			}
		}
		
		//*************************
		// Utils:
		
		public function removeCell(id:String):void
		{
			if( cells[id] != null)
			{
				totalCells--;
				removeChild(cells[id]);
				delete cells[id];
			}
		}
		
		public function removeShot(id:String):void
		{
			if( shots[id] != null )
			{
				totalShots--;
				removeChild(shots[id]);
				delete shots[id];
			}
		}
	}
}