//. About
// This is an OpenSCAD document to demo SynWrite OpenSCAD lexer highlighting
// By: Runsun Pan, Dec 2015

// Using "//. " at beginning of line for section
//       "//. " inside a module for in-module sections
//       "//: " inside a func for in-func section

//. String, Symbol, Number, boolean
"abc", "What \"Solid\" means"
 { } , < > ; # ( ) [ ] = . * + $ - ? / &
123, 23.56, (-456), 0.0023, a-34, a - 34
true false, undef, NaN
//. Id
// Id (Identifiers) are set up using: an <Identifier> token, a comon 
// id parser (p_id), specific parsers to match word pattern if 
// needed (like, constant) and rules of type "Tag detector" 
// to list keywords if needed. They are styled with corresponding styles

// valid identifiers (function name, module name, variable)
abc, _abc, _12, abc12, 12abc, 12_, $abc  
// invalid identifiers :
a$bc 12$ A$bc 
// constant (all capital)
_SCALE, UCONV_TABLE,  $ABC, _A12, 12A
// All cap followed by "(" is a mod or func name:
ABC(, BODY_ARM(
//. Id: Reserved
// Reserved words, can't be changed
// Define : Rules/r_id_openscad_reserved/Conditions
// style : Style/s_id_reserved
else for function if import include let module use
//. Id: Builtin.Core
// Builtins can be overwritten:
$children $fa $fn $fs $t $vpd $vpr $vpt abs acos asin atan atan2 ceil children 
chr concat
//. Id: Builtin.Actions
// OpenSCAD builtins representing an operation: 
color difference hull intersection minkowski mirror multmatrix offset resize 
rotate scale translate union
/// Id - Builtin.Object
circle cube cylinder polygon polyhedron sphere square text       
          
//. Blocks
// Blocks are setup mainly using Rules (Rules have 4 types: Range Start,
// Range End, Tag Detector, and Line Separator). 
// Some rules don't have Range End.
            
//. Block: Comment
 a=3; // Continuous comments can be folded together:

// line cmt2
/* 
   multiline 
   cmt
*/
// line 3 

//. Block: Operation
// OpenSCAD operation block: 
//   Start rule: r<id>(...){
//   End rule  : r...}

    union(){   
       translate( pq[0])
       sphere( r= r*mm ); 
               
       translate( pq[1])
       sphere( r= r*mm);          
    } 

//. BLock: For/If loop
// Share same rules w/ the "Operation Block"

    for(i=[0:3]){
      if(a==0){
        xxx;
      } else {
        bbb;
      }
      ddd;
    }
    
//. Block: Function    
// Function block:
//  Start_Rule: r_func <id>(
//  End_Rule  : r_;

// let() inside function is foldable
//    Start_Rule= r_let(, End_Rule = r_)
 function asc(c, n=0, m=get(4), d=[2,"a",false]) = 
 (   
      let( a=b
         , c="dd"
         , d=get(3)
         )
      // line-comment in func 
      c == chr(n) ? n : asc(c, n + 1)
      /* 
         multiline comment 
         in func
      */
 );                   
 
// Comment inside func is foldable  
 function hash(h,k, notfound)= 
 (
  // Note the differnce:
  /* 
     v= hash( h,k,3 )   // =3 when k not defined
     v= or( hash(h,k), 3 )  // =3 when h[k]= false,undef,"",0 ...
  */
  und( [ for(i=[0:2:len(h)-2])
          if( h[i]==k ) h[i+1] ][0], notfound )
);

//. Block: In-Func Section
// A //: inside a function is treated as an in-function section header
// Block rule: Start_Rule= r_inFuncSect //:, End_Rule= r_inFuncSect //: end
function chaindata( pts, nside, nsidelast )=
(   
    //: i=0 
   _i==0? 
   
   let( //###################################### i = 0    
        opt = popt(opt) // handle sopt
      , rot= und(rot, hash(opt,"rot",0 ))         
      )
      chaindata( pts
               , nside=nside
               , nsidelast=nsidelast
               )
                                
   //: 0< i <last     
   : 0<_i && _i <len(pts)-1?
    
      chaindata( pts
               , nside=nside
               , nsidelast=nsidelast
               ) 
               
  //: i=last
   :  
   
     let( 
         // #################################### i = last
         // line 2
          dtwi = twist? twist/(len(pts)-1) :0
        , rs = _rs
        , k = 
                     
     [ for( i=range(pts) )
              i==0 ? lofthead
              : i== len(pts)-1? lofttail
              : let( pts_unloft = newcuts[i]
                   , lofthead_at90i = 
                      // this is lofthead relocated to pl 90d to (i-1,i)
                      [for(xi= range(lofthead))                                    
                         anglePt( [ pts[i-1], pts[i], pts_unloft[0] ])    
                      ]
                   , projected_loft_i = 
                        [for(xi= range(lofttail))
                          let( phead= lofthead_at90i[xi]
                             , ptail= lofttail_at90i[xi]
                             )
                        projPt( p90, p012( pts_unloft ),get(pts,[i-1,i]) )
                        ]
                     ) 
              projected_loft_i 
        ]
        : newcuts
        )
      ["cuts", isarr(r) && r[0]==0? concat( [[pts[0]]], slice( cuts,1))
               :cuts
        ,"rs", rs
         ,"Rf", Rf 
         
         ,"__<b>debug</b>__","==>"  
         ,"debug", _debug
      ]    
                
   );  // chaindata 
            
 
//. Block: Module
// Module blocks have two Start_Rules and one End_Rule:
//   Start_Rule= r_module <id>(...){
//               r_module<id>(~)<cmt>{
//   End_rule=  r...}

module Line0( pq  // two-pointer
            , opt=[]
            )
{   
    //echom("Line0");
    r = hash( opt, "r", 0.015 );
    fn = hash( opt, "fn", 6); 
    
//    pqr= len(pq)==2?app(pq,randPt()):pq;
//    qpr= sel(pqr, [1,0,2]);
 
//    pts = concat( ptsP, ptsQ );

    color( hash(opt, "color"), hash(opt, "transp",1) )  
    
    hull(1,2,3){   
       translate( pq[0])
       sphere( r= r*mm ); 
               
       translate( pq[1])
       sphere( r= r*mm);          
    }        
    
    for(i=[0:3]){
      if(a==0){
        xxx;
      } else {
        bbb;
      }
      ddd;
    }
    
    abc def 
}

//rng = range(xi, d, xj+d);
//echo( rng=rng );
//echo( func2d(ybeg=2,yend=5));

//. Block: In-Mod Section
// A //. inside a module is treated as an in-module section header
// Block rule: Start_Rule= r_inModSect //., End_Rule= r_inModSect //. end
module ColorAxes( ops=[] ){

  //. init
	ops = concat( ops,
	[
		"r",0.012
		,"len", 4.5
		,"xr",-1
		,"yr",-1
		,"zr",-1
		,"fontscale", 1
		,"xops", ["color","red", "transp",0.3]
		,"yops", ["color","green", "transp",0.3]
		,"zops", ["color","blue", "transp",0.3]
	]);
	function ops(k)= hash(ops,k);
	r = ops("r");
	l = ops("len");

	fscale= ops("fontscale");
	//echo( "fscale" , fscale);
	//include <../others/TextGenerator.scad>
	//. Set data
	xr= hash( ops, "xr", if_v=-1, then_v=r) ;
	yr= hash( ops, "yr", r, if_v=-1, then_v=r);
	zr= hash( ops, "zr", r, if_v=-1, then_v=r);
	
	echo( "r",r, "xr", xr);
 
  //. Make Obj
	translate( [l+0.2, -6*r,0]) 
		rotate($vpr) scale( fscale/15 ) 
		color(xc) text( "x" );
	translate( [6*r, l+0.2, 0]) 
		rotate($vpr) scale( fscale/15 ) 
		color(yc) text( "y" );
	translate( [0, 0, l+0.2]) 
		rotate($vpr) scale( fscale/15 ) 
		color(zc) text( "z" );
    
  //. Draw line
	Line0( [[ l,0,0],[-l,0,0]], concat( ["r", xr], ops("xops") ));
	Line0( [[ 0, l,0],[0, -l,0]], concat( ["r", yr], ops("yops") ));
	Line0( [[ 0,0,l],[0,0,-l]], concat( ["r", zr], ops("zops") ));

	xc = hash(ops("xops"), "color");
	yc = hash(ops("yops"), "color");
	zc = hash(ops("zops"), "color");   
} 

                  
module ColorAxes_test(ops){ doctest( ColorAxes ,ops=ops);}
 
//========================================================
rand=["rand", "m=1, n=undef, seed=undef", "Any", "Random, Number, String",
" Given a number (m), return a random number between 0~m.
;; If another number (n) is given, return a random number between m~n.
;; If m is an array or string, return one of its member randomly.
"];

function rand(m=1, n=undef, seed=undef)=
(
	isarr( m )||isstr(m) ? m[ floor(rand(len(m),seed=seed)) ]
	: (isnum(n)
		? (seed==undef? rands( min(m,n), max(m,n), 1)
                     : rands( min(m,n), max(m,n), 1, seed) )
		: (seed==undef? rands( 0, m, 1)
                     : rands( 0, m, 1, seed) )
	  )[0]
);

module ChainByHull()// pts, opt=[]) // for debug use
{
    for( i = [1:len(pts)-1] )
        Line0( [pts[i-1], pts[i]], opt=opt );
}
//========================================================

function uconv(n,units="in,cm", utable=UCONV_TABLE)=
  
  let( frto = split(units,",")   
      , isftin= endwith(n, ["'","\""] ) 
	    , m    = or(num(n), n) 
      , from = (isftin||len(frto)==1) ? "in":frto[0]
	    , to   = or( frto[1] , frto[0] ) 
	    , units= str(from, ",", to) 
  )
//    [ from, to ]

	(from==to) ? m
	: isnum(utable)? m* utable
    : m * hash( utable
			  , units )
;


