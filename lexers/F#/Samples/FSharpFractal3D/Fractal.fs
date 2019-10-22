module Fractal

#nowarn "57"

open System
open System.Collections.Generic 
open System.IO
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Media.Media3D
open System.Xml

// ----------------------------------------------------------------------------------- //
// F# Types & Utility functions

// Point in the 3D space
type point3d = float * float * float
// 3D point represented using integers
type pointi3d = int * int * int
// Represents a triangle in 3D
type triangle = point3d * point3d * point3d
// Trianlge using integral points
type trianglei = pointi3d * pointi3d * pointi3d
// Triangle represented using indices
type triindex = int * int * int

// Cross-product taking two lists as parameters
let cross_tup ls1 ls2 =    
  ls2 |> List.map ( fun el2 -> 
    ls1 |> List.map ( fun el1 -> (el1, el2) ) ) |> List.concat


// Translate triangles
let translate (d:int * int * int) (pts:trianglei list) : trianglei list =
  let tpt (dx, dy, dz) ((x, y, z):pointi3d) = (x + dx, y + dy, z + dz)
  pts |> List.map ( fun (pt1, pt2, pt3) -> 
    (tpt d pt1, tpt d pt2, tpt d pt3) )


// Converting grid with triangles to mesh        
let to_mesh grid_size (trigs:trianglei list) = 
  let idx (x, y, z) =
    (((-z) * grid_size) + y) * grid_size + x
  trigs |> List.map ( fun (p1, p2, p3) -> (idx p1, idx p2, idx p3) )

// ----------------------------------------------------------------------------------- //
// Mesh representation

type Mesh =  
  { Points : point3d list;  
    Triangles : triindex list; } with
    
  // Creates WPF mesh from F# 'mesh'    
  member m.ToWpfMesh() =    
    let ret = new MeshGeometry3D()  
    m.Points |> List.iter (fun (x,y,z) -> 
      ret.Positions.Add(new Point3D(x, y, z)) )
    m.Triangles |> List.iter (fun (a,b,c) ->
      ret.TriangleIndices.Add(a)
      ret.TriangleIndices.Add(b)
      ret.TriangleIndices.Add(c) )
    ret

// ----------------------------------------------------------------------------------- //
// Module that contains all supporting functions

module Internal = 

  // Cube constructed using triangles -
  // first 'point' specifies the side of the cube and the rest is a list with 
  // triangles to be rendered (so it is possible to easy filter relevant sides)
  let private cubeMeshes : ((int * int * int) * trianglei list) list = 
    [ ( 1, 0, 0), [((1, 1, -1), (1, 1,  0), (1, 0, -1)); ((1, 1,  0), (1, 0,  0), (1, 0, -1)); ];   // right
      (-1, 0, 0), [((0, 1, -1), (0, 0, -1), (0, 1,  0)); ((0, 1,  0), (0, 0, -1), (0, 0,  0)); ];   // left
      ( 0, 0,-1), [((1, 1,  0), (0, 1,  0), (1, 0,  0)); ((0, 1,  0), (0, 0,  0), (1, 0,  0)); ];   // front
      ( 0, 0, 1), [((1, 1, -1), (1, 0, -1), (0, 1, -1)); ((0, 1, -1), (1, 0, -1), (0, 0, -1)); ];   // back
      ( 0, 1, 0), [((1, 1, -1), (0, 1, -1), (1, 1,  0)); ((0, 1, -1), (0, 1,  0), (1, 1,  0)); ];   // top
      ( 0,-1, 0), [((1, 0, -1), (1, 0,  0), (0, 0, -1)); ((0, 0, -1), (1, 0,  0), (0, 0,  0))  ]; ] // bottom

  // Returns a cube with filtered sides
  let private get_cube(incl_sides) = 
    [ for (side,trigs) in cubeMeshes do
        if Set.contains side incl_sides then
          yield! trigs ]
  
  // Pattern that represents the fractal
  //    [ [1; 1; 1]; [1; 0; 1]; [1; 1; 1] ];
  //    [ [1; 0; 1]; [0; 0; 0]; [1; 0; 1] ];
  //    [ [1; 1; 1]; [1; 0; 1]; [1; 1; 1] ];
  //
  // Created using the following rule:
  //   when 2 or more from [x,y,z] equal 1 then return 0 otherwise 1
  let private add_if_one n m = if (n=1) then m+1 else m
  let private pattern = Array3D.init 3 3 3 (fun x y z -> 
    let n = add_if_one x (add_if_one y (add_if_one z 0))
    if (n >= 2) then 0 else 1 )
  
  // Takes set representing included sides (around the current cube) and calls 'f'
  // for every sub-cube in the cube where pattern has '1', giving it coordinates  
  // and set with sides to include in the cube
  let private pattern_mapi incl_sides f =
  
    // Calculates set of sides to be included for specific location in the 
    // pattern. Uses 'incl_sides' for lookup when testing border sides and
    // uses the pattern for sides inside the pattern.
    let get_included_sides x y z = 
      seq { // loop over all possible 'directions',
            // test if out of range and use either pattern or 'incl_sides'
            for idx, neq, (dx, dy, dz) in 
                [ (x, 0, (-1,0,0)); (y, 0, (0,-1,0)); (z, 0, (0,0,-1));
                  (x, 2, ( 1,0,0)); (y, 2, (0, 1,0)); (z, 2, (0,0, 1)); ] do
              if ((idx <> neq && pattern.[x+dx, y+dy, z+dz] = 0) ||
                  (idx = neq  && Set.contains (dx, dy, dz) incl_sides)) then yield (dx, dy, dz) } |> set   
    
    // Generate list with all sides
    [ for x in [0 .. 2] do
        for y in [0 .. 2] do
          for z in [0 .. 2] do
            if (pattern.[x,y,z] = 1) then
              yield! f (x, y, z) (get_included_sides x y z) ]

  // In the first step we want to include all six sides  
  let private init_sides = 
    seq { for i in [-1; 1] do
            yield (i, 0, 0)
            yield (0, i, 0)
            yield (0, 0, i) } |> set
  
  // Exported/public members...
  let InitSides  = init_sides
  let PatternMap = pattern_mapi
  let GetCube    = get_cube


// ----------------------------------------------------------------------------------- //
// Main function for generating fractal recursively
             
let rec Generate k incl_sides = 
  if k = 1 then 
    (Internal.GetCube incl_sides) 
  else
    let d = k/3
    Internal.PatternMap incl_sides ( fun (x, y, z) incl_sides -> 
      (Generate d incl_sides) |> translate (x*d, y*d, -z*d)  )


// Number of repeats 
// (on my machine it runs very slowly with 4, but it loads after some time..)
let Repeats = 3

// Creates mesh representing the fractal  
let CreateFractal () =
  let three_pow = int (Math.Pow (3.0, float Repeats))
  
  // Generate point coordinates
  let grid_size = three_pow + 1
  let vec = [0.0 .. float (grid_size - 1)]  |> List.map ( fun n -> 
    n - (float (grid_size - 1)) / 2.0 ) 
  let mvec = vec |> List.map ( fun n -> -n ) 
  let pts = (cross_tup (cross_tup vec vec) mvec) |> List.map ( fun ((a, b), c) -> (a, b, c) )

  // Generate cubes
  let cubes = Generate three_pow Internal.InitSides
  
  // Build the mesh
  let fm = { Points = pts;  Triangles = to_mesh grid_size cubes; }
  (fm.Triangles.Length, fm.ToWpfMesh())
   
// ----------------------------------------------------------------------------------- //
// Create WPF window, set WPF properties, etc...

// Load window from XAML file
let CreateWindow (file : string) = 
  use stream = XmlReader.Create(file)
  XamlReader.Load(stream) :?> Window

// Main - start the application  
let Run() = 
  // create window
  let window = CreateWindow("..\\..\\fractal.xaml")
  let wp = window.FindName("view") :?> Viewport3D
  window.Content <- wp
  
  // create mesh in the window
  let br = new SolidColorBrush()
  br.Color <- Color.FromRgb(255uy, 255uy, 255uy)
  let vis = window.FindName("fractal") :?> ModelVisual3D
  let (trign, mesh) = CreateFractal ()
  vis.Content <- new GeometryModel3D(mesh, new DiffuseMaterial(br))
  window.Title <- window.Title + (String.Format(" (#triangles = {0})", trign))
  
  // run application
  let app = new Application()   
  window.Closed.Add( fun _ -> app.Shutdown(0) )
  app.Run(window) |> ignore