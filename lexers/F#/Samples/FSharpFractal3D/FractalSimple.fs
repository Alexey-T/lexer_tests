module FractalSimple

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
// NOTE:
//   This is a simplified version which doesn't remove unnecesary triangles
//   when placing two cubes side-by-side

let repeats = 3

// Some F# types to represent points, triangles, etc..
type point3d = float * float * float
type pointi3d = int * int * int
type triangle = point3d * point3d * point3d
type trianglei = pointi3d * pointi3d * pointi3d
type triindex = int * int * int

// Mesh and cube representation
type mesh =  { points : point3d list;  triangles : triindex list; }
type cubestruct = { Front : trianglei list; Right : trianglei list; Top : trianglei list; 
                    Back: trianglei list; Left : trianglei list; Bottom : trianglei list; }   
    
// Creates WPF mesh from F# 'mesh'    
let create_mesh (m:mesh) =
  let ret = new MeshGeometry3D()  
  m.points |> List.iter (fun (x,y,z) -> 
    ret.Positions.Add(new Point3D(x, y, z)) )
  m.triangles |> List.iter (fun (a,b,c) ->
    ret.TriangleIndices.Add(a)
    ret.TriangleIndices.Add(b)
    ret.TriangleIndices.Add(c) )
  ret

// Cross-product taking two lists as parameters
let cross_tup ls1 ls2 =    
  ls2 |> List.map ( fun el2 -> ls1 |> List.map ( fun el1 -> (el1, el2) ) ) |> List.concat


// Pattern that represents the fractal
let pattern = 
    [|
      [| [|1; 1; 1|]; [|1; 0; 1|]; [|1; 1; 1|] |];
      [| [|1; 0; 1|]; [|0; 0; 0|]; [|1; 0; 1|] |];
      [| [|1; 1; 1|]; [|1; 0; 1|]; [|1; 1; 1|] |];
    |]

// Pattern stored as a lists
let aa_t_ll = fun a -> a |> Array.toList |> List.map (Array.toList)
let pattern_list = pattern |> ( fun a -> a |> Array.toList |> List.map (aa_t_ll) )
    
// Cube constructed using triangles    
let cube = 
        { Front  = [((1, 1, 0), (0, 1, 0), (1, 0, 0)); ((0, 1, 0), (0, 0, 0), (1, 0, 0)); ];
          Right  = [((1, 1, -1), (1, 1, 0), (1, 0, -1)); ((1, 1, 0), (1, 0, 0), (1, 0, -1)); ];
          Top    = [((1, 1, -1), (0, 1, -1), (1, 1, 0)); ((0, 1, -1), (0, 1, 0), (1, 1, 0)); ];
          Back   = [((1, 1, -1), (1, 0, -1), (0, 1, -1)); ((0, 1, -1), (1, 0, -1), (0, 0, -1)); ];
          Left   = [((0, 1, -1), (0, 0, -1), (0, 1, 0)); ((0, 1, 0), (0, 0, -1), (0, 0, 0)); ];
          Bottom = [((1, 0, -1), (1, 0, 0), (0, 0, -1)); ((0, 0, -1), (1, 0, 0), (0, 0, 0)); ]; }
let cube_list = [cube.Front;cube.Right;cube.Top;cube.Back;cube.Left;cube.Bottom]          
      
// Translate triangles
let translate (d:int * int * int) (pts:trianglei list) =
  let tpt (dx, dy, dz) ((x, y, z):pointi3d) = (x + dx, y + dy, z + dz)
  pts |> List.map ( fun (pt1, pt2, pt3) -> (tpt d pt1, tpt d pt2, tpt d pt3) )
  
let pattern_mapi f =
  let c = pattern_list |> List.mapi ( fun y l -> l |> List.mapi ( fun z l -> l |> List.mapi ( fun x v -> 
    f (x, y, z) v ) ) )    
  c |> List.concat |> List.concat |> List.concat

let cut_cube (f,r,t,k,l,b) = 
  List.fold2 (fun list incl trig ->
    match incl with
      | true -> list@trig
      | false -> list ) [] [f;r;t;k;l;b] cube_list
      
let rec generate k cut = 
  if k = 1 then (cut_cube cut) else
    let d = k/3
    pattern_mapi ( fun (x, y, z) v -> 
      match v with
        | 1 -> (generate d cut) |> translate (x*d, y*d, -z*d)
        | _ -> [] )
        
let to_mesh grid_size (trigs:trianglei list) = 
  let idx (x, y, z) =
    (((-z) * grid_size) + y) * grid_size + x
  trigs |> List.map ( fun (p1, p2, p3) -> (idx p1, idx p2, idx p3) )
  

// Creates mesh representing the fractal  
let create_fractal_mesh =
  let three_pow = (int (Math.Pow (3.0, float repeats)))
  let grid_size = three_pow + 1
  let vec = [0.0 .. (float (grid_size - 1))] |> List.map ( fun n -> n - (float (grid_size - 1)) / 2.0 ) 
  let mvec = vec |> List.map ( fun n -> -n ) 
  let pts = (cross_tup (cross_tup vec vec) mvec) |> List.map ( fun ((a, b), c) -> (a, b, c) )
  
  let sides = (true,true,true,true,true,true)
  let cubes = generate three_pow sides
  let fm = { points = pts;  triangles = to_mesh grid_size cubes; }
  (fm.triangles.Length, create_mesh fm)
   
(*************************** WPF manipulation ***************************)

// Load window from XAML file
let CreateWindow(file : string) = 
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
  let (trign, mesh) = create_fractal_mesh
  vis.Content <- new GeometryModel3D(mesh, new DiffuseMaterial(br))
  window.Title <- window.Title + (String.Format("(trig = {0})", trign))
  
  // run application
  let app = new Application()   
  window.Closed.Add( fun _ -> app.Shutdown(0) )
  app.Run(window) |> ignore
  