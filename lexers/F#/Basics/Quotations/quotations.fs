#light

// Contains active patterns like 'Let', 'Int32', etc..
open Microsoft.FSharp.Quotations.Raw

// Contains <@ .. @> operator
open Microsoft.FSharp.Quotations.Typed

// List of expressions representing binary operators and their names
let binaryOperators = 
  [ <@@ ( + ) @@>, " + ";
    <@@ ( * ) @@>, " * ";
    <@@ ( - ) @@>, " - ";
    <@@ ( / ) @@>, " * "; ]

// Custom 'active pattern' that matches with any binary operator
// and returns type arguments, both sides of application and string from 
// the list defined above
let (|BinaryOperator|_|) expr =
  binaryOperators |> List.first (fun (op, str) ->
    match expr with
    | GenericTopDefnApp op (ty, [a;b]) -> Some(ty, (a,b), str)
    | _ -> None )
  
// Remove number from a variable name in quotation 
// e.g. 'foo#123.4' will be printed as 'foo' (in general this can cause 
// ambigous code, but it makes the output more readable)
let makeNiceVar (str:string) = 
  let i = str.IndexOfAny([|'$'; '#'|])
  str.Substring(0, i)
  
// Prints F# quotation..
let rec print spaces expr = 
  let print_noind = print spaces
  let print = print (spaces + "  ")
  
  match expr with
  | Let((var,value), body) -> 
      // Represents: let <var> = <value> in <body>
      printf "%slet %s = " spaces (makeNiceVar var.Text)
      print value
      printfn " in"
      print_noind body

  | String(s) ->
      // Represents a string
      printf "\"%s\"" s
      
  | Int32(n) ->
      // Represents a number
      printf "%d" n

  | Double(d) ->
      // Represents a number
      printf "%f" d
      
  | BinaryOperator (_, (left, right), str) ->
      // Represents binary operator: (<left> o <right>)
      print left
      printf "%s" str
      print right

  | Var(v) ->
      // Represents variable reference
      printf "%s" (makeNiceVar v.Text)
      
  | Seq(first, second) -> 
      // Represents sequence expression: <first>; <second>
      printf "%s" spaces
      print first
      printfn ";"
      printf "%s" spaces
      print second
      
  | Lambda(var, body) -> 
      // Represents lambda function declaration (with one argument)
      printfn "(fun %s -> " (makeNiceVar var.Text)
      print body
      printf "\n%s)" spaces
      
  | App(x,y) -> 
      // Represents generic application - functional function call
      printf "App("
      print x
      printf ", "
      print y
      printf ")"
      
  | AnyTopDefnUse (tdd, ty) ->      
      // Global value reference (e.g. F# function in a module)
      printf "%s" (snd tdd.Path)
  
  | Unit() ->
      // Represents unit value '()'
      printf "()"
            
  | x -> 
      printfn "\n\n!! Unknown value: %A\n" x
 
 
// Sample expression that we will process in this example 
let expr = 
  <@
    // lambda function calculating square with side effect
    let sqr (x:double) = 
      print_string "calculating..."
      x*x
    
    // application of 'sqr' to '3.0'
    let nine = sqr 3.0
    
    // some more sample applications
    print_float nine
    read_line()
  @>

// Sample top-level value marked using ReflectedDefinition attribute
// .. this means that we can access its quotation at runtime
[<ReflectedDefinition>]
let expr_topdef () =
  print_string "Running 'expr_topdef'!"
  
  // lambda function calculating square with side effect
  let sqr (x:double) = 
    print_string "calculating..."
    x*x
  
  // application of 'sqr' to '3.0'
  let nine = sqr 3.0
  
  // some more sample applications
  print_float nine
  read_line()

let main() =
  // Get 'Raw' (underlying representation that can be processed using 
  // active patterns) quotation and print it!
  
  printfn "-- first sample --"
  print "" expr.Raw
  printfn "\n"
  
  printfn "-- second sample --"  
  // Get top level definition
  let tdd = 
    match <@@ expr_topdef @@> with 
    | AnyTopDefnUse(td) -> td 
    | _ -> failwith "Failed to match with top defn"
  let expr =
    match (ResolveTopDefinition tdd) with
    | Some e -> e
    | None -> failwith "Failed to get quotation!"
  print "" expr
  read_line()