#light
#nowarn "57"
#nowarn "60"

open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Raw

// ------------------------------------------------------------------
// Type for storing simple expressions
// It supports only integral values and only basic operations
// (addition, subtraction, multiplication and division)

type SimpleExpr =
  | Int of int
  | Add of SimpleExpr * SimpleExpr
  | Sub of SimpleExpr * SimpleExpr
  | Mul of SimpleExpr * SimpleExpr
  | Div of SimpleExpr * SimpleExpr

// ------------------------------------------------------------------
module SimpleExprFuncs = 

  // Recursive function for evaluationg value of simple expression
  let rec eval = fun e ->
    match e with
      | Int i -> i
      | Add (x,y) -> eval x + eval y
      | Sub (x,y) -> eval x - eval y
      | Mul (x,y) -> eval x * eval y
      | Div (x,y) -> eval x / eval y
    
  // Recursive function for converting SimpleExpr to string 
  let rec tostring e = 
    match e with
      | Int n -> n.ToString()
      | Add (x,y) -> "("^(tostring x)^" + "^(tostring y)^")"
      | Sub (x,y) -> "("^(tostring x)^" - "^(tostring y)^")"
      | Mul (x,y) -> "("^(tostring x)^" * "^(tostring y)^")"
      | Div (x,y) -> "("^(tostring x)^" / "^(tostring y)^")"

  // Prints simple expression
  let rec print e = print_string (tostring e)

  // Parse function converts F# quotation (that uses only integers and 
  // basic mathematical operations) to 'SimpleExpr' representation
  let rec parse x =

    // x is int - returns simple expression with same number
    match x with  
      | Int32 (x) -> Int(x)
      | TopDefnApp <@@ ( + ) @@> [x1;x2] -> Add(parse x1, parse x2)
      | TopDefnApp <@@ ( - ) @@> [x1;x2] -> Sub(parse x1, parse x2)
      | TopDefnApp <@@ ( * ) @@> [x1;x2] -> Mul(parse x1, parse x2)
      | TopDefnApp <@@ ( / ) @@> [x1;x2] -> Div(parse x1, parse x2)
      | _ -> 
        // something else than efApps and efInt32 - error
        failwith ("Not supported construction in expression.")
      

// ------------------------------------------------------------------
// Augmentations for the 'SimpleExpr' type, so functions can 
// be accessed via the 'dot' notation

open SimpleExprFuncs

type SimpleExpr with
  static member Parse(s) = parse s
  override x.ToString() = tostring x
  member x.Print() = print x
  member x.Eval() = eval x
    
// ------------------------------------------------------------------
// program body

let main() =
  try
    // parse f# quotation
    let my_quot = <@@ (2*3 + 10/5)/2 - 2 @@>
    let my_parsed = SimpleExpr.Parse(my_quot)
    
    printfn "Expression: %s" (my_parsed.ToString())
    printfn "Evaluated: %i" (my_parsed.Eval())
  with e -> 
    printf "Error: %A" e
    
  read_line()