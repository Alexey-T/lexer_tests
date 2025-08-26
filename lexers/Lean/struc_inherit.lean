structure Point where
  x : Nat
  y : Nat

inductive Color where
  | red
  | green
  | blue

structure ColorPoint extends Point where
  color : Color

def Color.str (c : Color) : String :=
  match c with
  | red => "red"
  | green => "green"
  | blue => "blue"

instance : ToString ColorPoint where
  toString : ColorPoint -> String
    | { x := n, y := v, color := c } => s!"\{ x := {n}, y := {v}, color := {c.str} }"

def main: IO Unit := do 

  println! { x := 10, y := 20, color := Color.red : ColorPoint }
  println! Color.red.str
