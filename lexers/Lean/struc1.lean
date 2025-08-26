structure User where
  name : String
  age  : Nat

instance : ToString User where
  toString : User -> String
    | { name := n, age := v } => s!"\{ name := {n}, age := {v} }"

def user : User where
  name := "John Doe"
  age  := 28

def main : IO Unit := do
  println! user
