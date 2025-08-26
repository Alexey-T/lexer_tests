def add (x y : Nat) :=
  x + y

def add' (x : Nat) (y : Nat) :=
  x + y

def main: IO Unit := do 

  IO.println (add 2 3) 
  IO.println (add' 12 13)
