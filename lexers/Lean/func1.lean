def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

def main: IO Unit := do

  IO.println (maximum 10 5)
  IO.println (maximum 10 11)
  IO.println (maximum 12 11)
  IO.println (maximum (12 - 1) (7 + 4))
