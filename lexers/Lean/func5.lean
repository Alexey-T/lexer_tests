def double : Nat → Nat := fun
  | 0 => 0
  | k + 1 => double k + 2

def double2 : Nat → Nat :=
  fun x => x + x

def main: IO Unit := do

  let r := double 10
  IO.println r

  IO.println (double 2 + 1)
  IO.println (double2 12)
