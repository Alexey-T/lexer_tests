def sum : List Nat -> Nat
  | []     => 0
  | h :: t => h  + sum t


def main: IO Unit := do

  let vals := [1, 2, 3, 4, 5]

  let res := sum vals
  IO.println res
