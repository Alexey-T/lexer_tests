def double (n : Nat) : Nat := n + n

def mand (a b : Bool) : Bool :=
  match a with
  | true  => b
  | false => false


def main : IO Unit := do

  IO.println (mand true true)
  IO.println (mand true false)
  IO.println (mand false true)
  IO.println (mand false false)

  IO.println (double 5)

  let res := List.map (λ x => toString x) [1, 2, 3]
  IO.println res

  let res2 := [1, 2, 3, 4, 5, 6, 7].map (λ x => x + 1)
  IO.println res2

  for e in List.range 10 do
    IO.println e

  let data := (List.range 10).map (λ x => x * x)
  IO.println data
