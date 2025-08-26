def sum (xs : List Nat) :=
  xs.foldl (init := 0) (·+·)

def sum' (xs : List Nat) :=
  xs.foldl (init := 0) (λ x y => x + y)

#eval sum [1, 2, 3, 4]
#eval sum' [1, 2, 3, 4]
