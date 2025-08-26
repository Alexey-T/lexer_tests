notation "ℕ" => Nat

example : 2 + 13 ≠ 5 := by decide

def divide (x : ℕ) (y : { n : ℕ // n ≠ 0 }) : ℕ := x / y
#check divide 4 ⟨3, by decide⟩

def main : IO Unit := do

  let r := divide 4 ⟨2, by decide⟩
  println! r
