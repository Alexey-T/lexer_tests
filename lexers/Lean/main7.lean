-- def twice (f : Nat → Nat) (a : Nat) :=
--   f (f a)

def twice (f : α -> α) := f ∘ f


def main: IO Unit := do

  IO.println (twice (.+ 5) 10)
  IO.println (twice (.* 5) 10)
