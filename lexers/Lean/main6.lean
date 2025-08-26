open Nat

-- def swap : α × β → β × α
--   | (a, b) => (b, a)

def isZero : Nat → Bool
  | zero   => true
  | succ x => false

def fib : Nat -> Nat
  | 0 => 0
  | 1 => 1
  | n+2 => fib (n+1) + fib n

def main : IO Unit := do

  IO.println (fib 10)
  IO.println (fib 20)

  IO.println (isZero 10)
  IO.println (isZero 0)
  IO.println (isZero 4)
