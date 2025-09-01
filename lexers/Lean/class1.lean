-- Define a typeclass 'Showable' with one method 'show'
class Showable (α : Type) where
  show : α → String

-- Implement 'Showable' for the 'Nat' type
instance : Showable Nat where
  show n := s!"This is a Nat: {n}"

-- Implement 'Showable' for the 'String' type
instance : Showable String where
  show s := s!"This is a String: {s}"

-- A function that uses the 'Showable' typeclass
def display {α : Type} (x : α) [Showable α] : String :=
  show x

-- Example usage
#eval display 42      -- "This is a Nat: 42"
#eval display "hello" -- "This is a String: hello"
