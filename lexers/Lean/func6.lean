def compose (α β γ : Type) (g : β → γ) (f : α → β) (x : α) : γ :=
  g (f x)

def compose' (α β γ : Type) (g : β → γ) (f : α → β) (x : α) : γ :=
  (g ∘ f) x

def double (x : Nat) := 2 * x
def triple (x : Nat) := 3 * x

def main: IO Unit := do

  IO.println (compose Nat Nat Nat double triple 10)
  IO.println (compose' Nat Nat Nat double triple 10)
