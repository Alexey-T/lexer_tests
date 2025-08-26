variable (α β γ : Type)

def doTwice (h : α → α) (x : α) : α :=
  h (h x)

def doThrice (h : α → α) (x : α) : α :=
  h (h (h x))

def main: IO Unit := do

  let n := 4

  -- specify type following function name
  let res := doTwice Nat (fun e => e + 1) n
  IO.println res

  let res := doThrice Nat (.+ 5) 10
  IO.println res
