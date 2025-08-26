structure User where
  name : String
  age  : Nat

instance : ToString User where
  toString p := p.name ++ "@" ++ toString p.age


def main : IO Unit := do

  let p := { name := "Leo", age := 44 : User }

  IO.println p.name
  IO.println p.age
  IO.println p

  let p: User := { name := "Peter", age := 23 }
  IO.println p

  let p: User := { 
    name := "Lucia"
    age := 29
  }
  
  IO.println p

  p |> IO.println
