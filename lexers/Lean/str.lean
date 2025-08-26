def main: IO Unit :=
  let name := "John Doe"
  let age := 34

  let msg := s!"{name} is {age} years old"
  IO.println msg

def main2 : IO Unit := do

  let msg := "an old falcon"
  let msg := msg |>.replace "old" "young" |>.replace "an" "a"

  println! msg

def main3: IO Unit := do

  let word := "falcon"
  IO.println (String.length word)

  let word2 := "вселенная"
  IO.println (String.length word2)

  let word3 := "नमस्ते" -- cannot handle
  IO.println (String.length word3)
