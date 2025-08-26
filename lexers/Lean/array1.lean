def main : IO Unit := do

  let as := mkArray 5 'a'
  IO.println as

  let zeros := mkArray 5 0
  IO.println zeros

  let ones := zeros.map (λ e => e + 1)
  IO.println ones

  IO.println "--------------------"

  let vals := #[1, 2, 3, 4, 5]
  IO.println vals
  IO.println vals[0]
  IO.println vals[1]
  IO.println vals[11]? -- print none if idx does not exist

  for e in vals do 
    IO.println e
