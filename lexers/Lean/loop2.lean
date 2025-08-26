def main: IO Unit := do

  let vals := [1, 2, 3, 4, 5]

  let n := vals.length
  let mut i := 0
  let mut sum := 0

  while i < n do 

    sum := sum + vals[i]!
    i := i + 1

  IO.println sum
