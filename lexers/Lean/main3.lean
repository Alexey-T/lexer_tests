def main: IO Unit := do 

  let vals := [-3, -2, 0, -1, 2, 1, 3]
  
  for e in vals do
    if e < 0 then
      IO.println s!"{e} is negative"
    else if e > 0 then
      IO.println s!"{e} is positive"
    else
      IO.println s!"{e} zero"
