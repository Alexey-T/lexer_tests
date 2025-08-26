def main : IO Unit := do

  let a := Array.mk [1, 2, 3, 4, 5]

  -- makes a copy of a (since a is still referenced)
  -- and modifies element in position 3
  let b := a.set! 3 44
  let b := b.set! 4 55  -- modifies b in-place

  IO.println a
  IO.println b
