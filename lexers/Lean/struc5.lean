structure Point where
  x : Nat
  y : Nat

instance : ToString Point where
  toString p := toString p.x ++ "," ++ toString p.y

def add (p q : Point) : Point :=
  { x := p.x + q.x, y := p.y + q.y }

def Point.add (p q : Point) : Point :=
  { x := p.x + q.x, y := p.y + q.y }

def Point.add2 (p q : Point) : Point := {
   x := p.x + q.x
   y := p.y + q.y 
}

def Point.add3 (p q : Point) : Point where
   x := p.x + q.x
   y := p.y + q.y 


def main: IO Unit := do 

  println! Point.add (Point.mk 1 2) (Point.mk 3 3)
  println! Point.add2 (Point.mk 1 2) (Point.mk 3 3)
  println! Point.add3 (Point.mk 1 2) (Point.mk 3 3)

  println! add (Point.mk 1 2) (Point.mk 1 2)
