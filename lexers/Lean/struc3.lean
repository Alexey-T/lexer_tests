structure User where
  name : String  
  occupation : String

instance: ToString User where
  toString p := p.name ++ ": " ++ p.occupation

-- def User.make (_name : String) (_occupation: String): User :=
--   { name := _name, occupation := _occupation}

def User.make (_name _occupation: String): User :=
  { name := _name, occupation := _occupation}

def u1 := User.mk "John Doe" "gardener"
def u2 := User.make "Roger Roe" "driver"

def main: IO Unit := do 
  println! u1
  println! u2
