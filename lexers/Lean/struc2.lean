structure User where
  make ::
  name : String
  occupation : String

instance : ToString User where 
  toString u := s!"{u.name} {u.occupation}"

def u := User.make "John Doe" "gardener"

def main: IO Unit := println! u
