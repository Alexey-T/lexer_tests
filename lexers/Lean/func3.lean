def vals := (. * 2) <$> [1,2,3]
def vals' := List.map (λ e => e * 2) [1,2,3]

#eval vals
#eval vals'
