#light

let people = ["Granville"; "John"; "Rachel"; "Michael"; "Monica"]

// List.iter provides a simple way to act upon each element within a List
let printPeople p =
    List.iter print_string p

printPeople people

let addAFewMore = people @ ["Joey"; "Ross"; "Chandler"; "Phoebee"]

printPeople addAFewMore

let numbers = [[1;2;3];[4;5;6]]

// pattern matching is like haskell, however we use a few new keywords like match and with.
// to access the head and tail of the list we use :: instead of : in haskell.
let rec concat l = 
    match l with
    | head::tail -> head @ (concat tail)
    | [] -> []
    
concat numbers
    
let rec concatFun =
    function head::tail -> (head + " ") + concatFun tail
           | [] -> ""

concatFun people
    
