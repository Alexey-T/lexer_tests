#light

let sqr x = x * x
let multiply x y = x * y

print_int (sqr 3)
print_int (multiply 3 4)

// recursive function fibonacci series using pattern matching
let rec fib x =
    match x with
    | x when x <= 0 -> failwith "An integer greater than 0 is required."
    | 1 -> 1
    | 2 -> 1
    | x -> fib (x - 1) + fib (x - 2)
    
print_int (fib 15)

// functions as values
let add x y = x + y
let a1 = add 3
let a2 = a1 4

print_int a2
