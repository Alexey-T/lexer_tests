#light

/// Computes factorial for the given integer.
let rec fact n =
    match n with
    | 1 -> 1
    | n -> n * fact (n - 1)

/// Computes fibonacci number for given integer.
let rec fib n =
    match n with 
    | 1 -> 1
    | 2 -> 1
    | n -> fib (n - 1) + fib (n - 2)