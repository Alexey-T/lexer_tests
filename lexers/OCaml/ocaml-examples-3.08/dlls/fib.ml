(* File fib.ml: define some Caml functions. *)

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2);;

let format_result n = Printf.sprintf "Result is: %d\n" n;;

(* Export those two functions to C *)

Callback.register "fib" fib;;
Callback.register "format_result" format_result;;
