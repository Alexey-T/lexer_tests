// Automatically unwrapping an Optional result
//
//
//If we escape the current scope from an if (catch my_var) using a
//return, break, continue or Rethrow !,
//then the variable is automatically unwrapped to a non-Optional
//
//https://c3-lang.org/language-common/optionals-essential/

import std::io;
fn int? unreliable_function() {
   return 7;
}

fn void? test() {
    int? foo = unreliable_function();
    if (catch excuse = foo)
    {
        // Return the excuse with `?` operator
        return excuse?;
    }
    // Because the compiler knows 'foo' cannot
    // be empty here, it is unwrapped to non-Optional
    // 'int foo' in this scope:
    io::printfn("foo: %s", foo); // 7
}

fn void main(String[] args) {
   //  test();
   //  ^^^^^^
   // (/home/o3o/c3/c3lab/optional-essential/ex2.c3:26:4)
   // Error: The function returns 'void?', which is an optional and must be handled.
   // You can either assign it to a variable, rethrow it using '!', panic with '!!', use if-catch etc.
   // You can also silence the error using a void cast (e.g. '(void)the_call()') to ignore the error.

   test()!!;
   // or
   //(void)test();
}
