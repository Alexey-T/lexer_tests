// Function pointer default arguments and named parameters
// 
// It is possible to attach default arguments to function pointer aliases. 
// There is no requirement that the function has the same default arguments. 
// In fact, the function pointer may have default arguments where the function doesn’t have it and vice-versa. 
// Calling the function directly will then use the function’s default arguments, 
// and calling through the function pointer will yield the function pointer alias’ default argument.
// 
// Similarly, named parameter arguments follow the alias definition when calling through the function pointer:

import std::io;
alias TestFn = fn void(int y = 123);

fn void test(int x = 5) {
    io::printfn("X = %d", x);
}

fn void main() {
    TestFn test2 = &test;
    test();         // Prints X = 5
    test2();        // Prints X = 123
    test(x: 3);     // Prints X = 3
    // test(y: 3);  Error: There's no parameter with the name 'y'.
    test2(y: 4);    // Prints X = 4
}
