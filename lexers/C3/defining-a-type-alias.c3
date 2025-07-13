// Defining a type alias

// alias <type alias> = <type> creates a type alias. 
// A Type alias needs to follow the naming convention of user defined types (i.e. capitalized name with at least one lower case letter).

// alias CharPtr = char*;
// alias Numbers = int[10];
//
// Function pointers must be aliased in C3. The syntax is somewhat different from C:
//
// alias Callback = fn void(int a, bool b);
//
// This defines an alias to function pointer type of a function that returns nothing and requires two arguments: an int and a bool. 
// Here is a sample usage:
// Callback cb = my_callback;
// cb(10, false);
//
import std::io;

alias Integer = int;
alias Callback = fn int(Integer a, bool b);

fn Integer sum2(Integer i, bool b) {
   if (b) {
      return i + 2;
   } else {
      return 0;
   }
}

fn int mul2(Integer i, bool b) {
   if (b) {
      return i * 2;
   } else {
      return 0;
   }
}


fn void main() {
   Callback s = &sum2;
   io::printfn("sum %d", s(3, true));

   Callback m = &mul2;
   io::printfn("mul %d", m(3, true));
}
