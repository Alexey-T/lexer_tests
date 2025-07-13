// Function and variable aliases
//

//alias can also be used to create aliases for functions and variables.
//The syntax is alias <alias> = <original identifier>.
import std::io;

fn void foo() { 
   io::printn("inside foo");
}
int foo_var;

alias bar = foo;
alias bar_var = foo_var;

fn void test() {
  // These are the same:
  foo();
  bar();

  foo_var = 42;
  // These access the same variable:
  int x = foo_var;
  int y = bar_var;
  io::printfn("x:%d, y:%d", x, y);
  // error
  //bar_var= 64;
}

fn void main(String[] args) {
   test();
}
