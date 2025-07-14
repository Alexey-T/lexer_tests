// Fixed Size 1D Arrays
// 
// These are declared as <type>[<size>], e.g. int[4]. 
// Fixed arrays are treated as values and will be copied if given as parameter. 
// Unlike C, the number is part of its type. 
// Taking a pointer to a fixed array will create a pointer to a fixed array, e.g. int[4]*.
// 
// Unlike C, fixed arrays do not decay into pointers. 
// Instead, an int[4]* may be implicitly converted into an int*.
import std::io;
fn void main() {
 //  int x[3] = { 1, 2, 3 };
//   foo(x);
   int[3] x = { 1, 2, 3 };
   //io::printn(bar(x)); // error
   io::printn(bar(&x));
   int r = foo0(x);
   assert(r == 1);

   io::printn(r);
   io::printn(foo1(&x));
   foo2(&x);

   io::printn(x[0]); // print 42
   assert(x[0] == 42);
   foo3(x);
   io::printn(x[0]); // print 42
   assert(x[0] == 42);
}

fn int bar(int[] a) {
   return a[0];
}
fn int foo0(int[3] a) {
   return a[0];
}
// C3
fn int foo1(int* a) {
   return a[0];
}
fn void foo2(int* a) {
    a[0] = 42;
}
fn void foo3(int[3] a) {
    a[0] = 64;
}
