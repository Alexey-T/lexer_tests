// Struct Methods

// It’s possible to namespace functions with a union, struct or enum type to enable “dot syntax” calls:

import std::io;
struct Foo {
    int i;
}

fn void Foo.next(Foo* this) {
    if (this) this.i++;
}

fn void test() {
    Foo foo = { 2 };
    foo.next();
    foo.next();
    // Prints 4
    io::printfn("%d", foo.i);
}

fn void main(String[] args) {
   test();
    Foo foo = { 42 };
    foo.next();
    assert(foo.i == 43);
}
