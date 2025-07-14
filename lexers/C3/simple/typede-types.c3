// typedef types
//
// A typedef creates a new type.
// A typedef does not implicitly convert to or from any other type, unlike a type alias.
// Literals will convert to the typedef types if they would convert to the underlying type.
//
// Because a typedef type is a new type, it can have its own methods, like any other user-defined type.
//
typedef Foo = int;
fn void typedef_types() {
   Foo f = 0; // Valid since 0 converts to an int.
   f = f + 1;
   int i = 1;
   // f = f + i Error!
   f = f + (Foo)i; // Valid
}

typedef CString = char*;
typedef ZString = inline char*;

//typedef inline
//
//When interacting with various APIs it is sometimes desirable for typedef types to implicitly convert to its base type,
//but not from that type.
//Behaviour here is analogous how structs may use inline to create struct subtypes.
fn void typedef_inline() {
   CString abc = "abc";
   ZString altro = "alias";
   //char* from_abc = abc; // Error!
   char* from_def = altro; // Valid!
}


fn void main() {}
