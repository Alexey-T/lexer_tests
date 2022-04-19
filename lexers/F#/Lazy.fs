#light

// A heads up, those who have used Haskell will be familiar with the implicit
// use of lazy evaluation.  In F# lazy evaluation required explicit signature typing.

// Lazy evaluation allows a computation to not be computed until required.  For those coming
// from a C# background a good example is a LINQ expression, when the GetEnumerator is called 
// the query is executed returning some IEnumerable (or in F#'s case seq (sequence)).

let myComputation = lazy (15 + 1) // the addition of 15 + 1 has not been evauated, it just sits there in memory
let myComputationComputed = Lazy.force myComputation // tell the compiler to evaluate the lazy expression myComputation
print_int myComputationComputed // 16  