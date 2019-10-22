"""
# Itertools Package

The itertools package provides the `Iter` class for doing useful things with
iterators. It is Inspired by Python's itertools library, Rust's Iterator, and
Elixir's Enum and Stream.

## Iter

The Iter class wraps iterators so that additional methods may be applied to it.
Some methods, such as fold and collect, run through the underlying iterator in
order to return a result. Others, such as map and filter, are lazy. This means
that they return another Iter so that the resulting values are computed one by
one as needed. Lazy methods return Iter types.

For example, the following code creates an Iter from the values of an array
containing the numbers 1 through 5, increments each number by one, filters out
any odd numbers, and prints the rest.

```pony
let xs = Iter[I64]([1; 2; 3; 4; 5].values())
  .map[I64]({(x) => x + 1 })
  .filter({(x) => (x % 2) == 0 })
  .map[None]({(x) => env.out.print(x.string()) })
```

This will result in an iterator that prints the numbers 2, 4, and 6. However,
due to the lazy nature of the map and filter, no iteration has actually occurred
and nothing will be printed. One solution to this would be to loop over the
resulting Iter as so:

```pony
for x in xs do
  None
end
```

This will trigger the iteration and print out the values 2, 4, and 6. This is
where the `run` method comes in handy by doing the iteration without the need
for a loop. So the final code would be as follows:

```pony
Iter[I64]([1; 2; 3; 4; 5].values())
  .map[I64]({(x) => x + 1 })
  .filter({(x) => (x % 2) == 0 })
  .map[None]({(x) => env.out.print(x.string()) })
  .run()
```

Output:

```
2
4
6
```
"""
