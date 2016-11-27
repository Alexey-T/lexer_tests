@safe nothrow pure ulong fib_ok(ulong n)
{
    return n < 2 ? n : fib_ok(n - 1) + fib_ok(n - 2);
}

@safe nothrow ulong fib_bad(ulong n) pure
{
    return n < 2 ? n : fib_bad(n - 1) + fib_bad(n - 2);
}

/*
 * D functions can have (some combinations) of these attributes:
 * @safe, @trusted, @system, pure, nothrow.
 * As shown in the example, if at least one appears after the standard
 * declaration, tree do not recognize the f anymore.
 * The tree here should be:
 *
 * - ulong fib_ok(ulong n)
 * - ulong fib_bad(ulong n)
 *
 * I think the tree should take chars inside () surrounded by () preceded by
 * the previous 2 (or more) tokens (return type and func name).
 *
 * Hope this help.
 */

