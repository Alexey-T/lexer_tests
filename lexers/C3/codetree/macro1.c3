macro @foo(#a, b, #c)
{
    #c = #a(b) * b;
}

macro @foo2(#a)
{
    return #a * #a;
}

fn int square(int x)
{
    return x * x;
}

fn int test1()
{
    int a = 2;
    int b = 3;
    @foo(square, a + 1, b);
    return b; // 27
}

fn int printme(int a)
{
    io::printn(a);
    return a;
}

fn int test2()
{
    return @foo2(printme(2)); // Returns 2 and prints "2" twice.
}

macro long fib(long $n)
{
    $if $n <= 1:
        return $n;
    $else
        return fib($n - 1) + fib($n - 2);
    $endif
}
