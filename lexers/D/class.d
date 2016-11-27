class MyClass { // Class definition ignored...
    private bool isValid = false;

    public static ulong fib(in ulong n) @safe { // fib ignored
        return n < 2 ? n : fib(n - 1) + fib(n - 2);
    }

    private ulong fib2(in ulong n) { // fib2 ignored
        return n < 2 ? n : fib(n - 1) + fib(n - 2);
    }

    @property bool IsValid() const { return isValid; } // property ignored
}

pure nothrow ulong fib(in ulong n) @safe // still ignored...
{
    return n < 2 ? n : fib(n - 1) + fib(n - 2);
}

