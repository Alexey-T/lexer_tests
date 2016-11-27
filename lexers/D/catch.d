int main(string[] args)
{
    typeof(fib(0)) n;
    auto errMsg = "";

    MyClass myClass = new MyClass;
    const auto x = myClass.fib(0);
    auto y = MyClass.fib2(1);
    immutable auto b = myClass.IsValid;

    if (args.length == 2) {
        try {
            n = args[1].to!(typeof(n))();
        }
        catch(Exception e) {
            errMsg = e.msg;
        }
    }
    else {
        errMsg = "Please enter one non-negative number.";
    }

}
