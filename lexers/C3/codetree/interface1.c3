import std::io;

// Define a dynamic interface
interface MyName
{
    fn String myname();
}

struct Bob (MyName) { int x; }

// Required implementation as Bob implements MyName
fn String Bob.myname(Bob*) @dynamic { return "I am Bob!"; }

// Ad hoc implementation
fn String int.myname(int*) @dynamic { return "I am int!"; }

fn void whoareyou(any a)
{
    MyName b = (MyName)a;
    if (!&b.myname)
    {
        io::printn("I don't know who I am.");
        return;
    }
    io::printn(b.myname());
}

fn void main()
{
    int i = 1;
    double d = 1.0;
    Bob bob;

    any a = &i;
    whoareyou(a);
    a = &d;
    whoareyou(a);
    a = &bob;
    whoareyou(a);
}