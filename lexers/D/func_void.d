
import std.stdio;
import std.concurrency;

shared string[] sources;

void thread1()
{
    foreach (string str; sources) {
        writeln("T1: ", str);
    }
}

void main()
{
    sources = [ "1", "2", "3" ];
    sources ~= "4";
    spawn(&thread1);
}

