﻿open System

// [snippet:Declaration of a Hello type]
/// Says hello to the specified name
type Hello(name) = 
  member x.SayHello() = 
    Console.WriteLine("Hello " + name)
  member x.SayHelloLoud() = 
    (*[omit:(implementation omitted)]*)
    let msg = ("Hello " + name + "!!!").ToUpper()
    Console.WriteLine(msg)(*[/omit]*)
// [/snippet]

do
  // [snippet:Application entry point]
  let hello = new Hello("F#")
  hello.SayHello()
  // [/snippet]