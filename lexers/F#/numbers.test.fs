#light

open NUnit.Framework
open Numbers

[<TestFixture>]
type NumbersTest = class
    new() = {}
    
    /// Test to see that the factorial algorithm returns the expected value.
    [<Test>]
    member x.FactTest() =
        Assert.AreEqual(1, fact 1)
        Assert.AreEqual(720, fact 6)
        
    [<Test>]
    member x.FibTest() =
        Assert.AreEqual(8, fib 6)

end