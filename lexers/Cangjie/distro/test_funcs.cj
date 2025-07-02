macro package mci.macros
import std.format.Formatter
// define Macro for testing, Assert float64 equal
import std.ast.*

public macro ExpectFloat64(input: Tokens): Tokens {
    if (input.size != 5) {
        throw Exception("ExpectFloat64: input size must be 5")
    }
    let input1 = input[0]
    let input2 = input[2]
    let eps = input[4]
    let result = quote(
        if (abs($(input1) - $(input2)) > $(eps)) {
            let got = $(input1)
            let ect = $(input2)
            let d = $(eps)
            // let msg = "${got} != ${ect} ± ${d}"
            // failExpect(msg)
            let pad = PowerAssertDiagramBuilder("Got float != Expect float ± Allowed eps by Difference", 0)
            pad.r(got, "Got float", 0)
            pad.r(ect, "Expect float", 13)
            pad.r(d, "Allowed eps", 28)
            pad.r(abs(got - ect), "Difference", 43)
            pad.w(false)
        }
    )
    return result
}
