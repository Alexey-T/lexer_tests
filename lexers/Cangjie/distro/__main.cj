package simple_math_interpreter

import std.console.*
import simple_math_interpreter.ext.*

main() {
    println(
        """
###################################################
##  Enter an simple math expression.             ##
##  e.g.                                         ##
##  3 + 2 * (99 + 101) + 7                       ##
##  1+(1 -1) + 6* 30/9                           ##
##                                               ##
##  Press Ctrl + C to exit                       ##
###################################################
"""
    )

    while (true) {
        Console.stdIn.readln().okOr("Invalid Input!").andThen(eval).map(println).mapErr(println)
    }
}
