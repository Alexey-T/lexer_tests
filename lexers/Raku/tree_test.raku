grammar Calculator {
    token TOP { [ <add> | <sub> ] }
    rule  add { <num> '+' <num> }
    rule  sub { <num> '-' <num> }
    token num { \d+ }
}

grammar BetterCalculator is Calculator {
    rule calc-op:sym<mult> { <num> '*' <num> }
}
 
class BetterCalculations is Calculations {
    method calc-op:sym<mult> ($/) { make [*] $<num> }
}
 
say BetterCalculator.parse('2 * 3', actions => BetterCalculations).made;

grammar Foo {
    token TOP { <letter>+ }
    proto token letter {*}
          token letter:sym<R> { <sym> }
          token letter:sym<a> { <sym> }
          token letter:sym<k> { <sym> }
          token letter:sym<u> { <sym> }
          token letter:sym<*> {   .   }
}.parse("I ♥ Raku", actions => class {
    method TOP($/) { make $<letter>.grep(*.<sym>).join }
}).made.say; # OUTPUT: «Raku␤»

grammar Digifier {
    rule TOP {
        [ <.succ> <digit>+ ]+
    }
    token succ   { <?> }
    token digit { <[0..9]> }
}
 
class Devanagari {
    has @!numbers;
    method digit ($/) { @!numbers.tail ~= <०  १  २  ३  ४  ५  ६  ७  ८  ९>[$/] }
    method succ  ($)  { @!numbers.push: ''     }
    method TOP   ($/) { make @!numbers[^(*-1)] }
}