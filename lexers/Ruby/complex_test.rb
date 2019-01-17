# Rationals are a recent addition to the language
1e3  # Should be entirely purple like this
1/5r

# Regexes
/^xyz\Z/  # Should be yellow like this line (or orange, as in Sublime?)
['xyz' =~ /^xyz\Z/]
'xyz'[/^xyz\Z/]
/^xyz\Z/ =~ 'xyz'
/whitespace
regex
/x
/[]/
the above renders everything as a regex until the next ]/

# Heredocs are quite fragile, the last one is probably too complex to fix though
# you can eval this snippet in ruby to see how it should parse
p <<-end + 'world'
hello
end
p <<"x"
#{'y'}
x
p <<'z'
#{normal} string
z
p <<A, <<-B, <<-c  # comment
 Atext
A
 Btext
 B
 Ctext
c

# Ternary operator vs symbols/char literals
ternary ? yes : no  # Same as Sublime, but shouldn't both ? and : be red?
ternary ? yes:no
ternary ?[yes]:no

# Method arguments
def x *block   # The asterisk/ampersand is red in Sublime, orange looks fine though
def x(*block)
def x(&block)

# Backticks
Kernel.`()
the above renders everything a subshell call until the next `

# Class definition
class Foo::Bar  # Shouldn't :: be white to match below?
Foo::Bar.x      # Or should :: be cyan to match above?
module Foo::Bar
class const_get(:Foo)::Bar

# Module stuff
module Foo  # Should be green & underlined
  include SomeModule  # Anything following include should be cyan as below
  fail    SomeError   # same with raise/fail
  include SomeModule.x
  include Foo::Bar.!  # Bar should be cyan
end

# Keywords
[].prepend x  # This should be white or cyan, not red
[].include x  # there are lots of these, I think .new should be the only exception, if any
self          # Should be red, even though Sublime leaves it white this IS a keyword
__DIR__       # These keywords should all be red, there aren't many of them though
__method__    # The lowercase methods could be either white/red
__END__       # Adding a space or anything after this statement removes its unique styling

###################################################################################################

# Methods
# Not all methods have been assigned cyan, so it's really inconsistent
binding, gets, append, [select, reject], tr, gsub, read, [load, dump], p
# I understand stuff like .new, raise etc being red to add interest, but the addition of cyan
# seems completely arbitrary (esp. select, sub etc which aren't Kernel methods -
# Kernel#load is a stretch too as it's much more commonly used in JSON.load and various gems)

# Personal opinion: Methods should be white, not cyan, then both constants and classes can be cyan,
# purple being reserved only for true/false and escape sequences - this is what Sublime does

# Reasoning: All of these are classes/modules, which should ideally be cyan
NewError = Class.new  # Could either be a class or a constant when assigning
Contract Num => Num   # All classes, though very unconventional code it's a real library
Module::Module2       # Could either be a module or a constant
JSON.parse            # This is how it should look, the class is cyan, the method is white
Pathname()  # Note that methods can start with capital letters too, not just constants
Array()     # This very common method...
Hash[]      # ...just looks more normal in cyan to me, shows that it's from a class at least

# Purple is best kept for escape sequences/symbols
true false ?\s %i[symbol array] 100.0 0xff
# or it could be used for all caps constants, which works nicely here
DEFNITELY_A_CONSTANT = 5
Math::PI
MODULE::CONSTANT
# maybe even special cases for built in globals like ARGV/DATA/$1/$>
ARGV.first $$  # ARGV and $$ would be purple
ARGV.__id__    # this seems like a bug

# To reiterate, normal methods aren't cyan anyway
this_isnt_cyan()
Class.neither_is_this()
binding.pry  # so why is this?
JSON.load    # or this?
select       # lots of methods are being highlighted without a leading dot, this is a bug
reject
'string'.sub
'string'.tr
