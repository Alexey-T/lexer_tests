
$bar = 'it is \'worth\' $foo';
print $bar;

---
$bar = q(it is 'worth' $foo);
print $bar;

--
$bar = qq(it is "worth" $foo);
print $bar;

--
@baz = ('one', 'two', 'three');
@baz = qw(one two three);

my $bar = "Martha Stewedprune";
print <<"EOT";
=====

This is an example of
   text taken literally
   except that variables are
   expanded where their
   variable names appear.
   foo: $foo
   bar: $bar
EOT

--
