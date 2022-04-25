
<?php                  
$values = [<<<  "END"
a
  b
    c
      END, 'd e f'];
var_dump($values);


<?php
var_dump(array(<<<EOD
foobar!
  EOD
));

function foo()
{
    static $bar = <<<LABEL
Nothing in here...
LABEL;
}

// Class properties/constants
class foo
{
    const BAR = <<<FOOBAR
Constant example
FOOBAR;

    public $baz = <<<FOOBAR
Property example
FOOBAR;
}
?>
