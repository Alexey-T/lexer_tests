<?
$example = foo(
  <<<HEREDOC
  This is just a cool test.
  HEREDOC,
  $bar
);
$rest_of_the_code = 'hello';

    echo <<<NOWDOC
The quick brown fox
    NOWDOC
    ;

echo <<<END
      a
     b
    c
END;

stringManipulator(<<<END
   a
  b
 c
END);
 
$values = [<<<END
a
b
c
END, 'd e f'];
