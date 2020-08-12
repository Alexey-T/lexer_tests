<?
echo <<<EOT
This is a test! $var
DD;
EE;
EOT;
echo <<<EOT
    This is a test! $var
    EOT;
echo <<<'EOT'
This is a test! $var
EOT;
echo <<<HTML
This is a test!
<div class="foo-bar"></div>
HTML;
echo <<< JAVASCRIPT
var foo = 1;
$var
    ($var)
JAVASCRIPT;
echo <<<CSS
h2 {font-family: 'Arial';}
h3 {font-size: "$h3_size";}
CSS;
echo <<<SQL
SELECT * FROM users WHERE first_name = 'John' LIMIT $limit
SQL;
echo <<<  "SQL"
SELECT * FROM users WHERE first_name = 'John'\n
SQL;
