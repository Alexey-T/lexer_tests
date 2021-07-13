sub test {
   
my $message = <<END_MESSAGE;
...
END_MESSAGE
    
my $message = <<'END_MESSAGE';
Dear $name,
regards
END_MESSAGE

    print <<"EOT-EOT-EOT";
Syntax:
    Txt\'t txt
Text.
EOT-EOT-EOT

    exit;
}
