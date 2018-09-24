$INVALID_STRING="!"$\%&'()*/@`~";
$STRING_TO_EXECUTE="java myprogram -opt1 ABC -opt2 $INVALID_STRING";
@output=`$STRING_TO_EXECUTE 2>&1`;