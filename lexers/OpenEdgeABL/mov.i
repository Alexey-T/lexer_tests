IF  {2}[{1}]:VISIBLE = YES THEN
    IF  {2}[{1}]:X > 12 THEN
        ASSIGN 
            {2}[{1}]:X = {2}[{1}]:X - 12.
    ELSE
        ASSIGN
            {2}[{1}]:VISIBLE = NO.
