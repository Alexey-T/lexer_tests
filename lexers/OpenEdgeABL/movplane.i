IF  {2}[{1}]:VISIBLE = YES THEN
    IF  {2}[{1}]:X < 600 THEN
        ASSIGN 
            {2}[{1}]:X = {2}[{1}]:X + 15 NO-ERROR.
    ELSE DO:
        ASSIGN
            {2}[{1}]:VISIBLE = NO.
        ASSIGN
             {2}[{1}]:X = RANDOM(30,190)
             {2}[{1}]:Y = RANDOM(10,100)
             {2}[{1}]:VISIBLE = YES NO-ERROR.
    END.
