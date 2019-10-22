IF  {2}[{1}]:VISIBLE = YES THEN
    IF  {2}[{1}]:X > 12 THEN
        ASSIGN 
            {2}[{1}]:X = {2}[{1}]:X - 12 - velocidade NO-ERROR.
    ELSE DO:
        ASSIGN
            {2}[{1}]:VISIBLE = NO.
        ASSIGN
             {2}[{1}]:X = RANDOM(50,600)
             {2}[{1}]:Y = 150             
             {2}[{1}]:VISIBLE = YES NO-ERROR.
    END.
