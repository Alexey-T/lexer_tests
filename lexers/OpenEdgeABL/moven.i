IF  {2}[{1}]:VISIBLE = YES THEN
    IF  {2}[{1}]:X > 12 - velocidade THEN
        ASSIGN 
            {2}[{1}]:X = {2}[{1}]:X - 12 - velocidade NO-ERROR.
    ELSE DO:
        ASSIGN
             {2}[{1}]:X = RANDOM(500,700)
             {2}[{1}]:Y = RANDOM(208,322)
             {2}[{1}]:VISIBLE = YES
             icarros:SCREEN-VALUE = STRING( velocidade + INT(icarros:SCREEN-VALUE) ).
    END.
