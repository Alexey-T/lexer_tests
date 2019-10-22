    lw    $t0, 1060($gp)      # fetch background
    andi  $t1, $t0, 0xff00    # isolate blue
    sll   $t1, $t1, 2         # times 2
    andi  $t1, $t1, 0xff00    # get rid of overflow
    lui   $t2, 0xffff         # $t2 = 0xffff0000
    ori   $t2, $t2, 0x00ff    # $t2 = 0xffff00ff
    and   $t0, $t0, $t2       # get rid of old value of blue
    or    $t0, $t0, $t1       # new value
    sw    $t0, 1060($gp)      # background = ...