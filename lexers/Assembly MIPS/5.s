     lw    $t0, 0($gp)          # fetch i
     bltz  $t0, def             # i<0 -> default
     slti  $t1, $t0, 3          # i<3?
     beq   $t1, $zero, def      # no, -> default
     sll   $t0, $t0, 2          # turn i into a byte offset
     add   $t2, $t0, $gp
     lw    $t2, 1064($t2)       # fetch the branch table entry
     jr    $t2                  # go...
is0: sw    $zero, 28($gp)       # A[0] = 0
     j     done
is1: 
is2: addi  $t0, $zero, 1        # = 1
     sw    $t0, 32($gp)         # A[1] = 1
     j     done
def: addi  $t0, $zero, -1       # = -1
     sw    $t0, 28($gp)         # A[0] = -1
     j     done
done: