     .global prt_sum
prt_sum:
     save %sp,-96,%sp

     clr  %l0
     clr  %l1
     mov  %i0,%l2
loop:
     cmp  %l0,%i1
     bge  done
     nop

     ld   [%l2],%o0
     add  %l1,%o0,%l1
     add  %l2,4,%l2
     inc  %l0
     ba   loop
     nop
done:

/* parameter regs for printf
/*   %o0 - addr of format string
/*   %o1 - first value
*/

     set  fmt,%o0
     mov  %l1,%o1
     call printf
     nop

     ret
     restore


     .section ".data"
fmt: .asciz "%d\n"
