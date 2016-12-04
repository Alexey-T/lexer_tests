
define(`ptr_r',  l0)
define(`byte_r', l1)

	.global	capital
capital:
	save	%sp, -96, %sp
	mov	%i0, %ptr_r		! copy starting address
loop:
	ldub	[%ptr_r], %byte_r	! load next byte (unsigned)
	cmp	%byte_r, 0		! exit when zero byte reached
	beq	done
	nop
	cmp	%byte_r, 'a'		! cmp to lower limit 0x61 = 97 = 'a'
	blt	next			!   lower case letter will be >= 'a'
	nop
	cmp	%byte_r, 'z'		! cmp to upper limit 0x7a = 122 = 'z'
	bgt	next			!   lower case letter will be <= 'z'
	nop
	sub	%byte_r, 0x20, %byte_r	! change lower case letter to upper case
	stb	%byte_r, [%ptr_r]	! store back to string (update in place)
next:
	inc	%ptr_r			! increment string pointer
	ba	loop
	nop
done:
	ret
	restore


/* test driver for capital subroutine
 */

define(MACRO_EXP_CNT,1)dnl
define(CAPITAL,`
! expansion MACRO_EXP_CNT of `CAPITAL' macro, parameter is $1
	set	$1,%l0
	mov	%l0,%o0
	call	printf
	nop
	mov	%l0,%o0
	call	capital
	nop
	cmp	%l0,%o0
	beq	`next'MACRO_EXP_CNT
	nop
	mov	%o0,%o1
	set	errfmt,%o0
	call	printf
	nop
`next'MACRO_EXP_CNT:
	mov	%l0,%o0
	call	printf
	nop
define(`MACRO_EXP_CNT',eval(MACRO_EXP_CNT+1))dnl
')dnl

	.global	main
main:
	save	%sp, -96, %sp
	CAPITAL(str1)
	CAPITAL(str2)
	CAPITAL(str3)
	CAPITAL(str4)
	CAPITAL(str5)
	ret
	restore

	.section ".data"
str1:   .byte  0
str2:   .asciz "Hello, world!\n"
str3:   .asciz "This is a test of the capitalization subroutine.\n"
str4:   .asciz "What happens to punctuation? and numbers like 42?\n"
str5:   .asciz "abcdefghijklmnopqrstuvwxyz 0123456789 .,;: +-* /_=()&%%$#[]\n"

errfmt:	.asciz "--- capital returns with different value (=%x) in %o0 ---\n"
