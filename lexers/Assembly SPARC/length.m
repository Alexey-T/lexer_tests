
define(`ptr_r',  l0)
define(`byte_r', l1)
define(`len_r',  l2)

	.global	length
length:
	save	%sp, -96, %sp
	mov	%i0, %ptr_r		! copy starting address
	clr	%len_r			! length starts at 0
loop:
	ldub	[%ptr_r], %byte_r	! load next byte (unsigned)
	cmp	%byte_r, 0		! exit when zero byte reached
	beq	done
	nop
	inc	%ptr_r			! increment string pointer
	inc	%len_r			! increment length
	ba	loop
	nop
done:
	mov	%len_r,%i0		! copy length to return value
	ret
	restore


/* test driver for string length
 */

define(TEST_STRING,`! print the string before the call to length
	set	$1,%o0
	call	printf
	nop
	! printf should not change %o0, but reset just to be sure
	set	$1,%o0
	call	length
	nop
	! print the result
	mov	%o0,%o1
	set	cnt_fmt,%o0
	call	printf
	nop')dnl

	.global	main
main:
	save	%sp, -96, %sp
	TEST_STRING(str0)
	TEST_STRING(str1)
	TEST_STRING(str2)
	TEST_STRING(str3)
	TEST_STRING(str4)
	ret
	restore

	.section ".data"
str0:	.byte	0
str1:   .asciz "\n"
str2:   .asciz "This is a test.\n"
str3:   .asciz "Hello, world!\n"
str4:   .asciz "abcdefghijklmnopqrstuvwxyz 0123456789 .,;: +-* /_=()&%%$#[]\n"
cnt_fmt:.asciz "string length is %d bytes (not including terminating 0)\n"
