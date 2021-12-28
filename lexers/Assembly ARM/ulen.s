# Begin asmlist al_procedures

.text
	.align 4
.globl	_P$ULEN_$$_UTF8LENGTHFAST_SIGNED$PCHAR$INT64$$INT64
_P$ULEN_$$_UTF8LENGTHFAST_SIGNED$PCHAR$INT64$$INT64:
# [ulen.pas]
# [28] begin
	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	sub	sp,sp,#64
# Var p located at sp+0, size=OS_64
# Var ByteCount located at sp+8, size=OS_S64
# Var $result located at sp+16, size=OS_S64
# Var nx located at sp+24, size=OS_S64
# Var i located at sp+32, size=OS_S64
# Var cnt located at sp+40, size=OS_S64
# Var e located at sp+48, size=OS_S64
	str	x0,[sp]
	str	x1,[sp, #8]
# [29] Result := 0;
	str	xzr,[sp, #16]
# [30] e := ix+ByteCount; // End marker
	ldr	x0,[sp]
	ldr	x1,[sp, #8]
	add	x0,x1,x0
	str	x0,[sp, #48]
# [32] cnt := (not (ix-1)) and (sizeof(PtrInt)-1);
	ldr	x0,[sp]
	sub	x0,x0,#1
	mvn	x0,x0
	and	x0,x0,#7
	str	x0,[sp, #40]
# [33] if cnt>ByteCount then
	ldr	x0,[sp, #40]
	ldr	x1,[sp, #8]
	cmp	x0,x1
	b.gt	Lj7
	b	Lj8
Lj7:
# [34] cnt := ByteCount;
	ldr	x0,[sp, #8]
	str	x0,[sp, #40]
Lj8:
# [35] for i := 1 to cnt do
	ldr	x1,[sp, #40]
	cmp	x1,#1
	b.ge	Lj9
	b	Lj10
Lj9:
	str	xzr,[sp, #32]
Lj11:
	ldr	x0,[sp, #32]
	add	x0,x0,#1
	str	x0,[sp, #32]
# [43] Result += (pn8^ shr 7) and ((not pn8^) shr 6);
	ldr	x0,[sp]
	ldrsb	w0,[x0]
	eor	w0,w0,#255
	lsr	w0,w0,#6
	ldr	x2,[sp]
	ldrsb	w2,[x2]
	lsr	w2,w2,#7
	and	w0,w2,w0
	sxtw	x0,w0
	ldr	x2,[sp, #16]
	add	x0,x2,x0
	str	x0,[sp, #16]
# [44] inc(pn8);
	ldr	x0,[sp]
	add	x0,x0,#1
	str	x0,[sp]
	ldr	x0,[sp, #32]
	cmp	x1,x0
	b.le	Lj13
	b	Lj11
Lj13:
Lj10:
# [47] for i := 1 to (ByteCount-cnt) div sizeof(PtrUInt) do
	ldr	x0,[sp, #8]
	ldr	x1,[sp, #40]
	sub	x0,x0,x1
	asr	x1,x0,#63
	add	x0,x0,x1,lsr #61
	asr	x0,x0,#3
	cmp	x0,#1
	b.ge	Lj14
	b	Lj15
Lj14:
	str	xzr,[sp, #32]
Lj16:
	ldr	x1,[sp, #32]
	add	x1,x1,#1
	str	x1,[sp, #32]
# [50] nx := ((pnx^ and EIGHTYMASK) shr 7) and ((not pnx^) shr 6);
	ldr	x1,[sp]
	ldr	x1,[x1]
	and	x1,x1,#0x8080808080808080
	lsr	x1,x1,#7
	ldr	x2,[sp]
	ldr	x2,[x2]
	mvn	x2,x2
	lsr	x2,x2,#6
	and	x1,x2,x1
	str	x1,[sp, #24]
# [52] Result += (nx * ONEMASK) >> ((sizeof(PtrInt) - 1) * 8);
	ldr	x1,[sp, #24]
	mov	x2,#72340172838076673
	mul	x1,x1,x2
	lsr	x1,x1,#56
	ldr	x2,[sp, #16]
	add	x1,x2,x1
	str	x1,[sp, #16]
# [54] inc(pnx);
	ldr	x1,[sp]
	add	x1,x1,#8
	str	x1,[sp]
	ldr	x1,[sp, #32]
	cmp	x0,x1
	b.le	Lj18
	b	Lj16
Lj18:
Lj15:
# [57] while ix<e do
	b	Lj20
Lj19:
# [66] Result += (pn8^ shr 7) and ((not pn8^) shr 6);
	ldr	x0,[sp]
	ldrsb	w0,[x0]
	eor	w0,w0,#255
	lsr	w0,w0,#6
	ldr	x1,[sp]
	ldrsb	w1,[x1]
	lsr	w1,w1,#7
	and	w0,w1,w0
	sxtw	x1,w0
	ldr	x0,[sp, #16]
	add	x0,x0,x1
	str	x0,[sp, #16]
# [67] inc(pn8);
	ldr	x0,[sp]
	add	x0,x0,#1
	str	x0,[sp]
Lj20:
	ldr	x0,[sp]
	ldr	x1,[sp, #48]
	cmp	x0,x1
	b.lt	Lj19
	b	Lj21
Lj21:
# [69] Result := ByteCount - Result;
	ldr	x1,[sp, #8]
	ldr	x0,[sp, #16]
	sub	x0,x1,x0
	str	x0,[sp, #16]
# [70] end;
	ldr	x0,[sp, #16]
	mov	sp,x29
	ldp	x29,x30,[sp], #16
	ret

.text
	.align 4
.globl	_P$ULEN_$$_UTF8LENGTHFAST_UNSIGNED$PCHAR$INT64$$INT64
_P$ULEN_$$_UTF8LENGTHFAST_UNSIGNED$PCHAR$INT64$$INT64:
# [88] begin
	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	sub	sp,sp,#64
# Var p located at sp+0, size=OS_64
# Var ByteCount located at sp+8, size=OS_S64
# Var $result located at sp+16, size=OS_S64
# Var nx located at sp+24, size=OS_64
# Var i located at sp+32, size=OS_S64
# Var cnt located at sp+40, size=OS_S64
# Var e located at sp+48, size=OS_S64
	str	x0,[sp]
	str	x1,[sp, #8]
# [89] Result := 0;
	str	xzr,[sp, #16]
# [90] e := ix+PtrUInt(ByteCount); // End marker
	ldr	x1,[sp]
	ldr	x0,[sp, #8]
	add	x0,x0,x1
	str	x0,[sp, #48]
# [92] cnt := (not (ix-1)) and (sizeof(PtrInt)-1);
	ldr	x0,[sp]
	sub	x0,x0,#1
	mvn	x0,x0
	and	x0,x0,#7
	str	x0,[sp, #40]
# [93] if cnt>ByteCount then
	ldr	x0,[sp, #40]
	ldr	x1,[sp, #8]
	cmp	x0,x1
	b.gt	Lj24
	b	Lj25
Lj24:
# [94] cnt := ByteCount;
	ldr	x0,[sp, #8]
	str	x0,[sp, #40]
Lj25:
# [95] for i := 1 to cnt do
	ldr	x1,[sp, #40]
	cmp	x1,#1
	b.ge	Lj26
	b	Lj27
Lj26:
	str	xzr,[sp, #32]
Lj28:
	ldr	x0,[sp, #32]
	add	x0,x0,#1
	str	x0,[sp, #32]
# [103] Result += (pn8^ shr 7) and ((not pn8^) shr 6);
	ldr	x0,[sp]
	ldrb	w0,[x0]
	eor	w0,w0,#255
	lsr	w0,w0,#6
	ldr	x2,[sp]
	ldrb	w2,[x2]
	lsr	w2,w2,#7
	and	w0,w2,w0
	ldr	x2,[sp, #16]
	add	x0,x2,x0
	str	x0,[sp, #16]
# [104] inc(pn8);
	ldr	x0,[sp]
	add	x0,x0,#1
	str	x0,[sp]
	ldr	x0,[sp, #32]
	cmp	x1,x0
	b.le	Lj30
	b	Lj28
Lj30:
Lj27:
# [107] for i := 1 to (ByteCount-cnt) div sizeof(PtrUInt) do
	ldr	x0,[sp, #8]
	ldr	x1,[sp, #40]
	sub	x0,x0,x1
	asr	x1,x0,#63
	add	x0,x0,x1,lsr #61
	asr	x0,x0,#3
	cmp	x0,#1
	b.ge	Lj31
	b	Lj32
Lj31:
	str	xzr,[sp, #32]
Lj33:
	ldr	x1,[sp, #32]
	add	x1,x1,#1
	str	x1,[sp, #32]
# [110] nx := ((pnx^ and EIGHTYMASK) shr 7) and ((not pnx^) shr 6);
	ldr	x1,[sp]
	ldr	x1,[x1]
	and	x1,x1,#0x8080808080808080
	lsr	x1,x1,#7
	ldr	x2,[sp]
	ldr	x2,[x2]
	mvn	x2,x2
	lsr	x2,x2,#6
	and	x1,x2,x1
	str	x1,[sp, #24]
# [112] Result += (nx * ONEMASK) >> ((sizeof(PtrInt) - 1) * 8);
	ldr	x1,[sp, #24]
	mov	x2,#72340172838076673
	mul	x1,x1,x2
	lsr	x1,x1,#56
	ldr	x2,[sp, #16]
	add	x1,x2,x1
	str	x1,[sp, #16]
# [114] inc(pnx);
	ldr	x1,[sp]
	add	x1,x1,#8
	str	x1,[sp]
	ldr	x1,[sp, #32]
	cmp	x0,x1
	b.le	Lj35
	b	Lj33
Lj35:
Lj32:
# [117] while ix<e do
	b	Lj37
Lj36:
# [126] Result += (pn8^ shr 7) and ((not pn8^) shr 6);
	ldr	x0,[sp]
	ldrb	w0,[x0]
	eor	w0,w0,#255
	lsr	w0,w0,#6
	ldr	x1,[sp]
	ldrb	w1,[x1]
	lsr	w1,w1,#7
	and	w0,w1,w0
	ldr	x1,[sp, #16]
	add	x0,x1,x0
	str	x0,[sp, #16]
# [127] inc(pn8);
	ldr	x0,[sp]
	add	x0,x0,#1
	str	x0,[sp]
Lj37:
	ldr	x0,[sp]
	ldr	x1,[sp, #48]
	cmp	x0,x1
	b.lt	Lj36
	b	Lj38
Lj38:
# [129] Result := ByteCount - Result;
	ldr	x0,[sp, #8]
	ldr	x1,[sp, #16]
	sub	x0,x0,x1
	str	x0,[sp, #16]
# [130] end;
	ldr	x0,[sp, #16]
	mov	sp,x29
	ldp	x29,x30,[sp], #16
	ret

.text
	.align 4
.globl	_main
_main:
	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	sub	sp,sp,#32
# Var ARGC located at sp+0, size=OS_S32
# Var ARGV located at sp+8, size=OS_64
# Var ARGP located at sp+16, size=OS_64
	str	w0,[sp]
	str	x1,[sp, #8]
	str	x2,[sp, #16]
	ldr	x2,[sp, #16]
	ldr	x1,[sp, #8]
	ldr	w0,[sp]
	bl	_FPC_SYSTEMMAIN
	mov	sp,x29
	ldp	x29,x30,[sp], #16
	ret

.text
	.align 4
.globl	_PASCALMAIN
_PASCALMAIN:
# [ulen.pas]
# [136] begin
	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	stp	x19,x19,[sp, #-16]!
	bl	fpc_initializeunits
# [137] DefaultSystemCodePage := CP_UTF8;
	movz	w0,#65001
	adrp	x1,_U_$SYSTEM_$$_DEFAULTSYSTEMCODEPAGE@GOTPAGE
	ldr	x1,[x1, _U_$SYSTEM_$$_DEFAULTSYSTEMCODEPAGE@GOTPAGEOFF]
	sturh	w0,[x1]
# [138] Euro := '...';
	adrp	x1,_$$fpclocal$_ld1@PAGE
	add	x1,x1,_$$fpclocal$_ld1@PAGEOFF
	add	x1,x1,#16
	adrp	x0,_U_$P$ULEN_$$_EURO@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_EURO@GOTPAGEOFF]
	bl	fpc_ansistr_assign
# [140] writeln('Signed version');
	bl	fpc_get_output
	mov	x19,x0
	adrp	x2,_$ULEN$_Ld2@GOTPAGE
	ldr	x2,[x2, _$ULEN$_Ld2@GOTPAGEOFF]
	mov	x1,x19
	movz	w0,#0
	bl	fpc_write_text_shortstr
	bl	fpc_iocheck
	mov	x0,x19
	bl	fpc_writeln_end
	bl	fpc_iocheck
# [141] Len := Utf8LengthFast_Signed(PChar(Euro), Length(Euro));
	adrp	x0,_U_$P$ULEN_$$_EURO@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_EURO@GOTPAGEOFF]
	ldur	x1,[x0]
	cmp	x1,#0
	b.eq	Lj45
	ldur	x1,[x1, #-8]
Lj45:
	adrp	x0,_U_$P$ULEN_$$_EURO@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_EURO@GOTPAGEOFF]
	ldur	x0,[x0]
	cmp	x0,#0
	b.ne	Lj46
	adrp	x0,FPC_EMPTYCHAR@GOTPAGE
	ldr	x0,[x0, FPC_EMPTYCHAR@GOTPAGEOFF]
Lj46:
	bl	_P$ULEN_$$_UTF8LENGTHFAST_SIGNED$PCHAR$INT64$$INT64
	adrp	x1,_U_$P$ULEN_$$_LEN@GOTPAGE
	ldr	x1,[x1, _U_$P$ULEN_$$_LEN@GOTPAGEOFF]
	stur	x0,[x1]
# [142] writeln('Len = ',Len);
	bl	fpc_get_output
	mov	x19,x0
	adrp	x2,_$ULEN$_Ld3@GOTPAGE
	ldr	x2,[x2, _$ULEN$_Ld3@GOTPAGEOFF]
	mov	x1,x19
	movz	w0,#0
	bl	fpc_write_text_shortstr
	bl	fpc_iocheck
	adrp	x0,_U_$P$ULEN_$$_LEN@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_LEN@GOTPAGEOFF]
	ldur	x2,[x0]
	mov	x1,x19
	movz	w0,#0
	bl	fpc_write_text_sint
	bl	fpc_iocheck
	mov	x0,x19
	bl	fpc_writeln_end
	bl	fpc_iocheck
# [144] writeln('Unsigned version');
	bl	fpc_get_output
	mov	x19,x0
	adrp	x2,_$ULEN$_Ld4@GOTPAGE
	ldr	x2,[x2, _$ULEN$_Ld4@GOTPAGEOFF]
	mov	x1,x19
	movz	w0,#0
	bl	fpc_write_text_shortstr
	bl	fpc_iocheck
	mov	x0,x19
	bl	fpc_writeln_end
	bl	fpc_iocheck
# [145] Len := Utf8LengthFast_Unsigned(PChar(Euro), Length(Euro));
	adrp	x0,_U_$P$ULEN_$$_EURO@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_EURO@GOTPAGEOFF]
	ldur	x1,[x0]
	cmp	x1,#0
	b.eq	Lj47
	ldur	x1,[x1, #-8]
Lj47:
	adrp	x0,_U_$P$ULEN_$$_EURO@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_EURO@GOTPAGEOFF]
	ldur	x0,[x0]
	cmp	x0,#0
	b.ne	Lj48
	adrp	x0,FPC_EMPTYCHAR@GOTPAGE
	ldr	x0,[x0, FPC_EMPTYCHAR@GOTPAGEOFF]
Lj48:
	bl	_P$ULEN_$$_UTF8LENGTHFAST_UNSIGNED$PCHAR$INT64$$INT64
	adrp	x1,_U_$P$ULEN_$$_LEN@GOTPAGE
	ldr	x1,[x1, _U_$P$ULEN_$$_LEN@GOTPAGEOFF]
	stur	x0,[x1]
# [146] writeln('Len = ',Len);
	bl	fpc_get_output
	mov	x19,x0
	adrp	x2,_$ULEN$_Ld3@GOTPAGE
	ldr	x2,[x2, _$ULEN$_Ld3@GOTPAGEOFF]
	mov	x1,x19
	movz	w0,#0
	bl	fpc_write_text_shortstr
	bl	fpc_iocheck
	adrp	x0,_U_$P$ULEN_$$_LEN@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_LEN@GOTPAGEOFF]
	ldur	x2,[x0]
	mov	x1,x19
	movz	w0,#0
	bl	fpc_write_text_sint
	bl	fpc_iocheck
	mov	x0,x19
	bl	fpc_writeln_end
	bl	fpc_iocheck
# [147] end.
	bl	fpc_do_exit
	ret

.text
	.align 4
.globl	__P$ULEN_$$_init_implicit$
__P$ULEN_$$_init_implicit$:
	.set _INIT$_$P$ULEN, __P$ULEN_$$_init_implicit$
	.globl _INIT$_$P$ULEN
	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	ldp	x29,x30,[sp], #16
	ret

.text
	.align 4
.globl	__P$ULEN_$$_finalize_implicit$
__P$ULEN_$$_finalize_implicit$:
	.set _FINALIZE$_$P$ULEN, __P$ULEN_$$_finalize_implicit$
	.globl _FINALIZE$_$P$ULEN
	.set PASCALFINALIZE, __P$ULEN_$$_finalize_implicit$
	.globl PASCALFINALIZE
	stp	x29,x30,[sp, #-16]!
	mov	x29,sp
	adrp	x0,_U_$P$ULEN_$$_EURO@GOTPAGE
	ldr	x0,[x0, _U_$P$ULEN_$$_EURO@GOTPAGEOFF]
	bl	fpc_ansistr_decr_ref
	ldp	x29,x30,[sp], #16
	ret
# End asmlist al_procedures
# Begin asmlist al_globals


	.align 3
# [ulen.pas]
# [134] Len: PtrInt;
	.private_extern _U_$P$ULEN_$$_LEN
.globl _U_$P$ULEN_$$_LEN
.data
.zerofill __DATA, __common, _U_$P$ULEN_$$_LEN, 8,3




	.align 3
# [135] Euro: String;
	.private_extern _U_$P$ULEN_$$_EURO
.globl _U_$P$ULEN_$$_EURO
.data
.zerofill __DATA, __common, _U_$P$ULEN_$$_EURO, 8,3



.data
	.align 3
.globl	INITFINAL
INITFINAL:
	.quad	5,0
	.quad	_INIT$_$SYSTEM
	.quad	0,0
	.quad	_FINALIZE$_$OBJPAS
	.quad	_INIT$_$UNIX
	.quad	_FINALIZE$_$UNIX
	.quad	_INIT$_$SYSUTILS
	.quad	_FINALIZE$_$SYSUTILS
	.quad	_INIT$_$P$ULEN
	.quad	_FINALIZE$_$P$ULEN

.data
	.align 3
.globl	FPC_THREADVARTABLES
FPC_THREADVARTABLES:
	.long	1
	.byte	0,0,0,0
	.quad	_THREADVARLIST_$SYSTEM$indirect

.const_data
	.align 3
.globl	FPC_RESOURCESTRINGTABLES
FPC_RESOURCESTRINGTABLES:
	.quad	1
	.quad	_RESSTR_$SYSCONST_$$_START$indirect
	.quad	_RESSTR_$SYSCONST_$$_END$indirect

.data
	.align 3
.globl	FPC_WIDEINITTABLES
FPC_WIDEINITTABLES:
	.quad	0

.data
	.align 3
.globl	FPC_RESSTRINITTABLES
FPC_RESSTRINITTABLES:
	.quad	0

.section __TEXT, .fpc, regular, no_dead_strip
	.align 4
.reference __fpc_ident
__fpc_ident:
	.ascii	"FPC 3.3.1 [2021/11/12] for aarch64 - Darwin"

.data
	.align 3
.globl	__stklen
__stklen:
	.quad	8388608

.data
	.align 3
.globl	__heapsize
__heapsize:
	.quad	0

.data
	.align 3
.globl	__fpc_valgrind
__fpc_valgrind:
	.byte	0

.const_data
	.align 3
.globl	FPC_RESLOCATION
FPC_RESLOCATION:
	.quad	0
# End asmlist al_globals
# Begin asmlist al_typedconsts

.const
	.align 3
_$$fpclocal$_ld1:
	.short	65001,1
	.long	-1
	.quad	3
	.ascii	"\342\202\254\000"

.const
	.align 3
.globl	_$ULEN$_Ld2
_$ULEN$_Ld2:
	.ascii	"\016Signed version\000"

.const
	.align 3
.globl	_$ULEN$_Ld3
_$ULEN$_Ld3:
	.ascii	"\006Len = \000"

.const
	.align 3
.globl	_$ULEN$_Ld4
_$ULEN$_Ld4:
	.ascii	"\020Unsigned version\000"
# End asmlist al_typedconsts
# Begin asmlist al_rtti

.const_data
	.align 3
.globl	_RTTI_$P$ULEN_$$_def00000008
_RTTI_$P$ULEN_$$_def00000008:
	.byte	9,0,0,0,0,0,0,0
	.quad	0
	.short	65001
	.byte	0,0,0,0,0,0
# End asmlist al_rtti
# Begin asmlist al_indirectglobals

.const_data
	.align 3
.globl	_RTTI_$P$ULEN_$$_def00000008$indirect
_RTTI_$P$ULEN_$$_def00000008$indirect:
	.quad	_RTTI_$P$ULEN_$$_def00000008
# End asmlist al_indirectglobals
	.subsections_via_symbols

