; basic macro definitions, I wouldn't put them in tree
ERR_OK     EQU 0
ERR_SYNTAX EQU 1

; complex macros, may be put in tree

; nasm macro that takes no parameter
%macro ALME 0
           mov      ax, 0501h          ;DPMI allocation function
           mov      cx, dx             ;BX:CX = amount
           ror      edx, 16
           mov      bx, dx
           ror      edx, 16            ;restore amount
           int      31h
%endmacro

; nasm macro taking 1 par
%MACRO DOSB 1
           mov ah, %1
           int 21h
%ENDMACRO

; masm macro with no parameters
PUSHREGS MACRO
              push ax
              push cx
              push dx
              push bx
              push si
              push di
              push bp
              push ds
              push es
ENDM

; masm macro with 1 parameter
DOSB macro bval
          mov ah, bval
          int 21h
ENDM

; masm macro with 1 parameter
Print MACRO str
           mov dx, OFFSET str
           DOSB 09h
ENDM


; procedures using macros
my_exit PROC
        ALME
        DOSB 4ch
my_exit ENDP

print_str PROC
        Print non_existent string
        ret
print_str ENDP

; Both masm/nasm utilize the case INsensitive "MACRO" directive.
; The usage is exactly the same in both nasm/masm.
; While populating tree, for nasm take the 2nd token (after "%macro")
; While populating tree, for masm take the 1st token (before "macro")
