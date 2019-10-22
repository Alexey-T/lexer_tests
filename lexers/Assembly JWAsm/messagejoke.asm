        option casemap: none

        includelib kernel32.lib
        includelib user32.lib

        externdef MessageBoxA: near
        externdef ExitProcess: near

lf      equ 10                           ; ASCII line feed
cr      equ 13                           ; ASCII carriage return
IDOK    equ 1                            
IDCANC  equ 2


        .data

txt1    db "'I'm a huge fan of conservation', Peter said.", cr, lf
        db "'For example, I don't throw away used subway tickets,'", cr, lf
        db "'but I use them several times.'", 0
capt1   db "Ha, ha, ha, ha...", 0
txt2    db "Yes, I also think, that's a good joke ...", 0
capt2   db "You've pressed Ok.", 0
txt3    db "Oh, why did you press Cancel?", cr, lf
        db "Don't you like the joke?", 0
capt3   db "You've pressed Cancel.", 0

        .code

main    proc

        ; make room for arguments + 16 bit aligned stack

        sub        rsp, 40

        ; uType = MB_ICONINFORMATION | MB_OKCANCEL | MB_DEFBUTTON1

        mov        r9d, 65               
        lea        r8, [capt1]           ; r8  = lpCaption
        lea        rdx, [txt1]           ; rdx = lpText
        xor        rcx, rcx              ; rcx = hWnd
        call       MessageBoxA

        cmp        eax, IDOK             ; OK pressed?
        jne        m1                    ; no: jump
        
        ; uType = MB_ICONINFORMATION | MB_OK | MB_DEFBUTTON1

        mov        r9d, 64               
        lea        r8, [capt2]
        lea        rdx, [txt2]
        xor        rcx, rcx
        call       MessageBoxA
        jmp        short m2

m1:

        cmp        eax, IDCANC           ; Cancel pressed?
        jne        m2                    ; no: jump
        

        ; uType = MB_ICONINFORMATION | MB_OK | MB_DEFBUTTON1

        mov        r9d, 64               
        lea        r8, [capt3]
        lea        rdx, [txt3]
        xor        rcx, rcx
        call       MessageBoxA

m2:

        xor        ecx, ecx              ; ecx = exit code
        call       ExitProcess
main    endp
        end
