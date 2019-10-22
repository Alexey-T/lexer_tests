;
; Basic Windows(R) 32bit hooking example in FASM, (C) 2006 Bryan "RedGhost" Power
;
; Tested on:
;     Windows XP   SP2
;     Windows 2000 SP4
;
;     If it is not working on other versions of Windows you must disassemble for the correct opcode length (at least $5 bytes).
;
; Explaination:
;     Sometimes in a project you need to hook an API function for whatever reason, the easiest way is via a basic detour.
;     
;     A detour is inserting a jump to your hook at the start of the API, and if you choose to, emulating the instructions
;     you are overwriting and then calling the original API.
;
;     Here is a small example of hooking the "MessageBoxA" procedure from "user32.dl".
;
;
; here is the disassembly of "MessageBoxA" from "user32.dll" taken from OllyDbg
;
; Windows XP SP2
;
; 77D804EA > 8BFF             MOV EDI,EDI                    ; standard windows API padding
; 77D804EC   55               PUSH EBP                       ; start of stack frame
; 77D804ED   8BEC             MOV EBP,ESP                    ; right here we have exactly $5 bytes in opcodes, perfect, the length is $5
; 77D804EF   833D BC04DA77 00 CMP DWORD PTR DS:[77DA04BC],0
;
; Windows 2000 SP4
;
; 77E38098 > 55               PUSH EBP
; 77E38099   8BEC             MOV EBP,ESP
; 77E3809B   51               PUSH ECX
; 77E3809C   833D 3892E677 00 CMP DWORD PTR DS:[77E69238],0  ; here it's a bit more complex, we don't have exactly $5 bytes, the length is $B
;
; here we have the disassembly while our hook is inserted
;
; Windows XP SP2
;
; 77D804EA >-E9 AA0B6888      JMP detour_e.00401099          ; the jump to our hook
; 77D804EF   833D BC04DA77 00 CMP DWORD PTR DS:[77DA04BC],0
;
; Windows 2000 SP4
;
; 77E38098 >-E9 FC8F5C88      JMP detour_e.00401099
; 77E3809D   90               NOP
; 77E3809E   90               NOP
; 77E3809F   90               NOP
; 77E380A0   90               NOP
; 77E380A1   90               NOP
; 77E380A2   90               NOP                            ; all these nops to keep the instructions balanced
; 77E380A3   0F85 940E0100    JNZ user32.77E48F3D
;

format PE GUI

section '.flat' readable writeable executable

entry $ 			  ; entry point at current address
    mov   ecx, $5		  ; number of bytes for newer service packs, a length disassembly engine skips this work
    mov   eax, [MessageBoxA]
    cmp   word [eax], $FF8B	  ; test if the first opcode is not "mov edi, edi" to identify an older service pack
    jz	  @f
    add   ecx, $6		  ; we have an older MessageBoxA version, $B bytes in total (Windows 2000 SP4)

@@:
    mov   esi, [MessageBoxA]	  ; the procedure we wish to detour
    mov   edi, MessageBoxA_gate   ; the hook we want called when MessageBoxA is called
    call  insert_detour
    mov   [oMessageBoxA], eax	  ; address of the jump/call stub

    ; now we call MessageBoxA
    ; but magically we don't see the text we are passing =]
    push  $0
    push  _hello		  ; 'Hello World!'
    push  _hook_test		  ; 'This is a hooking example with detours in FASM!'
    push  $0
    call  [MessageBoxA]

    ; here we make the same call again but with oMessageBoxA
    push  $0
    push  _hello
    push  _hook_test
    push  $0
    call  [oMessageBoxA]

    push  $0
    call  [ExitProcess]
;---

oMessageBoxA dd ?

_hello	     db 'Hello World!', $0
_hook_test   db 'This is a hooking example with detours in FASM!', $0

; our hook which gets called when MessageBoxA is called in this process
MessageBoxA_gate:
    ; modify the parameters on the stack to our own
    mov   dword [esp+$C], .hello_hacked
    mov   dword [esp+$8], .hook_test_hacked

    jmp   [oMessageBoxA]	  ; now we let MessageBoxA continue

.hello_hacked:
    db 'Hacked Hello World ;)', $0
.hook_test_hacked:
    db 'Hmm? Is this what we are supposed to see?!?', $0
;---

; Allocate some memory.
;
; ecx = length
;
; returns allocated memory address in eax
; returns $0 on failure
halloc:
    cmp   dword [.heapaddr], $0
    jz	  .hinit

@@:
    push  ecx
    push  $0
    push  dword [.heapaddr]
    call  [HeapAlloc]

    ret

.hinit:
    ; get the process heap
    call  [GetProcessHeap]	  ; .ProcessHeap is located at +$18 in the Process Environment Block
    test  eax, eax
    jz	  .err

    mov   dword [.heapaddr], eax
    jmp   @b

.err:
    ret

.heapaddr:
    dd $0
;---

; Write a detour.
;
; esi = address of procedure (API) to detour
; edi = address of gateway (hook)
; ecx = length of opcode(s), must be atleast $5 (jmp + address = $5)
;
; returns jump/call stub in eax
; returns $0 on failure
insert_detour:
    push  ebx edi ebp
    pushfd

    cld

    mov   ebx, ecx		  ; save the length for later, a length disassembly engine is a good idea here

    ; allocate jump stub
    mov   ecx, ebx
    add   ecx, $5		  ; +$5 for jmp + address back to original API
    call  halloc
    test  eax, eax
    jz	  .done

    ; copy the instructions we are going to overwrite to our jump stub
    push  edi
    
    mov   ecx, ebx
    mov   edi, eax
    rep   movsb
    
    sub   esi, ebx		  ; movsb incremented it
    pop   edi

    ; write a jump back to the original API, skip our inserted jump
    mov   byte [eax+ebx], $E9	  ; jmp opcode
    mov   edx, esi
    sub   edx, eax		  ; relative offset
    sub   edx, $5
    mov   dword [eax+ebx+1], edx

    push  eax			  ; save stub address

    ; get page base
    mov   ebp, esi
    and   ebp, $FFFFF000

    ; change protection state of the page to allow write access
    push  .protection
    push  PAGE_EXECUTE_READWRITE  ; EXECUTE in case of multi-threaded or VirtualProtectEx detour
    push  $00001000
    push  ebp
    push  $FFFFFFFF
    call  [VirtualProtectEx]	  ; VirtualProtect calls VirtualProtectEx

    ; here we write the actual detour
    mov   byte [esi], $E9
    mov   edx, edi
    sub   edx, esi
    sub   edx, $5
    mov   dword [esi+1], edx

    ; pad with nops if instruction length > 5 so we don't corrupt the proceeding instructions
    cmp   ebx, $5
    jle   .cleanup

    ; ecx = number of nop instructions
    mov   ecx, ebx
    sub   ecx, $5

    mov   edi, esi
    add   edi, $5
    mov   al,  $90		  ; nop opcode
    rep   stosb

.cleanup:
    ; restore original protection
    push  .protection
    push  dword [.protection]
    push  $00001000
    push  ebp
    push  $FFFFFFFF
    call  [VirtualProtectEx]

    pop   eax			  ; return stub address

.done:
    popfd
    pop   ebp edi ebx
    ret

.protection:
    dd ?
;---

include 'win32a.inc'

data import
    dd $0, $0, $0, rva kernel32_name, rva kernel32_table
    dd $0, $0, $0, rva user32_name,   rva user32_table
    dd $0, $0, $0, $0, $0

kernel32_table:
    VirtualProtectEx	       dd rva _VirtualProtectEx
    GetProcessHeap	       dd rva _GetProcessHeap
    HeapAlloc		       dd rva _HeapAlloc
    ExitProcess 	       dd rva _ExitProcess
    dd $0

user32_table:
    MessageBoxA 	       dd rva _MessageBoxA
    dd $0

;kernel32
    _VirtualProtectEx	       dw $0
			       db 'VirtualProtectEx', $0
    _GetProcessHeap	       dw $0
			       db 'GetProcessHeap', $0
    _HeapAlloc		       dw $0
			       db 'HeapAlloc', $0
    _ExitProcess	       dw $0
			       db 'ExitProcess', $0

;user32
    _MessageBoxA	       dw $0
			       db 'MessageBoxA', $0

    kernel32_name db 'kernel32.dll', $0
    user32_name   db 'user32.dll', $0
end data
;---

;bye




