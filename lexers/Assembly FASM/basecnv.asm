
; Base Converter Version 1.0
; Copyright © Tommy Lillehagen, 2003.
; All rights reserved.

format PE GUI 4.0
entry start

include 'win32a.inc'

IDD_MAIN        = 100
IDC_NUM         = 101

IDM_MAIN        = 200
IDM_BIN         = 201
IDM_OCT         = 202
IDM_DEC         = 203
IDM_HEX         = 204
IDM_CHAR        = 205
IDM_COPY        = 206
IDM_CLEAR       = 207

section '.data' data readable writeable

        current_base    dd ?
        original_proc   dd ?
        hinstance       dd ?
        hdialog         dd ?
        hnum            dd ?
        hmenu           dd ?
        buffer          rb 100h

section '.code' code readable executable

    start:
        invoke  GetModuleHandle,0
        mov     [hinstance],eax
        invoke  DialogBoxParam,eax,IDD_MAIN,HWND_DESKTOP,MainDlg,0
        invoke  ExitProcess,0

    proc MainDlg hdlg,msg,wparam,lparam
        push    ebx edx esi edi
        cmp     [msg],WM_INITDIALOG
        je      .wminitdlg
        cmp     [msg],WM_INITMENU
        je      .wminitmenu
        cmp     [msg],WM_COMMAND
        je      .wmcommand
        cmp     [msg],WM_CLOSE
        je      .wmclose
        xor     eax,eax
        jmp     .finish
      .wminitdlg:
        mov     eax,[hdlg]
        mov     [hdialog],eax
        mov     [current_base],10
        invoke  GetDlgItem,[hdlg],IDC_NUM
        mov     [hnum],eax
        invoke  GetWindowLong,[hnum],GWL_WNDPROC
        mov     [original_proc],eax
        invoke  SetWindowLong,[hnum],GWL_WNDPROC,EditCtl
        invoke  LoadMenu,[hinstance],IDM_MAIN
        invoke  GetSubMenu,eax,0
        mov     [hmenu],eax
        jmp     .processed
      .wminitmenu:
        mov     ebx,[current_base]
        cmp     ebx,2
        setz    al
        neg     al
        and     eax,MF_CHECKED
        or      eax,MF_BYCOMMAND
        invoke  CheckMenuItem,[hmenu],IDM_BIN,eax
        cmp     ebx,8
        setz    al
        neg     al
        and     eax,MF_CHECKED
        or      eax,MF_BYCOMMAND
        invoke  CheckMenuItem,[hmenu],IDM_OCT,eax
        cmp     ebx,10
        setz    al
        neg     al
        and     eax,MF_CHECKED
        or      eax,MF_BYCOMMAND
        invoke  CheckMenuItem,[hmenu],IDM_DEC,eax
        cmp     ebx,16
        setz    al
        neg     al
        and     eax,MF_CHECKED
        or      eax,MF_BYCOMMAND
        invoke  CheckMenuItem,[hmenu],IDM_HEX,eax
        test    ebx,ebx
        setz    al
        neg     al
        and     eax,MF_CHECKED
        or      eax,MF_BYCOMMAND
        invoke  CheckMenuItem,[hmenu],IDM_CHAR,eax
        jmp     .processed
      .wmcommand:
        mov     eax,[wparam]
        mov     ebx,2
        cmp     eax,BN_CLICKED shl 16 + IDM_BIN
        je      .base_changed
        mov     ebx,8
        cmp     eax,BN_CLICKED shl 16 + IDM_OCT
        je      .base_changed
        mov     ebx,10
        cmp     eax,BN_CLICKED shl 16 + IDM_DEC
        je      .base_changed
        mov     ebx,16
        cmp     eax,BN_CLICKED shl 16 + IDM_HEX
        je      .base_changed
        xor     ebx,ebx
        cmp     eax,BN_CLICKED shl 16 + IDM_CHAR
        je      .base_changed
        cmp     eax,BN_CLICKED shl 16 + IDM_CLEAR
        jne     .maybe_copy
        invoke  SendMessage,[hnum],EM_SETSEL,0,-1
        invoke  SendMessage,[hnum],WM_CLEAR,0,0
      .maybe_copy:
        cmp     eax,BN_CLICKED shl 16 + IDM_COPY
        jne     .processed
        invoke  SendMessage,[hnum],EM_SETSEL,0,-1
        invoke  SendMessage,[hnum],WM_COPY,0,0
        jmp     .processed
      .base_changed:
        invoke  GetDlgItemText,[hdlg],IDC_NUM,buffer,100h
        mov     edi,buffer
        cmp     [current_base],0
        je      .not_from_num
        push    ebx
        mov     ebx,[current_base]
        call    StrToInt
        pop     ebx
        jmp     .show_result
      .not_from_num:
        mov     eax,dword [buffer]
      .show_result:
        test    ebx,ebx
        je      .not_to_num
        mov     edi,buffer
        call    IntToStr
        jmp     .done
      .not_to_num:
        mov     dword [buffer],eax
        mov     byte [buffer+4],0
      .done:
        invoke  SetDlgItemText,[hdlg],IDC_NUM,buffer
        mov     [current_base],ebx
        invoke  SendMessage,[hnum],EM_SETSEL,0,-1
        invoke  SetFocus,[hnum]
        jmp     .processed
      .wmclose:
        invoke  EndDialog,[hdlg],0
      .processed:
        xor     eax,eax
        inc     eax
      .finish:
        pop     edi esi edx ebx
        ret
    endp

    proc EditCtl hwnd,msg,wparam,lparam
        push    ebx esi edi
        cmp     [msg],WM_PASTE
        je      .finish
        cmp     [msg],WM_CONTEXTMENU
        je      .popupmenu
        cmp     [msg],WM_CHAR
        jne     .process
        cmp     [current_base],0
        je      .process
        mov     eax,[wparam]
        cmp     eax,'a'
        jl      .ok
        sub     eax,20h
        mov     [wparam],eax
      .ok:
        cmp     [current_base],10
        jle     .done
        cmp     eax,'A'
        jl      .done
        mov     ebx,[current_base]
        sub     ebx,10
        add     ebx,'A'
        cmp     eax,ebx
        jl      .process
      .done:
        cmp     eax,8
        je      .process
        mov     ecx,'0'-1
        lea     edx,[eax-'0']
        cmp     edx,[current_base]
        jae     .finish
      .process:
        invoke  CallWindowProc,[original_proc],[hwnd],[msg],[wparam],[lparam]
        jmp     .finish
      .popupmenu:
        mov     eax,[lparam]
        mov     ebx,eax
        and     eax,0FFFFh
        shr     ebx,16
        invoke  TrackPopupMenu,[hmenu],0,eax,ebx,0,[hdialog],0
      .finish:
        pop     edi esi ebx
        ret
    endp

    StrToInt:
        ; edi = buffer, ebx = base
        push    edx edi
        xor     eax,eax
        xor     edx,edx
     .loop:
        mov     dl,byte [edi]
        test    dl,dl
        jz      .end
        imul    eax,ebx
        sub     dl,'0'
        cmp     dl,9
        jle     .ok
        sub     dl,7
      .ok:
        add     eax,edx
        inc     edi
        jmp     .loop
      .end:
        pop     edi edx
        ret

    IntToStr:
        ; eax = number, ebx = base, edi = buffer
        push    ecx edx
        xor     ecx,ecx
      .new:
        xor     edx,edx
        div     ebx
        push    edx
        inc     ecx
        test    eax,eax
        jnz     .new
      .loop:
        pop     eax
        add     al,30h
        cmp     al,'9'
        jng     .ok
        add     al,7
      .ok:
        stosb
        loop    .loop
        mov     al,0
        stosb
        pop     edx ecx
        ret

section '.idata' import data readable writeable

        library kernel,'KERNEL32.DLL',\
                user,'USER32.DLL'

        import  kernel,\
                GetModuleHandle,'GetModuleHandleA',\
                ExitProcess,'ExitProcess'

        import  user,\
                DialogBoxParam,'DialogBoxParamA',\
                EndDialog,'EndDialog',\
                SetDlgItemText,'SetDlgItemTextA',\
                GetDlgItemText,'GetDlgItemTextA',\
                SetDlgItemInt,'SetDlgItemInt',\
                GetDlgItemInt,'GetDlgItemInt',\
                GetDlgItem,'GetDlgItem',\
                SetFocus,'SetFocus',\
                GetWindowLong,'GetWindowLongA',\
                SetWindowLong,'SetWindowLongA',\
                CallWindowProc,'CallWindowProcA',\
                SendMessage,'SendMessageA',\
                TrackPopupMenu,'TrackPopupMenu',\
                LoadMenu,'LoadMenuA',\
                GetSubMenu,'GetSubMenu',\
                CheckMenuItem,'CheckMenuItem'

section '.rsrc' resource data readable

        directory RT_DIALOG,dialogs,\
                  RT_MENU,menus

        resource dialogs,\
                 IDD_MAIN,LANG_ENGLISH+SUBLANG_DEFAULT,main_dialog

        resource menus,\
                 IDM_MAIN,LANG_ENGLISH+SUBLANG_DEFAULT,main_menu

        dialog main_dialog,'Base Converter',0,0,190,34,WS_CAPTION+WS_POPUP+WS_SYSMENU+DS_MODALFRAME+DS_CENTER,0,0,'Verdana',7
          dialogitem 'EDIT','0',IDC_NUM,2,2,186,30,WS_VISIBLE+WS_BORDER+WS_TABSTOP+ES_AUTOHSCROLL
        enddialog

        menu main_menu
            menuitem 'Base',0,MFR_POPUP+MFR_END
                menuitem 'Copy',IDM_COPY,0
                menuitem 'Clear',IDM_CLEAR,0
                menuseparator
                menuitem 'Binary',IDM_BIN,0
                menuitem 'Octal',IDM_OCT,0
                menuitem 'Decimal',IDM_DEC,0
                menuitem 'Hexadecimal',IDM_HEX,0
                menuseparator
                menuitem 'Characters',IDM_CHAR,MFR_END
