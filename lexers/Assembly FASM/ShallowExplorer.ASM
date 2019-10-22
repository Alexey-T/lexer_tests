

; traditional mandelbrot set:
;   defined as the prisoner set of the iteration
;     {x,y} = {x*x-y*y+x0,2*x*y+y0}
;   starting with {x,y} = {0,0}
;   at the point {x0,y0}


format PE64 GUI 4.0
entry Start

include 'win64a.inc'

DIB_RGB_COLORS	   = 0
START_WIDTH	   = 1280
START_HEIGHT	   = 720
MOUSE_LEFT	   = 0x00000001
MOUSE_RIGHT	   = 0x00000002

PATH_COUNT = 5
_QUADRATIC_SSE128  = 0x00001
_QUADRATIC_AVX256  = 0x00002
_QUADRATIC_4FMA256 = 0x00004
_QUADRATIC_3FMA256 = 0x00008
_QUADRATIC_FPU	   = 0x00010





section '.text' code readable executable

  Start:	       push  rbp

			xor  eax,eax
			mov  dword[SupportedPaths],eax
.CheckSSE:		mov  eax,1
			xor  ecx,ecx
		      cpuid
			and  ecx,0x00080000
			 jz  .Error
			 or  dword[SupportedPaths],_QUADRATIC_FPU
			 or  dword[SupportedPaths],_QUADRATIC_SSE128
.CheckAVX:		mov  eax,1
			xor  ecx,ecx
		      cpuid
			and  ecx,0x18000000
			cmp  ecx,0x18000000
			jne  .CheckDone
			mov  ecx,0
		     xgetbv
			and  eax,0x06
			cmp  eax,0x06
			jne  .CheckDone
			 or  dword[SupportedPaths],_QUADRATIC_AVX256
.Check4FMA:		mov  eax,0x80000001
			xor  ecx,ecx
		      cpuid
			and  ecx,0x00010000
			 jz  .Check3FMA
			 or  dword[SupportedPaths],_QUADRATIC_4FMA256
.Check3FMA:		mov  eax,1
			xor  ecx,ecx
		      cpuid
			and  ecx,0x00001000
			 jz  .CheckDone
			 or  dword[SupportedPaths],_QUADRATIC_3FMA256
.CheckDone:

		     invoke  GetCurrentProcess
		     invoke  GetProcessAffinityMask,rax,ProcessAffinityMask,SystemAffinityMask

			lea  rdi,[ThreadMaskArray]
			mov  rax,[ProcessAffinityMask]
			xor  ecx,ecx
		  @@:	mov  rbx,rax
			sub  rax,1
			and  rax,rbx
			xor  rbx,rax
			add  ecx,1
			mov  [rdi],rbx
			add  rdi,8
		       test  rax,rax
			jnz  @b
			mov  [CoreCount],ecx

		     invoke  QueryPerformanceFrequency,Frequency

		     invoke  GetModuleHandle,0
			mov  [wc.hInstance],rax
			mov  [hInstance],rax
		     invoke  LoadCursor,0,IDC_ARROW
			mov  [wc.hCursor],rax
		     invoke  RegisterClass,wc
		       test  rax,rax
			 jz  .Error

			lea  rcx,[CoolColors]
		       call  CalculateColors

		     invoke  CreateWindowEx,0,_class,_title,WS_VISIBLE+WS_OVERLAPPEDWINDOW,100,100,200,200,NULL,NULL,[wc.hInstance],NULL
			mov  [hMainWindow],rax
		       test  rax,rax
			 jz  .Error

		       call  CorrectMainWindowSize



  .MsgLoop:	     invoke  PeekMessage,msg,NULL,0,0,PM_REMOVE
			cmp  [msg.message],WM_QUIT
			 je  .EndLoop
		     invoke  TranslateMessage,msg
		     invoke  DispatchMessage,msg
		       call  Paint
		     invoke  Sleep,10
			jmp  .MsgLoop


  .Error:	      invoke  MessageBox,NULL,_error,NULL,MB_ICONERROR+MB_OK

  .EndLoop:	      invoke  ExitProcess,[msg.wParam]





proc WindowProc hwnd,wmsg,wparam,lparam

			mov  [hwnd],rcx
			mov  [wmsg],rdx
			mov  [wparam],r8
			mov  [lparam],r9
			cmp  edx,WM_CREATE
			 je  .wmcreate
			cmp  edx,WM_SIZE
			 je  .wmsize
			cmp  edx,WM_PAINT
			 je  .wmpaint
			cmp  edx,WM_DESTROY
			 je  .wmdestroy
			cmp  edx,WM_KEYDOWN
			 je  .wmkeydown
			cmp  edx,WM_LBUTTONDOWN
			 je  .wmlbuttondown
			cmp  edx,WM_RBUTTONDOWN
			 je  .wmrbuttondown
			cmp  edx,WM_LBUTTONUP
			 je  .wmlbuttonup
			cmp  edx,WM_RBUTTONUP
			 je  .wmrbuttonup
			cmp  edx,WM_MOUSEMOVE
			 je  .wmmousemove



  .defwndproc:	     invoke  DefWindowProc,rcx,rdx,r8,r9
			jmp  .finish

  .wmkeydown:		cmp  r8d,'M'
			 je  .kdM
			cmp  r8d,'L'
			 je  .kdL
			cmp  r8d,'R'
			 je  .kdR
			cmp  r8d,'T'
			 je  .kdT
			cmp  r8d,'C'
			 je  .kdC
			cmp  r8d,'S'
			 je  .kdS

			cmp  r8d,'1'
			 je  .kd1
			cmp  r8d,'2'
			 je  .kd2
			cmp  r8d,'3'
			 je  .kd3
			cmp  r8d,'4'
			 je  .kd4
			cmp  r8d,'5'
			 je  .kd5
			cmp  r8d,'6'
			 je  .kd6
			cmp  r8d,'7'
			 je  .kd7
			cmp  r8d,'8'
			 je  .kd8
			cmp  r8d,'9'
			 je  .kd9

			jmp  .returnz


	.kd1:		lea  rcx,[Colors+1024*0]
			jmp  .kdcolors
	.kd2:		lea  rcx,[Colors+1024*1]
			jmp  .kdcolors
	.kd3:		lea  rcx,[Colors+1024*2]
			jmp  .kdcolors
	.kd4:		lea  rcx,[Colors+1024*3]
			jmp  .kdcolors
	.kd5:		lea  rcx,[Colors+1024*4]
			jmp  .kdcolors
	.kd6:		lea  rcx,[Colors+1024*5]
			jmp  .kdcolors
	.kd7:		lea  rcx,[Colors+1024*6]
			jmp  .kdcolors
	.kd8:		lea  rcx,[Colors+1024*7]
			jmp  .kdcolors
	.kd9:		lea  rcx,[Colors+1024*8]
			jmp  .kdcolors


	.kdcolors:     call  CalculateColors
		       call  PaintAA
			jmp  .returnz

	.kdS:		not  [StatsQ]
		       call  PaintAA
			jmp  .returnz

	.kdR:		mov  ecx,PATH_COUNT
			mov  eax,dword[RenderPath]
		  @@:	add  eax,1
			xor  edx,edx
			div  ecx
			mov  eax,edx
			 bt  dword[SupportedPaths],edx
			jnc  @b
			mov  dword[RenderPath],eax
		       call  PaintAA
			jmp  .returnz

	.kdT:	       fldz
		       fstp  [Center.x]
		       fldz
		       fstp  [Center.y]
		       fld1
		       fadd  st0,st0
		       fadd  st0,st0
		       fstp  qword[Radius]
		       call  PaintAA
			jmp  .returnz

	.kdM:		add  [Depth],100
		       call  PaintAA
			jmp  .returnz

	.kdL:		sub  [Depth],100
			jle  .kdM
		       call  PaintAA
			jmp  .returnz

	.kdC:	     invoke  OpenClipboard,[hMainWindow]
		       test  rax,rax
			 jz  .returnz
		     invoke  GlobalAlloc,GHND+GMEM_SHARE,1024
		       test  rax,rax
			 jz  .returnz
			mov  [hglbCopy],rax
		     invoke  GlobalLock,[hglbCopy]
			mov  rdi,rax
			lea  rsi,[Message]
			mov  ecx,1024
		repnz movsb
		     invoke  GlobalUnlock,[hglbCopy]
		     invoke  EmptyClipboard
		     invoke  SetClipboardData,CF_TEXT,[hglbCopy]
		     invoke  CloseClipboard


  .wmmousemove:       movzx  eax,word[lparam+2*0]
			mov  [Mouse.x],eax
		      movzx  eax,word[lparam+2*1]
			mov  [Mouse.y],eax
			jmp  .returnz

  .wmlbuttondown:     movzx  eax,word[lparam+2*0]
			mov  [Mouse.x],eax
		      movzx  eax,word[lparam+2*1]
			mov  [Mouse.y],eax
			 or  [Mouse],MOUSE_LEFT
		       call  Paint
			jmp  .returnz

  .wmrbuttondown:     movzx  eax,word[lparam+2*0]
			mov  [Mouse.x],eax
		      movzx  eax,word[lparam+2*1]
			mov  [Mouse.y],eax
			 or  [Mouse],MOUSE_RIGHT
		       call  Paint
			jmp  .returnz

  .wmlbuttonup:       movzx  eax,word[lparam+2*0]
			mov  [Mouse.x],eax
		      movzx  eax,word[lparam+2*1]
			mov  [Mouse.y],eax
			and  [Mouse],not MOUSE_LEFT
		       call  PaintAA
			jmp  .returnz

  .wmrbuttonup:       movzx  eax,word[lparam+2*0]
			mov  [Mouse.x],eax
		      movzx  eax,word[lparam+2*1]
			mov  [Mouse.y],eax
			and  [Mouse],not MOUSE_RIGHT
		       call  PaintAA
			jmp  .returnz


  .wmcreate:

  .wmsize:	     invoke  GetClientRect,[hwnd],rect
			mov  eax,[rect.right] 
		       test  eax,eax 
			 jz  .returnz
			mov  eax,[rect.right] 
			mov  ecx,[rect.bottom]
			mov  edx,64
			and  eax,-16
			and  ecx,-16
			cmp  eax,edx
		      cmovb  eax,edx
			cmp  ecx,edx
		      cmovb  ecx,edx
			mov  [Bitmap.x],eax 
			mov  [Bitmap.y],ecx 
			mov  rcx,[hBitmap]
		       test  rcx,rcx
			 jz  .wmpaint_newbm 
			jmp  .wmpaint_redobm 

  .wmpaint:	     invoke  GetClientRect,[hMainWindow],rect 
			mov  eax,[rect.right] 
			mov  rcx,[hBitmap]
		       test  eax,eax 
			 jz  .returnz 
		       test  rcx,rcx
			 jz  .wmpaint_newbm 
    .wmpaint_ok:       call  PaintAA
			jmp  .returnz 
    .wmpaint_redobm: invoke  DeleteObject,[hBitmap] 
    .wmpaint_newbm:	mov  [bmiheader.biSize],sizeof.BITMAPINFOHEADER 
			mov  eax,[Bitmap.x] 
			mov  [bmiheader.biWidth],eax 
			mov  eax,[Bitmap.y] 
			neg  eax 
			mov  [bmiheader.biHeight],eax 
			mov  [bmiheader.biPlanes],1 
			mov  [bmiheader.biBitCount],32 
			mov  [bmiheader.biCompression],BI_RGB 
			mov  eax,[Bitmap.x] 
		       imul  eax,[Bitmap.y] 
			lea  eax,[4*rax+16]
			mov  [bmiheader.biSizeImage],eax 
			mov  [bmiheader.biXPelsPerMeter],0 
			mov  [bmiheader.biYPelsPerMeter],0 
			mov  [bmiheader.biClrUsed],0 
			mov  [bmiheader.biClrImportant],0 
		     invoke  CreateDIBSection,0,bmiheader,DIB_RGB_COLORS,ppvBits,0,0 
			mov  [hBitmap],rax

		       call  CalculateCoords
			jmp  .wmpaint_ok


  .wmdestroy:	     invoke  PostQuitMessage,0
		     invoke  DeleteObject,[hBitmap]
  .returnz:		xor  eax,eax
  .finish:		ret

endp






PaintAA:		mov  ecx,dword[RenderPath]
			mov  rcx,qword[RenderPaths_AA+8*rcx]
			cmp  [Mouse],0
			 je  Paint.ready
			ret

Paint:			cmp  [Mouse],MOUSE_LEFT
			 je  .zoomin
			cmp  [Mouse],MOUSE_RIGHT
			 je  .zoomout
			ret

    .zoomin:		fld  qword[ZoomFactor]
		       call  Zoom
			jmp  .go

    .zoomout:		fld  qword[ZoomFactor]
		       fld1
		     fdivrp  st1,st0
		       call  Zoom
			jmp  .go

    .go:		mov  ecx,dword[RenderPath]
			mov  rcx,qword[RenderPaths_noAA+8*rcx]
    .ready:
			cmp  qword[hMainWindow],0
			 je  .end

		       push  rbp rdi r15
			mov  r15,rcx
		     invoke  InvalidateRect,qword[hMainWindow],rect,FALSE
		     invoke  BeginPaint,qword[hMainWindow],ps
			mov  qword[hdc],rax
		     invoke  CreateCompatibleDC,rax
			mov  qword[hMemDC],rax
		     invoke  SelectObject,rax,qword[hBitmap]
		       call  PlotMain
		     invoke  BitBlt,[hdc],0,0,[rect.right],[rect.bottom],[hMemDC],0,0,SRCCOPY
		     invoke  DeleteDC,qword[hMemDC]
		     invoke  EndPaint,qword[hMainWindow],ps
			pop  r15 rdi rbp

       .end:		ret







PlotMain:	       push  rbp rbx rsi rdi r15

 .DrawImage:

		       call  CalculateCoords

			xor  eax,eax
			mov  [ITERs],rax

			mov  ebx,[CoreCount]
			sub  ebx,1
			lea  rsi,[hThreadArray]
			lea  rdi,[ThreadMaskArray]
	      @@:    invoke  CreateThread,0,0,r15,rbx,CREATE_SUSPENDED,0
			mov  [rsi],rax
		     invoke  SetThreadAffinityMask,rax,qword[rdi]
			add  rsi,8
			add  rdi,8
			sub  ebx,1
			jae  @b

		     invoke  QueryPerformanceCounter,Time1

			mov  ebx,[CoreCount]
			lea  rsi,[hThreadArray]
	      @@:    invoke  ResumeThread,qword[rsi]
			add  rsi,8
			sub  ebx,1
			jnz  @b

		     invoke  WaitForMultipleObjectsEx,qword[CoreCount],hThreadArray,1,-1,0

		     invoke  QueryPerformanceCounter,Time2

			mov  ebx,[CoreCount]
			lea  rsi,[hThreadArray]
	      @@:    invoke  CloseHandle,qword[rsi]
			add  rsi,8
			sub  ebx,1
			jnz  @b


			cmp  [StatsQ],0
			jne  .Done

 .WriteStats:

			lea  rdi,[Message]
			lea  rsi,[FixedMessage]
		      lodsb
		@@:   stosb
		      lodsb
		       test  al,al
			jnz  @b
			mov  al,0x0d
		      stosb

			mov  rax,'Render: '
		      stosq
			mov  eax,dword[RenderPath]
			shl  eax,4
		     movups  xmm0,dqword[RenderPathsStrings+rax]
		     movups  dqword[rdi],xmm0
			add  rdi,16
			mov  eax,0x0d
		      stosb

			mov  rax,'Depth:  '
		      stosq
			mov  eax,[Depth]
			shl  eax,2
		       call  PrintInteger
			mov  eax,0x0d
		      stosb

			mov  rax,'Zoom:   '
		      stosq
		       fld1
			fld  qword[Radius]
		      fyl2x
		       fchs
		       call  PrintFloat
			mov  al,0x0d
		      stosb

			mov  rax,'Center: '
		      stosq
			fld  qword[Center.x]
		       call  PrintFloat
			fld  qword[Center.y]
		       call  PrintFloat
			mov  ax,'*'+(178 shl 8)
		      stosw
			mov  al,0x0d
		      stosb



		       fild  [Time2]
		       fild  [Time1]
		      fsubp  st1,st0
		       fild  [Frequency]
		      fdivp  st1,st0
			fld  [const_f8_10p3]
		      fmulp  st1,st0
		      fistp  dword[rsp-8]
			mov  rax,'Time:   '
		      stosq
			mov  eax,dword[rsp-8]
		       call  PrintInteger
			mov  eax,'ms'+(0x0d shl 16)
		      stosd
			sub  rdi,1


		       fild  qword[ITERs]
		       fild  [Time2]
		       fild  [Time1]
		      fsubp  st1,st0
		       fild  [Frequency]
		      fdivp  st1,st0
			fld  [const_f8_10p6]
		      fmulp  st1,st0
		      fdivr  st0,st1
		      fistp  dword[rsp-8]
		       fstp  st0
			mov  rax,'IPS:    '
		      stosq
			mov  eax,dword[rsp-8]
		       call  PrintInteger
			mov  ax,'M'+(0x0d shl 8)
		      stosw

			mov  rax,'Size:   '
		      stosq
			mov  eax,dword[Bitmap.y]
		       call  PrintInteger
			mov  al,'x'
		      stosb
			mov  eax,dword[Bitmap.x]
		       call  PrintInteger


			mov  al,0
		      stosb


SMCHAR_HEIGHT = 11
SMCHAR_WIDTH  = 8

			lea  rdi,[Message]
			xor  eax,eax
			xor  edx,edx
			jmp  .w2
	      .w0:	xor  eax,eax
			add  edx,SMCHAR_HEIGHT
			jmp  .w2
	      .w1:     movd  xmm3,dword[TextColor]
		       call  DrawCharacter
			add  eax,SMCHAR_WIDTH
	      .w2:    movzx  ecx,byte[rdi]
			add  rdi,1
			cmp  ecx,0x0d
			 je  .w0
		       test  ecx,ecx
			jnz  .w1

			mov  eax,[Bitmap.y]
			add  eax,1
		       imul  eax,[Bitmap.x]
			shl  eax,1
			add  rax,[ppvBits]
			xor  dword[rax],0x00FFFFFF

.Done:

			pop  r15 rdi rsi rbx rbp
			ret



include 'QUADRATIC_SSE128.asm'
include 'QUADRATIC_AVX256.asm'
include 'QUADRATIC_4FMA256.asm'
include 'QUADRATIC_3FMA256.asm'
include 'QUADRATIC_FPU.asm'


Zoom:			fld  [Center.x]
		       fild  [Mouse.x]
		       fild  [Bitmap.x]
			fld  [Radius.x]
			fld  st4
		       fmul  st0,st1
		       fsub  st1,st0
		       fstp  [Radius.x]
		       fxch  st2
		       fadd  st0,st0
		     fdivrp  st1,st0
		       fld1
		      fsubp  st1,st0
		      fmulp  st1,st0
		      faddp  st1,st0
		       fstp  [Center.x]
			fld  [Center.y]
		       fild  [Mouse.y]
		       fild  [Bitmap.y]
			fld  [Radius.y]
			fld  st4
		       fmul  st0,st1
		       fsub  st1,st0
		       fstp  [Radius.y]
		       fxch  st2
		       fadd  st0,st0
		     fdivrp  st1,st0
		       fld1
		      fsubp  st1,st0
		      fmulp  st1,st0
		      faddp  st1,st0
		       fstp  [Center.y]
			fld  qword[Radius]
		      fmulp  st1,st0
		       fstp  qword[Radius]
			ret


CalculateColors:	mov  rax,rcx
		       pxor  xmm7,xmm7
			lea  rdx,[ColorTable]
			mov  r8d,256
	      @@:      movd  xmm0,[rcx]
		  punpcklbw  xmm0,xmm7
		       movq  [rdx],xmm0
			add  rcx,4
			add  rdx,8
			sub  r8d,1
			jnz  @b
		       movd  xmm0,[rax]
		  punpcklbw  xmm0,xmm7
		       movq  [rdx],xmm0
			ret



CalculateCoords:
		      fldcw  [f10Control]

		       fild  dword[Bitmap.y]
		       fild  dword[Bitmap.x]
			fld  st1
		       fmul  st0,st2
			fld  st1
		       fmul  st0,st2
		      faddp  st1,st0
		      fsqrt
			fld  qword[Radius]
		     fdivrp  st1,st0
		       fmul  st1,st0
		      fmulp  st2,st0

			lea  rbx,[xcoords]
			mov  esi,[Bitmap.x]
		       fild  dword[Bitmap.x]
			fld  qword[const_f8_2]
		     fdivrp  st1,st0
		       fld1
		       fchs
		  @@:	fld  st0
		       fmul  st0,st3
			fld  qword[Center.x]
		      faddp  st1,st0
		       fstp  qword[rbx]
			add  rbx,8
		       fadd  st0,st1
			sub  esi,1
			jns  @b
		       fstp  st0
		       fstp  st0
		       fstp  qword[Radius.x]

			lea  rbx,[ycoords]
			mov  edi,[Bitmap.y]
		       fild  dword[Bitmap.y]
			fld  qword[const_f8_2]
		     fdivrp  st1,st0
		       fld1
		       fchs
		  @@:	fld  st0
		       fmul  st0,st3
			fld  qword[Center.y]
		      faddp  st1,st0
		       fstp  qword[rbx]
			add  rbx,8
		       fadd  st0,st1
			sub  edi,1
			jns  @b
		       fstp  st0
		       fstp  st0
		       fstp  qword[Radius.y]

			ret






DrawCharacter:	   ; ecx: char  (from rsi)
		   ; eax,edx :  x,y coor



		       push  rax
		       push  rbx
		       push  rcx
		       push  rdx

			mov  rbx,[ppvBits]
		       imul  edx,[Bitmap.x]
			lea  rbx,[rbx+4*rax]
			lea  rbx,[rbx+4*rdx]

			mov  eax,SMCHAR_WIDTH
			add  eax,1
			shr  eax,1
		       imul  ecx,SMCHAR_HEIGHT
		       imul  ecx,eax
			add  rcx,SmallCharacterTable

		     pshufd  xmm3,xmm3,0
		       pxor  xmm7,xmm7
		     movdqa  xmm5,dqword[const_i2x8_8]
		  punpcklbw  xmm3,xmm7

			mov  r10,0x0001000100010001

			mov  r8d,SMCHAR_HEIGHT
	       .l1:	mov  r9d,SMCHAR_WIDTH
		.l2:
		      movzx  eax,byte[rcx]
		      movzx  edx,byte[rcx]
			add  rcx,1
			and  eax,0x0F
			shr  edx,4
		       imul  rax,r10
		       imul  rdx,r10
		       movq  xmm4,[rbx]
		       movq  xmm0,rdx
		       movq  xmm1,rax
		     pslldq  xmm0,8
			por  xmm0,xmm1	    ; xmm0 = a
		  punpcklbw  xmm4,xmm7
		      psrlw  xmm4,1
		     movdqa  xmm1,xmm5
		      psubw  xmm1,xmm0	    ; xmm1 = 8-a
		     pmullw  xmm0,xmm3
		     pmullw  xmm1,xmm4
		      paddw  xmm0,xmm1
		      psrlw  xmm0,3
		   packuswb  xmm0,xmm0
		       movq  [rbx],xmm0
			add  rbx,8
			sub  r9d,2
			 jg  .l2

			mov  r9d,SMCHAR_WIDTH
			add  r9d,1
			and  r9d,-2
			mov  eax,[Bitmap.x]
			sub  eax,r9d
			lea  rbx,[rbx+4*rax]
			sub  r8d,1
			jnz  .l1

			pop  rdx
			pop  rcx
			pop  rbx
			pop  rax

			ret


CorrectMainWindowSize:
		       push  rbp
			mov  esi,START_WIDTH
			mov  edi,START_HEIGHT
		     invoke  MoveWindow,dword[hMainWindow],0,0,esi,edi,TRUE
		     invoke  GetClientRect,[hMainWindow],rect
			mov  eax,2*START_WIDTH
			mov  ecx,2*START_HEIGHT
			sub  eax,dword[rect.right]
			sub  ecx,dword[rect.bottom]
			mov  r15d,eax
			mov  r14d,ecx
		     invoke  GetSystemMetrics,SM_CXSCREEN
			mov  esi,eax
		     invoke  GetSystemMetrics,SM_CYSCREEN
			mov  edi,eax
			shr  esi,1
			shr  edi,1
			mov  eax,START_WIDTH/2
			mov  ecx,START_HEIGHT/2
			sub  esi,eax
			sub  edi,ecx
			mov  eax,esi
			sar  eax,31
			not  eax
			and  esi,eax
			mov  eax,edi
			sar  eax,31
			not  eax
			and  edi,eax
		     invoke  MoveWindow,dword[hMainWindow],rsi,rdi,r15,r14,TRUE
			pop  rbp
			ret





PrintFloat:
virtual at rsp
.x	 dq ?,?,?,?
.d	 dq ?
.ex	 dq ?
.ii	 dq ?
.message dq ?
.rsi	 dq ?
.rbp	 dq ?
.rbx	 dq ?
.r12	 dq ?
.r13	 dq ?
.s	 dq ?
end virtual

			sub  rsp,8*(65)
			mov  [.rsi],rsi
			mov  [.rbp],rbp
			mov  [.rbx],rbx
			mov  [.r12],r12
			mov  [.r13],r13

			fld  st0
		       fstp  tword[.x]
		      movzx  edx,word[.x+8]
			mov  ecx,edx
			and  ecx,0x08000
			 jz  @f
		       fchs
			mov  al,'-'
		      stosb
		      @@:
			and  edx,0x07FFF
			 jz  .zero
			cmp  edx,0x07FFF
			 je  .inf
			mov  [.message],rdi

		       fld1
		       fld1
		       fadd  st0,st0
		      fdivp  st1,st0

		     fldlg2
		       fild  qword[OutputBits]
		      fmulp  st1,st0
		      faddp  st1,st0
		      fistp  qword[.d]
			mov  eax,1
			mov  qword[.ex],rax
		     fldlg2
			fld  st1
		      fyl2x
		      fistp  qword[.ii]
			lea  rdi,[.x]
		       call  CVT_f80_F192
			lea  rdi,[.s]
			lea  rsi,[.x]
			mov  rbx,[.ex]
			add  rbx,[.d]
			sub  rbx,[.ii]
		       call  POWMUL_F192
			sub  rax,1
			sub  rax,rbx
			add  rdi,[.d]
		 @@:	sub  rdi,1
			cmp  byte[rdi],'0'
			 je  @b
			add  rdi,1

			mov  rcx,rdi
			lea  rsi,[.s]
			mov  rdi,[.message]
			sub  rcx,rsi

			cmp  rax,16
			 jg  .print_w_exp
			cmp  rax,-8
			 jl  .print_w_exp
		       test  rax,rax
			 jz  .print_no_exp
			 js  .print_neg_exp
		.print_pos_exp:
			cmp  eax,ecx
			lea  eax,[rax+1]
			jae  @f
		       push  rax
		       push  rcx
			mov  ecx,eax
		  rep movsb
			mov  al,'.'
		      stosb
			pop  rcx
			pop  rax
			sub  ecx,eax
		  rep movsb
			jmp  .done
	     @@:	sub  eax,ecx
		  rep movsb
			mov  ecx,eax
			mov  al,'0'
		  rep stosb
			mov  al,'.'
		      stosb
			jmp  .done

		.print_neg_exp:
		       push  rcx
		       push  rax
			mov  ax,'0.'
		      stosw
			pop  rcx
			not  rcx
			mov  al,'0'
		  rep stosb
			pop  rcx
		  rep movsb
			jmp  .done

		.print_no_exp:
		      movsb
			sub  ecx,1
			mov  al,'.'
		      stosb
		  rep movsb
			jmp  .done

		.print_w_exp:
		      movsb
			sub  ecx,1
		       push  rax
			mov  al,'.'
		      stosb
		  rep movsb
			mov  al,'e'
		      stosb
			pop  rax
		       call  PrintInteger
		       ; jmp  .done

   .done:
			mov  rsi,[.rsi]
			mov  rbp,[.rbp]
			mov  rbx,[.rbx]
			mov  r12,[.r12]
			mov  r13,[.r13]
			add  rsp,8*65
			ret


  .zero:		mov  ax,'0.'
		       fstp  st0
		      stosw
			jmp  .done

 .inf:			mov  rax,[.x]
		       fstp  st0
		       test  rax,rax
			jns  .Nan
			shl  rax,1
			jnz  .Nan
			mov  eax,'inf'
		      stosd
			sub  rdi,1
			jmp  .done

	.Nan:
			mov  eax,'NaN'
		      stosd
			sub  rdi,1
			jmp  .done



CVT_f80_F192: ; pop fpu stack to rdi
			sub  rsp,8*5
		       fstp  tword[rsp]
			xor  eax,eax
			mov  [rdi+8*0],rax
			mov  [rdi+8*1],rax
		      movsx  ecx,word[rsp+8]
			and  ecx,0x7FFFF
			sub  rcx,16383
			mov  rax,qword[rsp+0]
			mov  [rdi+8*2],rax
			mov  [rdi+8*3],rcx
			add  rsp,8*5
			ret






POWMUL_F192:	;  [rdi] = string(round([rsi] * 10^rbx))

virtual at rsp
  .base  dq ?,?,?,?
  .unity dq ?,?,?,?
  .rdi	 dq ?
  .rsi	 dq ?
  .rbx	 dq ?
end virtual
			sub  rsp,8*13
			mov  [.rdi],rdi
			mov  [.rbx],rbx
			mov  [.rsi],rsi

		     movaps  xmm0,dqword[Bases+32*1+16*0]
		     movaps  xmm1,dqword[Bases+32*1+16*1]
		     movups  dqword[.unity+16*0],xmm0
		     movups  dqword[.unity+16*1],xmm1

			mov  rax,qword[OutputBase]
		       test  rbx,rbx
			 jz  .zero
			jns  @f
			neg  rax
			neg  rbx
		   @@:
			shl  rax,5
		     movaps  xmm0,dqword[Bases+rax+16*0]
		     movaps  xmm1,dqword[Bases+rax+16*1]
		     movups  dqword[.base+16*0],xmm0
		     movups  dqword[.base+16*1],xmm1

		       test  rbx,rbx
			 jz  .w3
	      .w1:	shr  rbx,1
			jnc  .w2
			lea  rdi,[.unity]
			lea  rsi,[.base]
		       call  MUL_F192
	      .w2:     test  rbx,rbx
			 jz  .w3
			lea  rdi,[.base]
			lea  rsi,[.base]
		       call  MUL_F192
			jmp  .w1
	      .w3:
	.zero:		lea  rdi,[.unity]
			mov  rsi,[.rsi]
		       call  MUL_F192

			; rax = sign bit
			; ecx = exponent

			sub  rcx,2*64-1
			jns  .error

		@@:	shr  r13,1
			rcr  r12,1
			rcr  r11,1
			add  rcx,1
			jnz  @b

			cmp  r11,rax
			 jb  .store
			 ja  .roundup
		       test  r12,1
			 jz  .store
     .roundup:		add  r12,1
			adc  r13,0
    .store:
			mov  rdi,[.rdi]
			mov  rbp,rsp
	.ComputeDigits: xor  edx,edx
			mov  rax,r13
			div  qword[OutputBase]
			mov  r13,rax
			mov  rcx,rax
			mov  rax,r12
			div  qword[OutputBase]
			mov  r12,rax
			 or  rcx,rax
		       push  rdx
			jnz  .ComputeDigits
	.PrintDigits:	pop  rax
			add  al,'0'
		      stosb
			cmp  rsp,rbp
			 jb  .PrintDigits

			mov  rax,rdi
			sub  rax,[.rdi]
			mov  rdi,[.rdi]
			mov  rsi,[.rsi]
			mov  rbx,[.rbx]

			add  rsp,8*13
			ret

.error: 	       int3


MUL_F192:		mov  rcx,[rsi+8*3]
			add  rcx,[rdi+8*3]

			mov  rax,[rsi+8*0]
			mul  qword[rdi+8*0]
			mov  r8,rax
			mov  r9,rdx
			xor  r10,r10
			xor  r11,r11
			xor  r12,r12
			xor  r13,r13

			mov  rax,[rsi+8*1]
			mul  qword[rdi+8*0]
			add  r9,rax
			adc  r10,rdx
			adc  r11,0
			mov  rax,[rsi+8*0]
			mul  qword[rdi+8*1]
			add  r9,rax
			adc  r10,rdx
			adc  r11,0

			mov  rax,[rsi+8*2]
			mul  qword[rdi+8*0]
			add  r10,rax
			adc  r11,rdx
			adc  r12,0
			mov  rax,[rsi+8*1]
			mul  qword[rdi+8*1]
			add  r10,rax
			adc  r11,rdx
			adc  r12,0
			mov  rax,[rsi+8*2]
			mul  qword[rdi+8*0]
			add  r10,rax
			adc  r11,rdx
			adc  r12,0

			mov  rax,[rsi+8*2]
			mul  qword[rdi+8*1]
			add  r11,rax
			adc  r12,rdx
			adc  r13,0
			mov  rax,[rsi+8*1]
			mul  qword[rdi+8*2]
			add  r11,rax
			adc  r12,rdx
			adc  r13,0

			mov  rax,[rsi+8*2]
			mul  qword[rdi+8*2]

			add  r12,rax
			adc  r13,rdx


			 js  @f
			shl  r8,1
			rcl  r9,1
			rcl  r10,1
			rcl  r11,1
			rcl  r12,1
			rcl  r13,1
			jmp  .round
		  @@:	add  rcx,1

    .round:		xor  eax,eax
			bts  rax,63
			cmp  r10,rax
			 jb  .store
			 ja  .roundup
		       test  r11,1
			 jz  .store
     .roundup:		add  r11,1
			adc  r12,0
			adc  r13,0
			jnc  .store
			rcr  r13,1
			rcr  r12,1
			rcr  r11,1
			add  rcx,1

    .store:		mov  [rdi+8*0],r11
			mov  [rdi+8*1],r12
			mov  [rdi+8*2],r13
			mov  [rdi+8*3],rcx
			ret


align 32
dq 0xcccccccccccccccc, 0xcccccccccccccccc, 0xcccccccccccccccc, -4
dq 0x8e38e38e38e38e38, 0x38e38e38e38e38e3, 0xe38e38e38e38e38e, -4
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, -3
dq 0x4924924924924924, 0x2492492492492492, 0x9249249249249249, -3
dq 0xaaaaaaaaaaaaaaaa, 0xaaaaaaaaaaaaaaaa, 0xaaaaaaaaaaaaaaaa, -3
dq 0xcccccccccccccccc, 0xcccccccccccccccc, 0xcccccccccccccccc, -3
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, -2
dq 0xaaaaaaaaaaaaaaaa, 0xaaaaaaaaaaaaaaaa, 0xaaaaaaaaaaaaaaaa, -2
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, -1
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, 0
Bases: dq ?,?,?,?
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, 0
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, 1
dq 0x0000000000000000, 0x0000000000000000, 0xc000000000000000, 1
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, 2
dq 0x0000000000000000, 0x0000000000000000, 0xa000000000000000, 2
dq 0x0000000000000000, 0x0000000000000000, 0xc000000000000000, 2
dq 0x0000000000000000, 0x0000000000000000, 0xe000000000000000, 2
dq 0x0000000000000000, 0x0000000000000000, 0x8000000000000000, 3
dq 0x0000000000000000, 0x0000000000000000, 0x9000000000000000, 3
dq 0x0000000000000000, 0x0000000000000000, 0xa000000000000000, 3




PrintInteger:	    ; rax: number
		       push  rbp
		       push  rdx
			mov  rbp,rsp
		       test  rax,rax
			jns  .l1
			mov  byte[rdi],'-'
			add  rdi,1
			neg  rax
		.l1:	xor  edx,edx
			div  [OutputBase]
		       push  rdx
		       test  rax,rax
			jnz  .l1
		.l2:	pop  rax
			add  al,'0'
		      stosb
			cmp  rsp,rbp
			 jb  .l2
			pop  rdx
			pop  rbp
			ret


section '.data' data readable writeable

include 'SmallChar.asm'


align 32
    const_f8_R	     dq 15.0,15.0,15.0,15.0
    const_f8v4_2     dq 2.0,2.0,2.0,2.0
    const_f8v4_1d2   dq 0.5,0.5,0.5,0.5

align 16
    const_i2x8_8     dw 8,8,8,8,8,8,8,8

align 8
  RenderPaths_AA    dq QUADRATIC_SSE128,     QUADRATIC_AVX256,	   QUADRATIC_4FMA256,	  QUADRATIC_3FMA256	,     QUADRATIC_FPU
  RenderPaths_noAA  dq QUADRATIC_SSE128_noAA,QUADRATIC_AVX256_noAA,QUADRATIC_4FMA256_noAA,QUADRATIC_3FMA256_noAA,     QUADRATIC_FPU_noAA

align 8
  wc WNDCLASS 0,WindowProc,0,0,NULL,NULL,NULL,COLOR_BTNFACE+1,NULL,_class
  rq 4
  OutputBase	   dq 10
  OutputBits	   dq 50
  const_f8_1	   dq 1.0
  const_f8_2	   dq 2.0
  const_f8_4	   dq 4.0
  const_f8_1d2	   dq 0.5
  const_f8_1dlgR   dq 0.255958024809815489387677678865
  const_f8_256	   dq 256.0
  const_f8_10p3    dq 1000.0,0
  const_f8_10p6    dq 1000000.0
  const_i2x4_256   dw 256,256,256,256
  const_i2x4_1	   dw 1,1,1,1

  Center:
     .x        dq 0.0
     .y        dq 0.0
  Radius:      dq 4.0,?
     .x        dq ?
     .y        dq ?
  ZoomFactor   dq 0.90

Colors:
 SunsetColors:	   dd 0x00000000,0x00040206,0x0009030c,0x000d0512,0x00120618,0x0016081e,0x001b0a24,0x001f0b2a,0x00240d30,0x00280f36,0x002d103d,0x00311243,0x00351349,0x003a154f,0x003e1755,0x0043185b,0x00471a61,0x004c1c67,0x00501d6d,0x00551f73,0x00592079,0x005e227f,0x0062247f,0x0067257c,0x006c277a,0x00712877,0x00762a74,0x007b2b71,0x00802d6e,0x00852e6c,0x008a2f69,0x008f3166,0x00943263,0x00993460,0x009e355d,0x00a3375b,0x00a83858,0x00ad3a55,0x00b23b52,0x00b73d4f,0x00bc3e4d,0x00c1404a,0x00c64147,0x00ca4344,0x00cc4542,0x00ce483f,0x00d14a3c,0x00d34c3a,0x00d54e37,0x00d75134,0x00da5332,0x00dc552f,0x00de582d,0x00e15a2a,0x00e35c27,0x00e55f25,0x00e76122,0x00ea631f,0x00ec651d,0x00ee681a,0x00f16a18,0x00f36c15,0x00f56f12,0x00f77110,0x00fa730d,0x00fa760e,0x00fa790f,0x00fa7b10,0x00fb7e11,0x00fb8112,0x00fb8413,0x00fb8614,0x00fc8915,0x00fc8c15,0x00fc8f16,0x00fc9217,0x00fd9418,0x00fd9719,0x00fd9a1a,0x00fd9d1b,0x00fe9f1c,0x00fea21d,0x00fea51e,0x00fea81f,0x00ffaa20,0x00ffad21,0x00ffb024,0x00ffb228,0x00ffb42d,0x00ffb731,0x00ffb935,0x00ffbc3a,0x00ffbe3e,0x00ffc042,0x00ffc347,0x00ffc54b,0x00ffc84f,0x00ffca53,0x00ffcc58,0x00ffcf5c,0x00ffd160,0x00ffd365,0x00ffd669,0x00ffd86d,0x00ffdb72,0x00ffdd76,0x00ffdf7a,0x00ffe17f,0x00ffe385,0x00ffe48b,0x00ffe692,0x00ffe798,0x00ffe89e,0x00ffeaa4,0x00ffebaa,0x00ffedb0,0x00ffeeb6,0x00fff0bc,0x00fff1c2,0x00fff2c8,0x00fff4ce,0x00fff5d4,0x00fff7db,0x00fff8e1,0x00fff9e7,0x00fffbed,0x00fffcf3,0x00fffef9,0x00ffffff,0x00fffef9,0x00fffcf3,0x00fffbed,0x00fff9e7,0x00fff8e1,0x00fff7db,0x00fff5d4,0x00fff4ce,0x00fff2c8,0x00fff1c2,0x00fff0bc,0x00ffeeb6,0x00ffedb0,0x00ffebaa,0x00ffeaa4,0x00ffe89e,0x00ffe798,0x00ffe692,0x00ffe48b,0x00ffe385,0x00ffe17f,0x00ffdf7a,0x00ffdd76,0x00ffdb72,0x00ffd86d,0x00ffd669,0x00ffd365,0x00ffd160,0x00ffcf5c,0x00ffcc58,0x00ffca53,0x00ffc84f,0x00ffc54b,0x00ffc347,0x00ffc042,0x00ffbe3e,0x00ffbc3a,0x00ffb935,0x00ffb731,0x00ffb42d,0x00ffb228,0x00ffb024,0x00ffad21,0x00ffaa20,0x00fea81f,0x00fea51e,0x00fea21d,0x00fe9f1c,0x00fd9d1b,0x00fd9a1a,0x00fd9719,0x00fd9418,0x00fc9217,0x00fc8f16,0x00fc8c15,0x00fc8915,0x00fb8614,0x00fb8413,0x00fb8112,0x00fb7e11,0x00fa7b10,0x00fa790f,0x00fa760e,0x00fa730d,0x00f77110,0x00f56f12,0x00f36c15,0x00f16a18,0x00ee681a,0x00ec651d,0x00ea631f,0x00e76122,0x00e55f25,0x00e35c27,0x00e15a2a,0x00de582d,0x00dc552f,0x00da5332,0x00d75134,0x00d54e37,0x00d34c3a,0x00d14a3c,0x00ce483f,0x00cc4542,0x00ca4344,0x00c64147,0x00c1404a,0x00bc3e4d,0x00b73d4f,0x00b23b52,0x00ad3a55,0x00a83858,0x00a3375b,0x009e355d,0x00993460,0x00943263,0x008f3166,0x008a2f69,0x00852e6c,0x00802d6e,0x007b2b71,0x00762a74,0x00712877,0x006c277a,0x0067257c,0x0062247f,0x005e227f,0x00592079,0x00551f73,0x00501d6d,0x004c1c67,0x00471a61,0x0043185b,0x003e1755,0x003a154f,0x00351349,0x00311243,0x002d103d,0x00280f36,0x00240d30,0x001f0b2a,0x001b0a24,0x0016081e,0x00120618,0x000d0512,0x0009030c,0x00040206
 TemperatureMap:   dd 0x00ee4e2e,0x00ee5131,0x00ee5434,0x00ef5837,0x00ef5b3a,0x00ef5e3d,0x00ef6240,0x00ef6543,0x00ef6847,0x00f06b4a,0x00f06f4d,0x00f07250,0x00f07554,0x00f07857,0x00f17b5a,0x00f17e5e,0x00f18161,0x00f18465,0x00f28768,0x00f28a6c,0x00f28d6f,0x00f29073,0x00f39377,0x00f3977c,0x00f39b80,0x00f49e85,0x00f4a289,0x00f4a68e,0x00f5a992,0x00f5ad97,0x00f5b19b,0x00f6b5a0,0x00f6b8a4,0x00f7bba9,0x00f7bfad,0x00f7c2b1,0x00f8c5b5,0x00f8c9ba,0x00f9ccbe,0x00f9cfc2,0x00fad3c6,0x00fad6cb,0x00fad9cf,0x00fbdcd2,0x00fbded5,0x00fbe1d8,0x00fce3db,0x00fce5de,0x00fce7e0,0x00fceae3,0x00fdece6,0x00fdeee9,0x00fdf0eb,0x00fdf2ee,0x00fcf3f0,0x00faf4f1,0x00f8f5f2,0x00f6f6f3,0x00f4f7f4,0x00f2f8f5,0x00f0f9f7,0x00eef9f8,0x00ecfaf9,0x00eafbfa,0x00e7fcfb,0x00e3fcfb,0x00dffcfc,0x00dafcfc,0x00d6fcfc,0x00d1fcfc,0x00cdfdfd,0x00c8fdfd,0x00c4fdfd,0x00c0fdfd,0x00bbfdfe,0x00b5fdfe,0x00aefdfe,0x00a7fdfe,0x009ffdfe,0x0098fcfd,0x0091fcfd,0x008afcfd,0x0082fcfd,0x007bfcfd,0x0074fcfd,0x006cfcfd,0x0069f9fc,0x0066f6fb,0x0062f3fa,0x005ff0fa,0x005bedf9,0x0058eaf8,0x0055e7f7,0x0051e4f6,0x004ee1f5,0x004adef4,0x0048daf3,0x0047d5f2,0x0046d0f1,0x0045cbf0,0x0044c7ee,0x0043c2ed,0x0042bdec,0x0041b8eb,0x0040b3e9,0x003faee8,0x003ea9e7,0x003da4e6,0x003c9fe5,0x003b9ae4,0x003a95e2,0x003990e1,0x00388be0,0x003786df,0x003781de,0x00367cdd,0x003577dc,0x003472db,0x00336ada,0x003263d9,0x00315bd8,0x003053d7,0x002f4cd6,0x002e44d5,0x002d3dd4,0x002c35d3,0x002b2ed2,0x002a26d1,0x002a26d1,0x002b2ed2,0x002c35d3,0x002d3dd4,0x002e44d5,0x002f4cd6,0x003053d7,0x00315bd8,0x003263d9,0x00336ada,0x003472db,0x003577dc,0x00367cdd,0x003781de,0x003786df,0x00388be0,0x003990e1,0x003a95e2,0x003b9ae4,0x003c9fe5,0x003da4e6,0x003ea9e7,0x003faee8,0x0040b3e9,0x0041b8eb,0x0042bdec,0x0043c2ed,0x0044c7ee,0x0045cbf0,0x0046d0f1,0x0047d5f2,0x0048daf3,0x004adef4,0x004ee1f5,0x0051e4f6,0x0055e7f7,0x0058eaf8,0x005bedf9,0x005ff0fa,0x0062f3fa,0x0066f6fb,0x0069f9fc,0x006cfcfd,0x0074fcfd,0x007bfcfd,0x0082fcfd,0x008afcfd,0x0091fcfd,0x0098fcfd,0x009ffdfe,0x00a7fdfe,0x00aefdfe,0x00b5fdfe,0x00bbfdfe,0x00c0fdfd,0x00c4fdfd,0x00c8fdfd,0x00cdfdfd,0x00d1fcfc,0x00d6fcfc,0x00dafcfc,0x00dffcfc,0x00e3fcfb,0x00e7fcfb,0x00eafbfa,0x00ecfaf9,0x00eef9f8,0x00f0f9f7,0x00f2f8f5,0x00f4f7f4,0x00f6f6f3,0x00f8f5f2,0x00faf4f1,0x00fcf3f0,0x00fdf2ee,0x00fdf0eb,0x00fdeee9,0x00fdece6,0x00fceae3,0x00fce7e0,0x00fce5de,0x00fce3db,0x00fbe1d8,0x00fbded5,0x00fbdcd2,0x00fad9cf,0x00fad6cb,0x00fad3c6,0x00f9cfc2,0x00f9ccbe,0x00f8c9ba,0x00f8c5b5,0x00f7c2b1,0x00f7bfad,0x00f7bba9,0x00f6b8a4,0x00f6b5a0,0x00f5b19b,0x00f5ad97,0x00f5a992,0x00f4a68e,0x00f4a289,0x00f49e85,0x00f39b80,0x00f3977c,0x00f39377,0x00f29073,0x00f28d6f,0x00f28a6c,0x00f28768,0x00f18465,0x00f18161,0x00f17e5e,0x00f17b5a,0x00f07857,0x00f07554,0x00f07250,0x00f06f4d,0x00f06b4a,0x00ef6847,0x00ef6543,0x00ef6240,0x00ef5e3d,0x00ef5b3a,0x00ef5837,0x00ee5434,0x00ee5131,0x00ee4e2e
 SolarColors:	   dd 0x004d002b,0x004f002c,0x0052012d,0x0054012e,0x0057022f,0x00590230,0x005b0331,0x005e0333,0x00600434,0x00630435,0x00650536,0x00670537,0x006a0638,0x006c0639,0x006f073a,0x0071073b,0x0073083c,0x0076083e,0x0078093f,0x007b0940,0x007d0a41,0x007f0a42,0x00820b43,0x00840b44,0x00870c45,0x00890c46,0x008b0d48,0x008e0d49,0x00900e4a,0x00930e4b,0x00950f4c,0x00970f4d,0x009a104e,0x009c124d,0x009d144d,0x009f164c,0x00a1184c,0x00a31a4b,0x00a51d4b,0x00a71f4a,0x00a92149,0x00aa2349,0x00ac2548,0x00ae2748,0x00b02947,0x00b22c47,0x00b42e46,0x00b53045,0x00b73245,0x00b93444,0x00bb3644,0x00bd3843,0x00bf3a43,0x00c13d42,0x00c23f42,0x00c44141,0x00c64340,0x00c84540,0x00ca473f,0x00cc493f,0x00ce4c3e,0x00cf4e3e,0x00d1503d,0x00d3523c,0x00d5543c,0x00d6573d,0x00d75a3d,0x00d85c3d,0x00d95f3e,0x00da623e,0x00db643e,0x00dd673f,0x00de6a3f,0x00df6c40,0x00e06f40,0x00e17240,0x00e27441,0x00e37741,0x00e47a41,0x00e67c42,0x00e77f42,0x00e88243,0x00e98443,0x00ea8743,0x00eb8a44,0x00ec8c44,0x00ed8f44,0x00ee9245,0x00f09445,0x00f19746,0x00f29a46,0x00f39c46,0x00f49f47,0x00f5a247,0x00f6a447,0x00f7a748,0x00f8aa49,0x00f8ac4d,0x00f9ae51,0x00f9b055,0x00f9b259,0x00f9b45d,0x00f9b661,0x00fab865,0x00faba69,0x00fabc6d,0x00fabf71,0x00fac175,0x00fbc378,0x00fbc57c,0x00fbc780,0x00fbc984,0x00fbcb88,0x00fccd8c,0x00fccf90,0x00fcd294,0x00fcd498,0x00fdd69c,0x00fdd8a0,0x00fddaa4,0x00fddca7,0x00fddeab,0x00fee0af,0x00fee2b3,0x00fee4b7,0x00fee7bb,0x00fee9bf,0x00ffebc3,0x00ffebc3,0x00fee9bf,0x00fee7bb,0x00fee4b7,0x00fee2b3,0x00fee0af,0x00fddeab,0x00fddca7,0x00fddaa4,0x00fdd8a0,0x00fdd69c,0x00fcd498,0x00fcd294,0x00fccf90,0x00fccd8c,0x00fbcb88,0x00fbc984,0x00fbc780,0x00fbc57c,0x00fbc378,0x00fac175,0x00fabf71,0x00fabc6d,0x00faba69,0x00fab865,0x00f9b661,0x00f9b45d,0x00f9b259,0x00f9b055,0x00f9ae51,0x00f8ac4d,0x00f8aa49,0x00f7a748,0x00f6a447,0x00f5a247,0x00f49f47,0x00f39c46,0x00f29a46,0x00f19746,0x00f09445,0x00ee9245,0x00ed8f44,0x00ec8c44,0x00eb8a44,0x00ea8743,0x00e98443,0x00e88243,0x00e77f42,0x00e67c42,0x00e47a41,0x00e37741,0x00e27441,0x00e17240,0x00e06f40,0x00df6c40,0x00de6a3f,0x00dd673f,0x00db643e,0x00da623e,0x00d95f3e,0x00d85c3d,0x00d75a3d,0x00d6573d,0x00d5543c,0x00d3523c,0x00d1503d,0x00cf4e3e,0x00ce4c3e,0x00cc493f,0x00ca473f,0x00c84540,0x00c64340,0x00c44141,0x00c23f42,0x00c13d42,0x00bf3a43,0x00bd3843,0x00bb3644,0x00b93444,0x00b73245,0x00b53045,0x00b42e46,0x00b22c47,0x00b02947,0x00ae2748,0x00ac2548,0x00aa2349,0x00a92149,0x00a71f4a,0x00a51d4b,0x00a31a4b,0x00a1184c,0x009f164c,0x009d144d,0x009c124d,0x009a104e,0x00970f4d,0x00950f4c,0x00930e4b,0x00900e4a,0x008e0d49,0x008b0d48,0x00890c46,0x00870c45,0x00840b44,0x00820b43,0x007f0a42,0x007d0a41,0x007b0940,0x0078093f,0x0076083e,0x0073083c,0x0071073b,0x006f073a,0x006c0639,0x006a0638,0x00670537,0x00650536,0x00630435,0x00600434,0x005e0333,0x005b0331,0x00590230,0x0057022f,0x0054012e,0x0052012d,0x004f002c,0x004d002b
 LakeColors:	   dd 0x004b0f87,0x004c1189,0x004e148c,0x0050178e,0x00511a90,0x00531d92,0x00552095,0x00562297,0x00582599,0x0059289c,0x005b2b9e,0x005d2ea0,0x005e30a2,0x006033a5,0x006236a7,0x006339a9,0x00653cab,0x00663fae,0x006841b0,0x006a44b2,0x006b47b5,0x006d4ab7,0x006f4db9,0x007050bb,0x007252be,0x007355c0,0x007558c2,0x00775bc5,0x00785ec7,0x007a60c9,0x007b63cb,0x007d66ce,0x007f69d0,0x00806cd2,0x00826fd5,0x008471d7,0x008574d9,0x008777db,0x00887ade,0x008a7de0,0x008c7fe2,0x008d82e5,0x008f85e7,0x009087e8,0x009289e8,0x00938be8,0x00948de8,0x00958fe8,0x009691e8,0x009893e8,0x009995e8,0x009a97e8,0x009b99e8,0x009c9be8,0x009d9de8,0x009f9ee8,0x00a0a0e8,0x00a1a2e8,0x00a2a4e8,0x00a3a6e8,0x00a5a8e8,0x00a6aae8,0x00a7ace8,0x00a8aee8,0x00a9b0e9,0x00abb2e9,0x00acb4e9,0x00adb5e9,0x00aeb7e9,0x00afb9e9,0x00b1bbe9,0x00b2bde9,0x00b3bfe9,0x00b4c1e9,0x00b5c3e9,0x00b7c5e9,0x00b8c7e9,0x00b9c9e9,0x00bacbe9,0x00bbcce9,0x00bdcee9,0x00bed0e9,0x00bfd2e9,0x00c0d4e9,0x00c1d6e9,0x00c2d8e9,0x00c4d8e9,0x00c5d9e8,0x00c6d9e8,0x00c7d9e7,0x00c8dae7,0x00c9dae6,0x00cadae6,0x00cbdbe5,0x00ccdbe5,0x00cddce4,0x00cedce4,0x00cfdce3,0x00d0dde3,0x00d1dde2,0x00d3dde2,0x00d4dee1,0x00d5dee1,0x00d6dee0,0x00d7dfe0,0x00d8dfdf,0x00d9dfdf,0x00dae0df,0x00dbe0de,0x00dce1de,0x00dde1dd,0x00dee1dd,0x00dfe2dc,0x00e0e2dc,0x00e2e2db,0x00e3e3db,0x00e4e3da,0x00e5e3da,0x00e6e4d9,0x00e7e4d9,0x00e8e4d8,0x00e9e5d8,0x00eae5d7,0x00ebe6d7,0x00ece6d6,0x00ede6d6,0x00eee7d5,0x00efe7d5,0x00efe7d5,0x00eee7d5,0x00ede6d6,0x00ece6d6,0x00ebe6d7,0x00eae5d7,0x00e9e5d8,0x00e8e4d8,0x00e7e4d9,0x00e6e4d9,0x00e5e3da,0x00e4e3da,0x00e3e3db,0x00e2e2db,0x00e0e2dc,0x00dfe2dc,0x00dee1dd,0x00dde1dd,0x00dce1de,0x00dbe0de,0x00dae0df,0x00d9dfdf,0x00d8dfdf,0x00d7dfe0,0x00d6dee0,0x00d5dee1,0x00d4dee1,0x00d3dde2,0x00d1dde2,0x00d0dde3,0x00cfdce3,0x00cedce4,0x00cddce4,0x00ccdbe5,0x00cbdbe5,0x00cadae6,0x00c9dae6,0x00c8dae7,0x00c7d9e7,0x00c6d9e8,0x00c5d9e8,0x00c4d8e9,0x00c2d8e9,0x00c1d6e9,0x00c0d4e9,0x00bfd2e9,0x00bed0e9,0x00bdcee9,0x00bbcce9,0x00bacbe9,0x00b9c9e9,0x00b8c7e9,0x00b7c5e9,0x00b5c3e9,0x00b4c1e9,0x00b3bfe9,0x00b2bde9,0x00b1bbe9,0x00afb9e9,0x00aeb7e9,0x00adb5e9,0x00acb4e9,0x00abb2e9,0x00a9b0e9,0x00a8aee8,0x00a7ace8,0x00a6aae8,0x00a5a8e8,0x00a3a6e8,0x00a2a4e8,0x00a1a2e8,0x00a0a0e8,0x009f9ee8,0x009d9de8,0x009c9be8,0x009b99e8,0x009a97e8,0x009995e8,0x009893e8,0x009691e8,0x00958fe8,0x00948de8,0x00938be8,0x009289e8,0x009087e8,0x008f85e7,0x008d82e5,0x008c7fe2,0x008a7de0,0x00887ade,0x008777db,0x008574d9,0x008471d7,0x00826fd5,0x00806cd2,0x007f69d0,0x007d66ce,0x007b63cb,0x007a60c9,0x00785ec7,0x00775bc5,0x007558c2,0x007355c0,0x007252be,0x007050bb,0x006f4db9,0x006d4ab7,0x006b47b5,0x006a44b2,0x006841b0,0x00663fae,0x00653cab,0x006339a9,0x006236a7,0x006033a5,0x005e30a2,0x005d2ea0,0x005b2b9e,0x0059289c,0x00582599,0x00562297,0x00552095,0x00531d92,0x00511a90,0x0050178e,0x004e148c,0x004c1189,0x004b0f87
 SouthwestColors:  dd 0x00654f34,0x00694f33,0x006d4e32,0x00714e31,0x00754d30,0x00794d2f,0x007c4c2e,0x00804c2d,0x00844b2c,0x00884a2b,0x008c4a2a,0x00904929,0x00944928,0x00984827,0x009b4826,0x009d4926,0x009e4b26,0x009f4d27,0x00a04f27,0x00a15027,0x00a35227,0x00a45427,0x00a55627,0x00a65728,0x00a75928,0x00a85b28,0x00a95d28,0x00aa5e28,0x00ab6028,0x00ac632a,0x00ad652d,0x00ae6830,0x00af6b33,0x00b06e36,0x00b17138,0x00b2743b,0x00b3773e,0x00b47a41,0x00b57c44,0x00b67f47,0x00b78249,0x00b8854c,0x00b9884f,0x00ba8a52,0x00bb8d54,0x00bc8f56,0x00bd9258,0x00bf945a,0x00c0975c,0x00c1995e,0x00c29c60,0x00c39e62,0x00c4a165,0x00c5a367,0x00c7a669,0x00c8a86b,0x00c9ab6d,0x00caad6e,0x00cbaf6e,0x00ccb16e,0x00cdb26e,0x00ceb46d,0x00cfb66d,0x00d0b86d,0x00d1ba6d,0x00d2bb6d,0x00d3bd6c,0x00d4bf6c,0x00d5c16c,0x00d6c36c,0x00d7c46c,0x00d7c66b,0x00d7c76a,0x00d7c769,0x00d7c869,0x00d6c868,0x00d6c967,0x00d6ca66,0x00d6ca65,0x00d6cb64,0x00d5cc63,0x00d5cc63,0x00d5cd62,0x00d5cd61,0x00d4ce60,0x00d4cf5f,0x00d2ce5d,0x00d0cd5c,0x00cecc5a,0x00cccb58,0x00cbca56,0x00c9c954,0x00c7c853,0x00c5c751,0x00c3c64f,0x00c1c54d,0x00bfc44c,0x00bdc34a,0x00bbc248,0x00b9c146,0x00b6bf49,0x00b3be4c,0x00b0bd4f,0x00adbb53,0x00aaba56,0x00a7b959,0x00a4b85d,0x00a1b660,0x009eb563,0x009cb467,0x0099b26a,0x0096b16d,0x0093b071,0x0090ae74,0x008cad7a,0x0088ab81,0x0085aa88,0x0081a88f,0x007da796,0x0079a59d,0x0076a3a5,0x0072a2ac,0x006ea0b3,0x006a9fba,0x00679dc1,0x00639cc8,0x005f9acf,0x005b99d6,0x005b99d6,0x005f9acf,0x00639cc8,0x00679dc1,0x006a9fba,0x006ea0b3,0x0072a2ac,0x0076a3a5,0x0079a59d,0x007da796,0x0081a88f,0x0085aa88,0x0088ab81,0x008cad7a,0x0090ae74,0x0093b071,0x0096b16d,0x0099b26a,0x009cb467,0x009eb563,0x00a1b660,0x00a4b85d,0x00a7b959,0x00aaba56,0x00adbb53,0x00b0bd4f,0x00b3be4c,0x00b6bf49,0x00b9c146,0x00bbc248,0x00bdc34a,0x00bfc44c,0x00c1c54d,0x00c3c64f,0x00c5c751,0x00c7c853,0x00c9c954,0x00cbca56,0x00cccb58,0x00cecc5a,0x00d0cd5c,0x00d2ce5d,0x00d4cf5f,0x00d4ce60,0x00d5cd61,0x00d5cd62,0x00d5cc63,0x00d5cc63,0x00d6cb64,0x00d6ca65,0x00d6ca66,0x00d6c967,0x00d6c868,0x00d7c869,0x00d7c769,0x00d7c76a,0x00d7c66b,0x00d7c46c,0x00d6c36c,0x00d5c16c,0x00d4bf6c,0x00d3bd6c,0x00d2bb6d,0x00d1ba6d,0x00d0b86d,0x00cfb66d,0x00ceb46d,0x00cdb26e,0x00ccb16e,0x00cbaf6e,0x00caad6e,0x00c9ab6d,0x00c8a86b,0x00c7a669,0x00c5a367,0x00c4a165,0x00c39e62,0x00c29c60,0x00c1995e,0x00c0975c,0x00bf945a,0x00bd9258,0x00bc8f56,0x00bb8d54,0x00ba8a52,0x00b9884f,0x00b8854c,0x00b78249,0x00b67f47,0x00b57c44,0x00b47a41,0x00b3773e,0x00b2743b,0x00b17138,0x00b06e36,0x00af6b33,0x00ae6830,0x00ad652d,0x00ac632a,0x00ab6028,0x00aa5e28,0x00a95d28,0x00a85b28,0x00a75928,0x00a65728,0x00a55627,0x00a45427,0x00a35227,0x00a15027,0x00a04f27,0x009f4d27,0x009e4b26,0x009d4926,0x009b4826,0x00984827,0x00944928,0x00904929,0x008c4a2a,0x00884a2b,0x00844b2c,0x00804c2d,0x007c4c2e,0x00794d2f,0x00754d30,0x00714e31,0x006d4e32,0x00694f33,0x00654f34
 PlumColors:	   dd 0x00000000,0x00010104,0x00020208,0x0003030c,0x00040410,0x00050514,0x00060618,0x0007061c,0x00080720,0x00080824,0x00090928,0x000a0a2c,0x000b0b30,0x000c0c34,0x000d0d38,0x000e0e3c,0x000f0f40,0x00101044,0x00111148,0x0012114a,0x0014124c,0x0016134e,0x00181450,0x00191552,0x001b1654,0x001d1756,0x001e1858,0x0020195a,0x0022195c,0x00241a5e,0x00251b60,0x00271c62,0x00291d64,0x002a1e66,0x002c1f68,0x002e206a,0x0030216c,0x0031226d,0x0033236e,0x0036246e,0x0038256f,0x003a2670,0x003c2771,0x003e2871,0x00402972,0x00422a73,0x00442b74,0x00462c75,0x00482d75,0x004a2e76,0x004c2f77,0x004e3078,0x00503178,0x00523279,0x0054337a,0x0056347a,0x0057367b,0x0059377b,0x005b397b,0x005d3a7c,0x005f3b7c,0x00603d7c,0x00623e7c,0x0064407d,0x0066417d,0x0068427d,0x006a447e,0x006b457e,0x006d477e,0x006f487f,0x0071497f,0x00734b7f,0x00744c7f,0x00764e80,0x00775080,0x00795281,0x007a5482,0x007b5582,0x007c5783,0x007d5983,0x007e5b84,0x007f5d85,0x00815f85,0x00826186,0x00836386,0x00846587,0x00856788,0x00866988,0x00886b89,0x00896d89,0x008a6f8a,0x008b718b,0x008b738c,0x008b768e,0x008b7990,0x008b7b91,0x008b7e93,0x008b8195,0x008b8396,0x008b8698,0x008b899a,0x008b8b9b,0x008b8e9d,0x008b919f,0x008b93a0,0x008b96a2,0x008b99a4,0x008b9ba5,0x008b9ea7,0x008ba1a9,0x008aa4ac,0x0088a8af,0x0087abb3,0x0085afb6,0x0084b2ba,0x0082b6bd,0x0080bac1,0x007fbdc4,0x007dc1c8,0x007cc4cb,0x007ac8cf,0x0078ccd2,0x0077cfd6,0x0075d3da,0x0074d7dd,0x0072dae1,0x0070dee4,0x006fe1e8,0x006fe1e8,0x0070dee4,0x0072dae1,0x0074d7dd,0x0075d3da,0x0077cfd6,0x0078ccd2,0x007ac8cf,0x007cc4cb,0x007dc1c8,0x007fbdc4,0x0080bac1,0x0082b6bd,0x0084b2ba,0x0085afb6,0x0087abb3,0x0088a8af,0x008aa4ac,0x008ba1a9,0x008b9ea7,0x008b9ba5,0x008b99a4,0x008b96a2,0x008b93a0,0x008b919f,0x008b8e9d,0x008b8b9b,0x008b899a,0x008b8698,0x008b8396,0x008b8195,0x008b7e93,0x008b7b91,0x008b7990,0x008b768e,0x008b738c,0x008b718b,0x008a6f8a,0x00896d89,0x00886b89,0x00866988,0x00856788,0x00846587,0x00836386,0x00826186,0x00815f85,0x007f5d85,0x007e5b84,0x007d5983,0x007c5783,0x007b5582,0x007a5482,0x00795281,0x00775080,0x00764e80,0x00744c7f,0x00734b7f,0x0071497f,0x006f487f,0x006d477e,0x006b457e,0x006a447e,0x0068427d,0x0066417d,0x0064407d,0x00623e7c,0x00603d7c,0x005f3b7c,0x005d3a7c,0x005b397b,0x0059377b,0x0057367b,0x0056347a,0x0054337a,0x00523279,0x00503178,0x004e3078,0x004c2f77,0x004a2e76,0x00482d75,0x00462c75,0x00442b74,0x00422a73,0x00402972,0x003e2871,0x003c2771,0x003a2670,0x0038256f,0x0036246e,0x0033236e,0x0031226d,0x0030216c,0x002e206a,0x002c1f68,0x002a1e66,0x00291d64,0x00271c62,0x00251b60,0x00241a5e,0x0022195c,0x0020195a,0x001e1858,0x001d1756,0x001b1654,0x00191552,0x00181450,0x0016134e,0x0014124c,0x0012114a,0x00111148,0x00101044,0x000f0f40,0x000e0e3c,0x000d0d38,0x000c0c34,0x000b0b30,0x000a0a2c,0x00090928,0x00080824,0x00080720,0x0007061c,0x00060618,0x00050514,0x00040410,0x0003030c,0x00020208,0x00010104,0x00000000
 HueColors:	   dd 0x00ff0000,0x00ff0c00,0x00ff1800,0x00ff2400,0x00ff3000,0x00ff3c00,0x00ff4800,0x00ff5400,0x00ff6000,0x00ff6c00,0x00ff7800,0x00ff8400,0x00ff9000,0x00ff9c00,0x00ffa800,0x00ffb400,0x00ffc000,0x00ffcc00,0x00ffd800,0x00ffe400,0x00fff000,0x00fffc00,0x00f6ff00,0x00eaff00,0x00deff00,0x00d2ff00,0x00c6ff00,0x00baff00,0x00aeff00,0x00a2ff00,0x0096ff00,0x008aff00,0x007eff00,0x0072ff00,0x0066ff00,0x005aff00,0x004eff00,0x0042ff00,0x0036ff00,0x002aff00,0x001eff00,0x0012ff00,0x0006ff00,0x0000ff06,0x0000ff12,0x0000ff1e,0x0000ff2a,0x0000ff36,0x0000ff42,0x0000ff4e,0x0000ff5a,0x0000ff66,0x0000ff72,0x0000ff7e,0x0000ff8a,0x0000ff96,0x0000ffa2,0x0000ffae,0x0000ffba,0x0000ffc6,0x0000ffd2,0x0000ffde,0x0000ffea,0x0000fff6,0x0000fcff,0x0000f0ff,0x0000e4ff,0x0000d8ff,0x0000ccff,0x0000c0ff,0x0000b4ff,0x0000a8ff,0x00009cff,0x000090ff,0x000084ff,0x000078ff,0x00006cff,0x000060ff,0x000054ff,0x000048ff,0x00003cff,0x000030ff,0x000024ff,0x000018ff,0x00000cff,0x000000ff,0x000c00ff,0x001800ff,0x002400ff,0x003000ff,0x003c00ff,0x004800ff,0x005400ff,0x006000ff,0x006c00ff,0x007800ff,0x008400ff,0x009000ff,0x009c00ff,0x00a800ff,0x00b400ff,0x00c000ff,0x00cc00ff,0x00d800ff,0x00e400ff,0x00f000ff,0x00fc00ff,0x00ff00f6,0x00ff00ea,0x00ff00de,0x00ff00d2,0x00ff00c6,0x00ff00ba,0x00ff00ae,0x00ff00a2,0x00ff0096,0x00ff008a,0x00ff007e,0x00ff0072,0x00ff0066,0x00ff005a,0x00ff004e,0x00ff0042,0x00ff0036,0x00ff002a,0x00ff001e,0x00ff0012,0x00ff0006,0x00ff0006,0x00ff0012,0x00ff001e,0x00ff002a,0x00ff0036,0x00ff0042,0x00ff004e,0x00ff005a,0x00ff0066,0x00ff0072,0x00ff007e,0x00ff008a,0x00ff0096,0x00ff00a2,0x00ff00ae,0x00ff00ba,0x00ff00c6,0x00ff00d2,0x00ff00de,0x00ff00ea,0x00ff00f6,0x00fc00ff,0x00f000ff,0x00e400ff,0x00d800ff,0x00cc00ff,0x00c000ff,0x00b400ff,0x00a800ff,0x009c00ff,0x009000ff,0x008400ff,0x007800ff,0x006c00ff,0x006000ff,0x005400ff,0x004800ff,0x003c00ff,0x003000ff,0x002400ff,0x001800ff,0x000c00ff,0x000000ff,0x00000cff,0x000018ff,0x000024ff,0x000030ff,0x00003cff,0x000048ff,0x000054ff,0x000060ff,0x00006cff,0x000078ff,0x000084ff,0x000090ff,0x00009cff,0x0000a8ff,0x0000b4ff,0x0000c0ff,0x0000ccff,0x0000d8ff,0x0000e4ff,0x0000f0ff,0x0000fcff,0x0000fff6,0x0000ffea,0x0000ffde,0x0000ffd2,0x0000ffc6,0x0000ffba,0x0000ffae,0x0000ffa2,0x0000ff96,0x0000ff8a,0x0000ff7e,0x0000ff72,0x0000ff66,0x0000ff5a,0x0000ff4e,0x0000ff42,0x0000ff36,0x0000ff2a,0x0000ff1e,0x0000ff12,0x0000ff06,0x0006ff00,0x0012ff00,0x001eff00,0x002aff00,0x0036ff00,0x0042ff00,0x004eff00,0x005aff00,0x0066ff00,0x0072ff00,0x007eff00,0x008aff00,0x0096ff00,0x00a2ff00,0x00aeff00,0x00baff00,0x00c6ff00,0x00d2ff00,0x00deff00,0x00eaff00,0x00f6ff00,0x00fffc00,0x00fff000,0x00ffe400,0x00ffd800,0x00ffcc00,0x00ffc000,0x00ffb400,0x00ffa800,0x00ff9c00,0x00ff9000,0x00ff8400,0x00ff7800,0x00ff6c00,0x00ff6000,0x00ff5400,0x00ff4800,0x00ff3c00,0x00ff3000,0x00ff2400,0x00ff1800,0x00ff0c00,0x00ff0000
 CoolColors:	   dd 0x00170013,0x00160116,0x00150219,0x0013031c,0x0012031f,0x00100422,0x000f0525,0x000d0628,0x000c072b,0x000a082e,0x00090931,0x00070934,0x00060a37,0x00040b3a,0x00030c3d,0x00010d40,0x00000e43,0x00000f46,0x00001149,0x0000124c,0x0000134f,0x00001552,0x00001655,0x00001758,0x0000195c,0x00001a5f,0x00001b62,0x00001d65,0x00001e68,0x0000206b,0x0000216e,0x00002271,0x00002474,0x00002577,0x0000277a,0x0000297d,0x00002b80,0x00002c83,0x00002e86,0x00003089,0x0000328c,0x0000338f,0x00003592,0x00003795,0x00003998,0x00003a9b,0x00003c9e,0x00003ea1,0x000040a4,0x000041a7,0x000043aa,0x000044ad,0x000046b0,0x000047b3,0x000049b5,0x00004ab8,0x00004cbb,0x00004ebe,0x00004fc1,0x000051c4,0x000052c7,0x000054ca,0x000055cd,0x000057d0,0x000058d3,0x00005ad6,0x00005cd9,0x00005edb,0x000060de,0x000062e1,0x000064e4,0x000066e6,0x000068e9,0x00006aec,0x00006cef,0x00006ef1,0x000070f4,0x000072f7,0x000074fa,0x000076fc,0x000078ff,0x00027bff,0x00057dff,0x000780ff,0x000a82ff,0x000c85ff,0x000e87ff,0x00118aff,0x00138cff,0x00168eff,0x001891ff,0x001a93ff,0x001d96ff,0x001f98ff,0x00219bff,0x00249dff,0x0026a0ff,0x002aa2ff,0x002da4ff,0x0031a6ff,0x0034a8ff,0x0037abff,0x003badff,0x003eafff,0x0042b1ff,0x0045b3ff,0x0049b6ff,0x004cb8ff,0x0050baff,0x0053bcff,0x0056beff,0x005ac1ff,0x005dc3ff,0x0063c5ff,0x0068c7ff,0x006dc9ff,0x0072ccff,0x0077ceff,0x007dd0ff,0x0082d2ff,0x0087d4ff,0x008cd7ff,0x0092d9ff,0x0097dbff,0x009cddff,0x00a1dfff,0x00a7e2ff,0x00ace4ff,0x00b1e6ff,0x00b6e8fe,0x00bbe9fc,0x00c0ebfb,0x00c4ecf9,0x00c9eef8,0x00ceeff6,0x00d3f1f5,0x00d8f3f3,0x00ddf4f2,0x00e2f6f0,0x00e7f7ef,0x00ebf9ed,0x00f0faec,0x00f5fcea,0x00fafde9,0x00ffffe7,0x00fffde3,0x00fffadf,0x00fff8db,0x00fff5d7,0x00fff3d3,0x00fff0cf,0x00ffeecb,0x00ffebc7,0x00ffe9c3,0x00ffe6c0,0x00ffe4bc,0x00ffe1b8,0x00ffdfb4,0x00ffdcb0,0x00ffdaac,0x00ffd7a8,0x00ffd5a3,0x00ffd39f,0x00ffd19a,0x00ffcf96,0x00ffcd91,0x00ffca8d,0x00ffc889,0x00ffc684,0x00ffc480,0x00ffc27b,0x00ffc077,0x00ffbe72,0x00ffbb6e,0x00ffb969,0x00ffb765,0x00ffb560,0x00feb35d,0x00fdb15a,0x00fcae57,0x00faac53,0x00f9aa50,0x00f8a74d,0x00f7a549,0x00f6a346,0x00f5a143,0x00f49e40,0x00f29c3c,0x00f19a39,0x00f09836,0x00ef9532,0x00ee932f,0x00ed912c,0x00e98e2a,0x00e58b28,0x00e18926,0x00dd8624,0x00da8322,0x00d68021,0x00d27d1f,0x00ce7b1d,0x00cb781b,0x00c77519,0x00c37217,0x00bf7015,0x00bb6d13,0x00b86a12,0x00b46710,0x00b0640e,0x00ac610d,0x00a85e0c,0x00a45a0b,0x00a0570a,0x009c5309,0x00985009,0x00944c08,0x00904907,0x008c4506,0x00884205,0x00843e04,0x00803b03,0x007c3703,0x00783402,0x00743001,0x00702d00,0x006c2a01,0x00672701,0x00622402,0x005e2202,0x00591f03,0x00541c03,0x00501904,0x004b1604,0x00471405,0x00421105,0x003d0e06,0x00390b07,0x00340807,0x002f0608,0x002b0308,0x00260009,0x00250009,0x0024000a,0x0023000b,0x0023000b,0x0022000c,0x0021000d,0x0020000d,0x001f000e,0x001e000f,0x001d000f,0x001c0010,0x001b0011,0x001a0011,0x00190012,0x00180013
		   dd 0x00160012,0x00170011,0x00190010,0x001b0010,0x001c000f,0x001e000e,0x001f000d,0x0021000d,0x0022000c,0x0024000b,0x0025000b,0x0027000a,0x00280009,0x002a0008,0x002b0008,0x002d0007,0x002e0006,0x00300006,0x00310006,0x00320005,0x00340005,0x00350005,0x00360005,0x00370004,0x00390004,0x003a0004,0x003b0004,0x003c0003,0x003e0003,0x003f0003,0x00400003,0x00420002,0x00430002,0x00440002,0x00460002,0x00480002,0x00490002,0x004b0001,0x004c0001,0x004e0001,0x00500001,0x00510001,0x00530001,0x00540001,0x00560001,0x00580000,0x00590000,0x005b0000,0x005c0000,0x005e0000,0x005f0000,0x00610000,0x00620000,0x00640000,0x00650000,0x00670000,0x00680000,0x006a0000,0x006b0000,0x006d0000,0x006e0000,0x00700000,0x00710000,0x00730000,0x00740000,0x00770200,0x007a0300,0x007d0500,0x00800600,0x00830800,0x00850900,0x00880b00,0x008b0c00,0x008e0e00,0x00911000,0x00941100,0x00971300,0x009a1400,0x009c1600,0x009f1700,0x00a21900,0x00a51c00,0x00a81f00,0x00ab2200,0x00ad2500,0x00b02800,0x00b32c00,0x00b62f00,0x00b93200,0x00bb3500,0x00be3800,0x00c13b00,0x00c43e00,0x00c74100,0x00c94400,0x00cc4700,0x00cf4a00,0x00d14f00,0x00d25301,0x00d45701,0x00d55b02,0x00d75f02,0x00d86303,0x00da6703,0x00db6b04,0x00dd6f04,0x00de7304,0x00e07705,0x00e17b05,0x00e37f06,0x00e48306,0x00e68707,0x00e78b07,0x00e98e08,0x00ea9008,0x00ec9309,0x00ed960a,0x00ef980a,0x00f09b0b,0x00f29e0c,0x00f3a00c,0x00f5a30d,0x00f6a50e,0x00f8a80f,0x00f9ab0f,0x00fbad10,0x00fcb011,0x00feb211,0x00ffb512,0x00ffb713,0x00ffb814,0x00ffba15,0x00ffbc16,0x00ffbd17,0x00ffbf19,0x00ffc01a,0x00ffc21b,0x00ffc41c,0x00ffc51d,0x00ffc71e,0x00ffc91f,0x00ffca20,0x00ffcc21,0x00ffcd23,0x00ffcf24,0x00ffd125,0x00ffd426,0x00ffd627,0x00ffd828,0x00ffda2a,0x00ffdd2b,0x00ffdf2c,0x00ffe12d,0x00ffe32e,0x00ffe62f,0x00ffe831,0x00ffea32,0x00ffec33,0x00ffef34,0x00fff135,0x00fff337,0x00fff439,0x00fff53c,0x00fff53e,0x00fff641,0x00fff743,0x00fff846,0x00fff848,0x00fff94b,0x00fffa4d,0x00fffb50,0x00fffb52,0x00fffc55,0x00fffd57,0x00fffe5a,0x00fffe5c,0x00ffff5f,0x00ffff60,0x00ffff62,0x00ffff64,0x00ffff65,0x00ffff67,0x00ffff68,0x00ffff6a,0x00ffff6c,0x00ffff6d,0x00ffff6f,0x00ffff70,0x00ffff72,0x00ffff74,0x00ffff75,0x00ffff77,0x00ffff78,0x00f9fb78,0x00f3f878,0x00eef478,0x00e8f178,0x00e2ed78,0x00dcea78,0x00d6e678,0x00d1e378,0x00cbdf78,0x00c5dc78,0x00bfd878,0x00b9d578,0x00b4d178,0x00aece78,0x00a8ca78,0x00a2c778,0x009cc278,0x0097bc78,0x0091b777,0x008bb277,0x0085ad77,0x0080a876,0x007aa276,0x00749d76,0x006f9875,0x00699375,0x00638d75,0x005d8875,0x00588374,0x00527e74,0x004c7874,0x00467373,0x00436f72,0x00406b70,0x003d676e,0x003a636d,0x00365f6b,0x00335a6a,0x00305668,0x002d5267,0x002a4e65,0x00274a63,0x00234662,0x00204260,0x001d3d5f,0x001a395d,0x0017355b,0x0013315a,0x00142e55,0x00142b51,0x0014284c,0x00142548,0x00142243,0x00141f3f,0x00141c3a,0x00151836,0x00151531,0x0015122d,0x00150f28,0x00150c24,0x0015091f,0x0016061b,0x00160316



align 4
  TextColor   dd 0x00FFFFFF
  Depth       dd 100
  RenderPath  dd 0


align 2

  f10Control   dw 0x037F
  f4Control    dw 0x007F

align 1
  FixedMessage db 'S: toggle stats   R: render path   T: reset   M: more depth   N: less depth   1-9: colors',0
  RenderPathsStrings db 'SSE (128bit)    '
		     db 'AVX (256bit)    '
		     db 'FMA4 (256bit)   '
		     db 'FMA3 (256bit)   '
		     db 'FPU             '


  _title TCHAR 'Shallow Mandelbrot Explorer',0
  _error TCHAR 'Startup failed.',0
  _class TCHAR 'Mandelbrot64',0





align 32
  xcoords	   rq 4096
  ycoords	   rq 4096

align 8
ColorTable rq 256+16



align 8
  hMainWindow	   dq ?
  hStatusWindow    dq ?
  hInstance	   dq ?
  hBitmap	   dq ?
  hglbCopy	   dq ?
  hMemDC	   dq ?
  hdc		   dq ?
  ppvBits	   dq ?
  ITERs 	   dq ?
  Frequency	   dq ?
  Time1 	   dq ?
  Time2 	   dq ?


  hThreadArray	      dq 64 dup ?
  ThreadMaskArray     dq 64 dup ?
  ProcessAffinityMask dq ?
  SystemAffinityMask  dq ?



align 4
  Bitmap.x	dd ?
  Bitmap.y	dd ?
  Mouse.x	dd ?
  Mouse.y	dd ?
  Mouse 	dd ?
  CoreCount	dd ?,?
  SupportedPaths dd ?

  msg		MSG
  rect		RECT
  ps		PAINTSTRUCT
  bmiheader	BITMAPINFOHEADER
  of		OPENFILENAME


align 1
  StatsQ	rb 1
  Message	rb 1024
  Temp		rb 1024

section '.idata' import data readable writeable

  library kernel32,'KERNEL32.DLL',\
	  user32,'USER32.DLL',\
	  gdi32,'GDI32.DLL'

include 'api/kernel32.inc'
include 'api/user32.inc'
include 'api/gdi32.inc'