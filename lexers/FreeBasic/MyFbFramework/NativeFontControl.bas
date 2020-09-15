﻿'################################################################################
'#  MonthCalendar.bi                                                            #
'#  This file is part of MyFBFramework                                          #
'#  Authors: Xusinboy Bekchanov (2018-2019)                                     #
'################################################################################

#Include Once "NativeFontControl.bi"

Namespace My.Sys.Forms
	#IfNDef __USE_GTK__
		Sub NativeFontControl.HandleIsAllocated(ByRef Sender As My.Sys.Forms.Control)
			If Sender.Child Then
				With QNativeFontControl(Sender.Child)
					
				End With
			End If
		End Sub
		
		Sub NativeFontControl.WndProc(ByRef Message As Message)
		End Sub
		
		Sub NativeFontControl.ProcessMessage(ByRef Message As Message)
			Base.ProcessMessage(Message)
		End Sub
	#EndIf
	
	Operator NativeFontControl.Cast As My.Sys.Forms.Control Ptr
		Return Cast(My.Sys.Forms.Control Ptr, @This)
	End Operator
	
	Constructor NativeFontControl
		With This
			WLet FClassName, "NativeFontControl"
			WLet FClassAncestor, "NativeFontCtl"
			#IfNDef __USE_GTK__
				.RegisterClass "NativeFontControl","NativeFontCtl"
				.Style        = WS_CHILD
				.ExStyle      = 0
				.ChildProc    = @WndProc
				.OnHandleIsAllocated = @HandleIsAllocated
			#EndIf
			.Width        = 175
			.Height       = 21
			.Child        = @This
		End With
	End Constructor
	
	Destructor NativeFontControl
		#IfNDef __USE_GTK__
			UnregisterClass "NativeFontControl",GetModuleHandle(NULL)
		#EndIf
	End Destructor
End Namespace
