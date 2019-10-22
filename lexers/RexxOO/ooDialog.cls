/******************************************************************************/
/*                                                                            */
/*               ooDialog.CLS - OODialog Class Definition File                */
/*               Windows Dialog Interface for Open Object REXX                */
/*                                                                            */
/******************************************************************************/

::requires "oodPlain.CLS"
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/*                                                                            */
/* Dialog class extensions                                                    */
/*                                                                            */
/******************************************************************************/

::class 'DialogExtensions' public MIXINCLASS object

   /* This method will do the same as Execute but it will do an asynchronous */
   /* dispatch of messages      */

::method ExecuteAsync unguarded
   use strict arg sleeptime = 10, sh = "NORMAL", icon = 0, nomodal = 0
   if \ sleeptime~Datatype('W') then sleeptime = 10

   if self~startit(icon, nomodal) \= 0 then
   do
      if self~UseStem = 1 then self~SetDataStem(self~DlgData); else self~SetData
      self~show(sh)
      self~start("AsyncMessageHandling", sleeptime)
      do while self~AutomaticMethods~Items > 0
         p = self~AutomaticMethods~pull
         m = self~AutomaticMethods~pull
         c = self~AutomaticMethods~pull
         o = c~new(p, self)
         o~start(m)
      end
      return 0
   end
   else self~stopit
   return 1

   /* This method ends the asynchronous execution of a dialog */

::method EndAsyncExecution unguarded
   do while self~finished = 0
      call msSleep 20
   end
   if self~InitCode = 1 then do
      if (self~UseStem = 1) then self~GetDataStem(self~DlgData)
      self~GetData
   end
   if self~Adm \= 0 then self~stopit
   return self~InitCode


   /* This method is started asynchronously to handle dialog messages */

::method AsyncMessageHandling unguarded protected
   use strict arg sleeptime = 10
   if \ sleepTime~datatype('W') then sleepTime = 10
   do while self~finished = 0 & self~Adm \= 0
      msg = GetDlgMsg(self~Adm)
      if msg~pos("1DLGDELETED1") > 0 then do
          if self~finished = 0 then self~finished = 2
      end
      else do
          if msg \= "" then interpret("self~"msg)
          else call msSleep sleeptime /* to yield cpu */
      end
   end

   /* Run a nonmodal dialog */
::method Popup
   use strict arg sh = "NORMAL", sleeptime = 10, icon = 0
   if \ sleeptime~Datatype('W') then sleeptime = 10
   /* start Execute and return to caller */
   self~start("Execute", sh, icon, sleeptime, "NOTMODAL")


::method PopupAsChild
   use strict arg parent, sh = "NORMAL", sleeptime = 10, icon = 0

   if \ parent~isA(.PlainBaseDialog) then do
       call errorDialog "No parent dialog specified!"
       return -1
   end
   if \ sleeptime~Datatype('W') then sleeptime = 10
   parent~ChildDialogs~insert(self)  /* add to the parents child list (last item) */
   self~ParentDlg = parent

   /* start Execute and return to caller */
   self~start("Execute", sh, icon, sleeptime, "NOTMODAL")


::method SendMessageToItem unguarded
   parse arg id,a2,a3,a4
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return SendWinMsg("DLG",self~DlgHandle,id,a2,a3,a4)


 /* The following Methods are to set/get the behaviour of/from a scrollbar */
 /* You can use scrollbars to connect them with numerical entry fields */

::method SetSBRange unguarded
   use arg id, min, max, redraw
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if Arg(4, "o") = 1 then redraw = 1
   return HandleScrollBar("SR", self~GetItem(id),min,max, redraw)

::method GetSBRange unguarded
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return HandleScrollBar("GR",self~GetItem(id))

::method SetSBPos unguarded
   use arg id, pos, redraw
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if Arg(3, "o") = 1 then redraw = 1
   return HandleScrollBar("SP",self~GetItem(id),pos, redraw)

::method GetSBPos unguarded
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return HandleScrollBar("GP",self~GetItem(id))

::method ConnectScrollBar
   use arg id, progup, progdn, progpos, min, max, pos, progpgup, progpgdn,,
           progtop, progbottom, progtrack, progendsc
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if ((Arg(5, 'o') = 0) & (min~Datatype('N') \= 0)) & ((Arg(6,'o') = 0) & (max~Datatype('N') \= 0)) then self~SetSBRange(id, min, max, 0)
   if (Arg(7,'o') = 0) & (pos~Datatype('N') \= 0) then self~SetSBPos(id, pos, 0)

   hnd = self~GetItem(id)
   wf = "0x0000FFFF"
   nf = "0xFFFFFFFF"
   /* Install hooks for WM_VSCROLL and WM_HSCROLL */
   self~AddUserMsg(progdn, 0x00000114, wf, 0x00000001, wf, hnd, nf)
   self~AddUserMsg(progup, 0x00000114, wf, 0x00000000, wf, hnd, nf)
   self~AddUserMsg(progdn, 0x00000115, wf, 0x00000001, wf, hnd, nf)
   self~AddUserMsg(progup, 0x00000115, wf, 0x00000000, wf, hnd, nf)
   if Arg(4,'o') = 0 then do
      self~AddUserMsg(progpos, 0x00000114, wf, 0x00000004, wf, hnd, nf)
      self~AddUserMsg(progpos, 0x00000115, wf, 0x00000004, wf, hnd, nf)
   end
   if Arg(8,'o') = 0 then do
      self~AddUserMsg(progpgup, 0x00000114, wf, 0x00000002, wf, hnd, nf)
      self~AddUserMsg(progpgup, 0x00000115, wf, 0x00000002, wf, hnd, nf)
   end
   if Arg(9,'o') = 0 then do
      self~AddUserMsg(progpgdn, 0x00000114, wf, 0x00000003, wf, hnd, nf)
      self~AddUserMsg(progpgdn, 0x00000115, wf, 0x00000003, wf, hnd, nf)
   end
   if Arg(10,'o') = 0 then do
      self~AddUserMsg(progtop, 0x00000114, wf, 0x00000006, wf, hnd, nf)
      self~AddUserMsg(progtop, 0x00000115, wf, 0x00000006, wf, hnd, nf)
   end
   if Arg(11,'o') = 0 then do
      self~AddUserMsg(progbottom, 0x00000114, wf, 0x00000007, wf, hnd, nf)
      self~AddUserMsg(progbottom, 0x00000115, wf, 0x00000007, wf, hnd, nf)
   end
   if Arg(12,'o') = 0 then do
      self~AddUserMsg(progtrack, 0x00000114, wf, 0x00000005, wf, hnd, nf)
      self~AddUserMsg(progtrack, 0x00000115, wf, 0x00000005, wf, hnd, nf)
   end
   if Arg(13,'o') = 0 then do
      self~AddUserMsg(progendsc, 0x00000114, wf, 0x00000008, wf, hnd, nf)
      self~AddUserMsg(progendsc, 0x00000115, wf, 0x00000008, wf, hnd, nf)
   end
   return 0


::method CombineELwithSB
   use arg elid, sbid, step, poscode

   pos = self~GetEntryLine(elid)
   parse value self~GetSBRange(sbid) with min max
   if pos~DataType("N") = 0 then return

   if Arg(4,"o") = 0 then do
       sbc = .DlgUtil~loWord(poscode)
       if sbc = 0 & step > 0 then step = -step
       else if sbc = 1 & step < 0 then step = +step
   end

   if (step > 0) then do
      if pos < max then pos = pos + step
   end
   else if (step < 0) then do
      if pos > min then pos = pos + step
   end
   else if Arg(4,'o') = 0 then do
      /* calc the position returned by message */
      pos = .DlgUtil~hiWord(poscode)
   end
   self~SetSBPos(sbid, pos)
   self~SetEntryLine(elid,pos)
   return 0


::method ConnectAllSBEvents
   use arg id, prog, min, max, pos
   return self~ConnectScrollBar(id, prog, prog, prog, min, max, pos, prog, prog, prog, prog)

::method DetermineSBPosition unguarded
   use arg id, posdata, single, page
   if Arg(3,"o") = 1 then single = 1
   if Arg(4,"o") = 1 then page = 10
   code = .DlgUtil~loWord(posdata)
   parse value self~GetSBRange(id) with rmin rmax
   pos = self~GetSBPos(id)
   select
      /* Line up */
      when code = 0 then pos = max(rmin,pos - single)
      /* Line down */
      when code = 1 then pos = min(rmax,pos + single)
      /* page up */
      when code = 2 then pos = max(rmin,pos - page)
      /* page down */
      when code = 3 then pos = min(rmax,pos + page)
      /* track position */
      when code = 4 then pos = .DlgUtil~hiWord(posdata)
      /* tracking */
      when code = 5 then pos = .DlgUtil~hiWord(posdata)
      /* top */
      when code = 6 then pos = rmin
      /* bottom */
      when code = 7 then pos = rmax
      otherwise nop;
   end
   self~SetSBPos(id, pos)
   return pos


/* set/get the horizontal scroll width of a list box */

::method SetListWidth unguarded
   use arg id, dlgunits
   if Arg(2,'o') = 1 | dlgunits~datatype("N") = 0 then return -1
   return self~BoxMessage(id, 0x00000194, dlgunits*self~FactorX, 0)

::method GetListWidth unguarded
   use arg id
   return self~BoxMessage(id, 0x00000193, 0, 0)/self~FactorX

::method SetListColumnWidth unguarded
   use arg id, dlgunits
   if Arg(2,'o') = 1 | dlgunits~datatype("N") = 0 then return -1
   return self~BoxMessage(id, 0x00000195, dlgunits*self~FactorX, 0)

::method SetListItemHeight unguarded
   use arg id, dlgunits
   if Arg(2,'o') = 1 | dlgunits~datatype("N") = 0 then return -1
   return self~BoxMessage(id, 0x000001A0, 0, dlgunits*self~FactorY)

::method GetListItemHeight unguarded
   use arg id
   return self~BoxMessage(id, 0x000001A1, 0, 0)/self~FactorY

::method SetItemFont unguarded
   use arg id, hFont, bRedraw     /* WM_SETFONT */
   if Arg(3, 'o') = 1 then bRedraw = 1
   return SendWinMsg("ANY", self~GetItem(id), "0x0030", hFont, bRedraw)

::method HideWindowFast unguarded
   use arg hwnd
   return WndShow_Pos("S", hwnd, "HIDE FAST")

::method ShowWindowFast unguarded
   use arg hwnd
   return WndShow_Pos("S", hwnd, "NORMAL FAST", self~Adm)

::method HideItemFast unguarded
   use arg id
   h = self~GetItem(id)
   if h = 0 then return -1
   return WndShow_Pos("S", h, "HIDE FAST")

::method ShowItemFast unguarded
   use arg id
   h = self~GetItem(id)
   if h = 0 then return -1
   return WndShow_Pos("S",h, "NORMAL FAST")

   /* This method returns the size and position of the given button */
::method GetButtonRect unguarded
   use arg id
   ret = self~GetItem(id)
   return WindowRect("GET", ret)

   /* Returns the size and position of the given window */

::method GetWindowRect unguarded
   use arg hwnd
   return WindowRect("GET",hwnd)

   /* This method will resize a dialog item */

::method ResizeItem unguarded
   use arg id, width, high, showOptions
   if Arg(4,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   hnd = self~GetItem(id)
   if hnd = 0 then return -1
   return WndShow_Pos("P",hnd, 0, 0, width * self~FactorX, high* self~FactorY,,
                           "NOMOVE "||showOptions)

   /* This method will move a dialog item to an other position */

::method MoveItem unguarded
   use arg id, xPos, yPos, showOptions
   if Arg(4,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   h = self~GetItem(id)
   if h = 0 then return -1
   return WndShow_Pos("P",h, xPos * self~FactorX, yPos * self~FactorY, 0, 0,,
                           "NOSIZE "||showOptions)

   /* This method will add a method that will be started automatically */
   /* and run concurrently when the dialog is executed. That is the   */
   /* method 'InClass'~'MethodName' will be started, when the dialog receives */
   /* the message 'execute' or execute async. You can use this method to */
   /* animate buttons */

::method AddAutoStartMethod
   use arg InClass, MethodName, Parameters
   if Arg(1,"o") = 0 then self~AutomaticMethods~push(InClass); else self~AutomaticMethods~push(self)
   if Arg(2,"o") = 0 then self~AutomaticMethods~push(MethodName)
   if Arg(3,"o") = 0 then self~AutomaticMethods~push(Parameters)



/******************************************************************************/
/* Bitmap extensions */


::method BkgBitmap attribute protected  /* handle to bitmap that will be displayed in the background */
::method BkgBrushBmp attribute protected /* handle to brushbmp that will be displayed in the background */

   /* This method loads a bitmap from file into memory */
   /* It returns a handle to the bitmap */

::method LoadBitmap
   use arg bmpFilename, option
   if Arg(2, 'o') = 1 then option = ""; else option = option~translate
   return LoadRemoveBitmap(self~Adm, bmpFilename, "LOAD" || option)

   /* This method will free a memory bitmap */

::method RemoveBitmap
   use arg hBitmap
   return LoadRemoveBitmap(self~Adm, hBitmap, "REMOVE")

   /* This method connects a bitmap and a class method with a push button */
   /* The given bitmap will be shown instead of a push button */

::method AddBitmapButton
   use arg id, x, y, cx, cy, text, fnk, bmp, focus, sel, disabl, opts
   if self~activePtr = 0 then return -2
   id = self~CheckId(id)
   if  id = -1 then return
   if Arg(4, 'o') = 1 then cx = 0
   if Arg(5, 'o') = 1 then cy = 0
   if Arg(6, 'o') = 1 then text = ""
   if Arg(7, 'o') = 1 then fnk = ""
   if Arg(9, 'o') = 1 then focus = ""
   if Arg(10, 'o') = 1 then sel = ""
   if Arg(11, 'o') = 1 then disabl = ""
   opts = self~GetDefaultOpts(opts) || " OWNER"
   if focus = "" & sel = "" & opts~wordpos("FRAME") = 0 & opts~wordpos("TAB") = 0 then opts = opts || " NOTAB"

   self~DialogItemCount = self~DialogItemCount + 1
   self~activePtr = UsrAddControl("BUT", self~activePtr, id, x, y, cx, cy, text, opts)
   self~ConnectBitmapButton(id, fnk~space(0), bmp, focus, sel, disabl, opts)


::method ConnectBitmapButton
   use arg id, msgToRise, bmpNormal, bmpFocused, bmpSelected, bmpDisabled, styleOptions
   if Arg(2,'o') = 1 then msgToRise = ""
   if Arg(4,'o') = 1 then bmpFocused = ""
   if Arg(5,'o') = 1 then bmpSelected = ""
   if Arg(6,'o') = 1 then bmpDisabled = ""
   if Arg(7,'o') = 1 then styleOptions = ""; else styleOptions = styleOptions~translate
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return BmpButton(self~Adm, "A", msgToRise, id, bmpNormal, bmpFocused, bmpSelected, bmpDisabled, styleOptions);


   /* This method changes the bitmap of a bitmap button */
   /* possible options are: (see ConnectBitmapButton) */

::method ChangeBitmapButton unguarded
   use arg id, bmpNormal, bmpFocused, bmpSelected, bmpDisabled, styleOptions
   if Arg(3,'o') = 1 then bmpFocused = ""
   if Arg(4,'o') = 1 then bmpSelected = ""
   if Arg(5,'o') = 1 then bmpDisabled = ""
   if Arg(6,'o') = 1 then styleOptions = ""; else styleOptions = styleOptions~translate
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return BmpButton(self~Adm, "C", id, bmpNormal, bmpFocused, bmpSelected, bmpDisabled, styleOptions);

   /* Sets the position of a bitmap within a button */

::method DisplaceBitmap unguarded
   use arg id, x, y
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return BmpButton(self~Adm, "S", id, x, y)

   /* Gets the position of a bitmap within a button */

::method GetBmpDisplacement unguarded
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return BmpButton(self~Adm, "G", id)

   /* This method will set a bitmap as the background brush. */
   /* (seems to work only on NT) */

::method TiledBackgroundBitmap
   use arg bmpFilename
   if self~BkgBrushBmp \= 0 then self~DeleteObject(self~BkgBrushBmp)
   self~BkgBrushBmp = self~CreateBrush(,bmpFilename)
   ret = SetBackground(self~Adm, "BRU", self~BkgBrushBmp)

::method BackgroundColor
   use arg color
   if self~BkgBrushBmp \= 0 then self~DeleteObject(self~BkgBrushBmp)
   self~BkgBrushBmp = self~CreateBrush(color)
   ret = SetBackground(self~Adm, "BRU", self~BkgBrushBmp)

   /* This method sets a bitmap as the background brush. */
   /* (seems to work only on NT) */
   /* argument option can be: */
   /*    "USEPAL" : This will use the color palette of the bitmap */

::method BackgroundBitmap
   use arg bmpFilename, option
   if Arg(2, 'o') = 1 then option = ""; else option = option~translate
   self~BkgBitmap = self~LoadBitmap(bmpFilename, option)
   ret = SetBackground(self~Adm, "BMP", self~BkgBitmap)


::method CreateBrush unguarded
    use arg color, brushSpecifier
    if Arg() = 0 then return HandleDC_Obj("B")
    if Arg(1, "o") = 1 then color = 1
    if Arg(2, "o") = 1 then return HandleDC_Obj("B", color)
    brushSpecifier = brushSpecifier~translate
    if brushSpecifier = "UPDIAGONAL" | brushSpecifier = "DOWNDIAGONAL" | brushSpecifier = "CROSS" | brushSpecifier = "DIAGCROSS",
    | brushSpecifier = "HORIZONTAL" | brushSpecifier = "VERTICAL" then return HandleDC_Obj("B", color, brushSpecifier)
    else return HandleDC_Obj("B", self~Adm, color, brushSpecifier)  /* brushSpecifier must be filename or resource ID */


::method SetItemSysColor
   use arg id, colorbk, colorfg
   if \ id~DataType("W") then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if Arg(3,'o') = 1 then
       return SetBackground(self~Adm, "COL", id, colorbk, "", "SYS")
   else
       return SetBackground(self~Adm, "COL", id, colorbk, colorfg, "SYS")

::method SetItemColor
   use arg id, colorbk, colorfg
   if \ id~DataType("W") then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if Arg(3,'o') = 1 then
       return SetBackground(self~Adm, "COL", id, colorbk)
   else
       return SetBackground(self~Adm, "COL", id, colorbk, colorfg)


   /* This method scrolls 'text' in the button */

::method ScrollInButton unguarded
   use arg id, text, fontName, fontSize, fontStyle, displaceY, step, sleep, color
   hnd = self~GetItem(id)
   if Arg() = 1 then
      return self~ScrollText(hnd)
   else
      return self~ScrollText(hnd, text, fontName, fontSize, fontStyle, displaceY, step, sleep, color)


   /* This method will scroll text in a window with the given size, font, */
   /* color. Text will be scrolled from right to left. */
   /* Call it twice and the scrolling will stop */
   /* See method WriteToWindow for valid fontStyles */

::method ScrollText unguarded
   use arg hwnd, text, fontName, fontSize, fontStyle,,
           displaceY, step, sleep, color

   if Arg(3, "o") = 1 | fontName = "FONTNAME" then fontName = "System"
   if Arg(4, "o") = 1 | fontSize = "FONTSIZE" then fontSize = 10
   if Arg(5, "o") = 1 | fontStyle = "FONTSTYLE" then fontStyle = ""
   if Arg(6, "o") = 1 | displaceY = "DISPLACEY" then displaceY = 0
   if Arg(7, "o") = 1 | step = "STEP" then step = 4
   if Arg(8, "o") = 1 | sleep = "SLEEP" then sleep = 10
   if Arg(9, "o") = 1 | color = "COLOR" then color = 0

   if self~ScrollNow = 0 & Arg() > 1 then
   do
      self~ScrollNow = 1
      ret = ScrollText(self~Adm, hwnd, text, fontName, fontSize, fontStyle,,
                       displaceY, step, sleep, color)
   end; else do
      -- Send the interrupt scroll message, 0x0a02 == WM_USER_INTERRUPTSCROLL
      ret = SendWinMsg("ANY",self~DlgHandle, "0x0a02", hwnd, 0)
   end
   self~ScrollNow = 0
   return ret




/******************************************************************************/
/* Graphics and drawing extensions */

   /* Connect WM_DRAWITEM with a method */

::method ConnectDraw
  use arg id, msgToRise
  if Arg(1,'o') = 1 then do
      if arg(2,'o') = 1 then msgToRise = "OnDraw"
      return self~AddUserMsg(msgToRise, 0x0000002B, 0xFFFFFFFF, 0, 0, 0, 0)
  end
  else do
     if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
     if id = -1 then return -1
     return self~AddUserMsg(msgToRise, 0x0000002B, 0xFFFFFFFF, '0x0000'||id~d2x(4), 0xFFFFFFFF, 0, 0)
  end

   /* This method draws the given button */

::method DrawButton unguarded
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return WindowRect("BTN", self~Adm, id)

   /* This method erases the draw area of the given button */

::method ClearButtonRect unguarded
   use arg id
   hwnd = self~GetItem(id)
   return WindowRect("CLR", self~Adm, hwnd)

   /* This method erases the draw area of the given window */

::method ClearWindowRect unguarded
   use arg hwnd
   return WindowRect("CLR", self~Adm, hwnd)

   /* Clears the whole dialog  */
::method Clear unguarded
   parse value WindowRect("GET", self~Hwnd) with r.1 r.2 r.3 r.4
   return WindowRect("CLR", self~Adm, self~Hwnd,r.1,r.2,r.3,r.4)

   /* Clears the given rectangle left, top, right, bottom of the given window */

::method ClearRect unguarded
   use arg hwnd, left, top, right, bottom
   return WindowRect("CLR", self~Adm, hwnd,left,top,right,bottom)

   /* This method redraws the given button */

::method RedrawButton unguarded
   use arg id, erasebkg
   hnd = self~GetItem(id)
   if Arg(2, 'o') = 1 then erasebkg = 0
   return WindowRect("RDW", hnd, erasebkg)

   /* This method redraws the rectangle of the given window */

::method RedrawWindowRect unguarded
   use arg hwnd, erasebkg
   if Arg(1, 'o') = 1 then hwnd = self~hwnd
   if Arg(2, 'o') = 1 then erasebkg = 0
   return WindowRect("RDW", hwnd, erasebkg)


   /* This method allows you to write text to a window in the given font, in */
   /* the given size, and to the given position */
   /* argument 'fontStyle' can be one or more off: */
   /*    "THIN"            "UNDERLINE"    */
   /*    "EXTRALIGHT"      "ITALIC"       */
   /*    "LIGHT"           "STRIKEOUT"    */
   /*    "MEDIUM"                */
   /*    "SEMIBOLD"              */
   /*    "EXTRABOLD"             */
   /*    "BOLD"                  */
   /*    "HEAVY"                 */

::method WriteToWindow unguarded
   use arg hwnd, xPos, yPos, text, fontName, fontSize, fontStyle, fgColor, bkColor
   if Arg(5, "o") = 1 then fontName = "System"
   if Arg(6, "o") = 1 then fontSize = 10
   if Arg(7, "o") = 1 then fontStyle = ""; else fontStyle = fontStyle~translate
   if Arg(8, "o") = 1 then fgColor = -1
   if Arg(9, "o") = 1 then bkColor = -1
   return WriteText(hwnd, xPos, yPos, text, fontName, fontSize, fontStyle, fgColor, bkColor)  /* more than 6 arguments means use font */


   /* This method allows you to write text to a button in the given font in */
   /* the given size to the given position. */
   /* See method WriteToWindow for valid fontStyles */

::method WriteToButton unguarded
   argarr = Arg(1, "A")
   argarr[1] = self~GetItem(argarr[1])  /* transform first argument into window handle */
   forward message (WriteToWindow) arguments (argarr)


   /* This method moves the rectangle within a button */

::method ScrollButton unguarded
   use arg id, xPos, yPos, left, top, right, bottom
   return ScrollTheWindow(self~Adm, self~GetItem(id), xPos, yPos, left, top, right, bottom, "yes")


   /* This method will return the device context of a window */

::method GetWindowDC unguarded
   use arg hwnd
   return HandleDC_Obj("G", hwnd)

   /* This method will return the device context of a button */

::method GetButtonDC unguarded
   use arg id
   return self~GetWindowDC(self~GetItem(id))

   /* This method will free the device context of a window */

::method FreeWindowDC unguarded
   use arg hwnd, dc
   return HandleDC_Obj("F", hwnd, dc)

   /* This method will free the device context of a button */

::method FreeButtonDC unguarded
   use arg id, dc
   hnd = self~GetItem(id)
   return self~FreeWindowDC(hnd, dc)

   /* This method will return the horizontal bitmap extension */

::method GetBitmapSizeX unguarded
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   parse value BmpButton(self~Adm, "E", id) with x .
   return x

   /* This method will return the vertical bitmap extension */

::method GetBitmapSizeY unguarded
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   parse value BmpButton(self~Adm, "E", id) with . y
   return y

   /* This method will draw the bitmap of a button */
   /* Use this method to move a bitmap or a part of it... */

::method DrawBitmap unguarded
   use arg hwnd, id, px, py, srcx, srcy, xlen, ylen
   if Arg(3, "o") = 1 then px = 0
   if Arg(4, "o") = 1 then py = 0
   if Arg(5, "o") = 1 then srcx = 0
   if Arg(6, "o") = 1 then srcy = 0
   if Arg(7, "o") = 1 then xlen = 0
   if Arg(8, "o") = 1 then ylen = 0
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if Arg(1, "o") = 1 then hwnd = self~GetItem(id)
   return BmpButton(self~Adm, "D", hwnd, id, px, py, srcx, srcy, xlen, ylen)


   /* This method will draw the bitmap step by step */

::method DimBitmap unguarded
   use arg id, bmp, cx, cy, stepx, stepy, steps

   if Arg(5, "o") = 1 then steps = 10
   if Arg(6, "o") = 1 then stepx = 2
   if Arg(7, "o") = 1 then stepy = 2

   dc = self~GetButtonDC(id)
   br = HandleDC_Obj("B",self~Adm, 0, bmp)
   pen = HandleDC_Obj("P",0, "NULL", 0)
   obr = self~ObjectToDC(dc, br)
   open = self~ObjectToDC(dc, pen)

   ret = DCDraw("DIM", dc, cx, cy, stepx, stepy, steps)

   self~ObjectToDC(dc, obr)
   self~ObjectToDC(dc, open)
   self~DeleteObject(pen)
   self~FreeButtonDC(id, dc)

   /* This method will scroll a bitmap from one position to an other */
   /* within a button */

::method ScrollBitmapFromTo unguarded
   use arg id, fromX, fromY, toX, toY, stepX, stepY, delay, displace

   if Arg(8, "o") = 1 then delay = 0
   if Arg(9, "o") = 1 then displace = 1
   xl = self~GetBitmapSizeX(id)
   yl = self~GetBitmapSizeY(id)

   if xl <= 0 | yl <= 0 then return 1
   if Arg(6, "o") = 1 then stepx = 0
   if Arg(7, "o") = 1 then stepy = 0

   if stepx = 0 & stepy = 0 then return 1

   curx=fromx
   cury=fromy
   if (stepx > 0) then cursx = -curx; else cursx = 0
   if (stepy > 0) then cursy = -cury; else cursy = 0
   if (cursx > xl) then cursx = xl
   if (cursy > yl) then cursy = yl

   hwnd = self~GetItem(id)
   if hwnd = 0 then return -1

   parse value self~GetWindowRect(hwnd) with r.left r.top r.right r.bottom
   r.right = r.right - r.left
   r.bottom = r.bottom - r.top
   drawx = 1
   drawy = 1

   if (stepx >= 0) then
   do
      left = max(fromx, 0)
      lenx = xl - cursx
   end
   else do
      left = fromx
      lenx = min(r.right-fromx, xl)
   end
   if (stepy >= 0) then
   do
      top = max(fromy, 0)
      leny = yl - cursy
   end
   else do
      top = fromy
      leny = min(r.bottom-fromy, yl)
   end

   self~DrawBitmap(hwnd, id, left, top, cursx, cursy, lenx, leny)
   if (stepx < 0) then cursx = min(lenx, xl); else cursx = cursx-stepx;
   if (stepy < 0) then cursy = min(leny, yl); else cursy = cursy-stepy;

   worktodo = 1
   if (delay > 0) then timer = WinTimer("START", delay); else timer = 0
   do while worktodo = 1

      sleft = curx
      srgt = curx+xl
      if srgt > r.right then srgt = r.right
      if sleft < 0 then sleft = 0

      s_top = cury
      sbot = cury+yl
      if sbot > r.bottom-stepy then sbot = r.bottom-stepy
      if s_top < 0 then s_top = 0

      ret = ScrollTheWindow(self~Adm, hwnd, stepx, stepy, sleft, s_top, srgt, sbot, "yes")
      if ret = 1 then do
         if (timer \== 0) then ret = WinTimer("STOP", timer)
         return 1
      end

      if (stepx < 0) then left = r.right + stepx; else left = curx
      rgt = curx+xl

      if rgt > r.right then rgt = r.right
      if left < 0 then left = 0

      if (stepy < 0) then top = r.bottom+stepy; else top = cury
      bot = cury+yl
      if bot > r.bottom then bot = r.bot
      if top < 0 then top = 0

      if stepx \= 0 & drawx=1 then self~DrawBitmap(hwnd, id, left, top, cursx, cursy, abs(stepx), yl)
      if stepy \= 0 & drawy=1 then self~DrawBitmap(hwnd, id, left, top, cursx, cursy, xl, abs(stepy))
      if (timer \== 0) then ret = WinTimer("WAIT", timer)

      drawx = 0
      if (stepx <= 0) then do
         if cursx < xl then do
            cursx = cursx - stepx
            if cursx < xl then drawx = 1
         end
      end
      else do
         if cursx > 0 then do
            cursx = cursx - stepx
            if cursx > 0 then drawx = 1
         end
      end
      curx = curx + stepx

      drawy = 0
      if (stepy <= 0) then do
         if cursy < yl then do
            cursy = cursy - stepy
            if cursy < yl then drawy = 1
         end
      end
      else do
         if cursy > 0 then do
            cursy = cursy - stepy
            if cursy > 0 then drawy = 1
         end
      end
      cury = cury + stepy

                 /* so repaint will draw the cursor at the current position */
      if displace=1 then self~DisplaceBitmap(id, curx, cury);

      worktodo = 0
      if (stepx>0) & (curx+stepx <= tox) then worktodo = 1
      if (stepy>0) & (cury+stepy <= toy) then worktodo = 1
      if (stepx<0) & (curx+stepx >= tox) then worktodo = 1
      if (stepy<0) & (cury+stepy >= toy) then worktodo = 1
   end
   if (timer \== 0) then ret = WinTimer("STOP", timer)
   self~DisplaceBitmap(id, tox, toy);
   self~RedrawButton(id,0)
   return 0

   /* This method will install an animated button and run it concurrently to main */
   /* activity */

::method ConnectAnimatedButton
   use arg buttonid, msgToRise, AutoClass, bmpfrom, bmpto, movex, movey, sizex, sizey, delay, xnow, ynow

   if Arg(2, "o") = 1 then msgToRise = ""
   if Arg(3, "o") = 1 then autoclass = .AnimatedButton
   if Arg(5, "o") = 1 | bmpfrom~datatype("N") \= 1 then
       self~ConnectBitmapButton(buttonid, msgToRise, .local[bmpfrom][1])
   else self~ConnectBitmapButton(buttonid, msgToRise, bmpfrom)

   if Arg(5, "o") = 1 then bmpto = 0
   if Arg(8, "o") = 1 then sizex = self~GetBitmapSizeX(buttonid)
   if Arg(9, "o") = 1 then sizey = self~GetBitmapSizeY(buttonid)
   if Arg(11, "o") = 1 then xnow = 0
   if Arg(12, "o") = 1 then ynow = 0

   self~AddAutoStartMethod(AutoClass, "RUN", buttonid || "," || bmpfrom ||"," || bmpto ||"," || movex || ",",
     || movey || "," || sizex || "," || sizey || "," || delay || "," || xnow || "," || ynow)



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
  The menu methods in this block are purposively not documented.  Do NOT use
  them in your code. The WILL be removed from ooDialog in the future.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::attribute menuBar get
::attribute menuBar set private

::method linkMenu
  use strict arg menu

  if \ menu~isA(.MenuBar) then return -1

  oldMenu = self~menuBar
  self~menuBar = menu
  return oldMenu

::method unlinkMenu
  menu = self~menuBar
  self~menuBar = .nil
  return menu
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/


::method enableMenuItem unguarded
    use strict arg id
    if self~menuBar == .nil then return -2
    return self~menuBar~enable(id)

::method disableMenuItem unguarded
    use strict arg id
    if self~menuBar == .nil then return -2
    return self~menuBar~disable(id)

::method checkMenuItem unguarded
    use strict arg id
    if self~menuBar == .nil then return -2
    return self~menuBar~check(id)

::method uncheckMenuItem unguarded
    use strict arg id
    if self~menuBar == .nil then return -2
    return self~menuBar~uncheck(id)

::method grayMenuItem unguarded
    use strict arg id
    if self~menuBar == .nil then return -2
    return self~menuBar~gray(id)

::method setMenuItemRadio unguarded
    use strict arg idstart, idend, idset
    if self~menuBar == .nil then return -2
    return self~menuBar~checkRadio(idstart, idend, idset)

::method getMenuItemState unguarded
    use strict arg id
    if self~menuBar == .nil then return -2
    return self~menuBar~getState(id)


/******************************************************************************/
/* New extensions */

::method ConnectResize
   use arg msgToRise
   self~AddUserMsg(msgToRise, 0x00000005, 0xFFFFFFFF, 0, 0, 0, 0)

::method ConnectMove
   use arg msgToRise
   self~AddUserMsg(msgToRise, 0x00000003, 0xFFFFFFFF, 0, 0, 0, 0)

::method ConnectPosChanged
   use arg msgToRise
   self~AddUserMsg(msgToRise, 0x00000047, 0xFFFFFFFF, 0, 0, 0, 0)

::method ConnectMouseCapture
   use arg msgToRise
   self~AddUserMsg(msgToRise, 0x00000215, 0xFFFFFFFF, 0, 0, 0, 0)

::method connectMenuItem
   use strict arg id, msgToRaise

   id = self~resolveResourceId(id)
   if id = -1 then return -1

   if id < 3 | id = 9 then return 0

   return self~addUserMsg(msgToRaise, 0x00000111, 0xFFFFFFFF, id, 0x0000FFFF, 0, 0)

-- Connect WM_CONTEXTMENU messages to a method.
::method connectContextMenu
   use strict arg msgToRaise, hwnd = .nil
   if hwnd \== .nil then return self~addUserMsg(msgToRaise, 0x0000007B, 0xFFFFFFFF, hwnd, 0xFFFFFFFF, 0, 0, 0x00000201)
   return self~addUserMsg(msgToRaise, 0x0000007B, 0xFFFFFFFF, 0, 0, 0, 0, 0x00000201)

::method PeekDialogMessage unguarded
   return GetDlgMsg(self~Adm, "Peek")

::method SetWindowRect unguarded
   use arg hnd, x, y, width, high, showOptions
   if Arg(6,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   return WndShow_Pos("P",hnd, x, y, width, high, showOptions)

   /* Redraws the given rectangle */
::method RedrawRect unguarded
   use arg hwnd, left, top, right, bottom, erasebkg
   if Arg(1,'o') = 1 then hwnd = self~hwnd
   if Arg(6, 'o') = 1 then erasebkg = 0
   return WindowRect("RDW", hwnd,left,top,right,bottom, erasebkg)

::method RedrawWindow unguarded
   use arg hnd
   return WndShow_Pos("S", hnd, "REDRAW")

::method GetFocus unguarded
   return Wnd_Desktop("GETFOC", self~DlgHandle)

::method SetFocus unguarded
   use arg hwnd
   return Wnd_Desktop("SETFOC", self~DlgHandle, hwnd)

::method SetFocusToWindow unguarded
   use arg hwnd
   return Wnd_Desktop("SETFOC", self~DlgHandle, hwnd, 'F')

::method GetMouseCapture
   return Wnd_Desktop("CAP", self~DlgHandle, "G")

::method CaptureMouse
   return Wnd_Desktop("CAP", self~DlgHandle, self~DlgHandle)

::method ReleaseMouseCapture
   return Wnd_Desktop("CAP", self~DlgHandle, "R")

::method IsMouseButtonDown
   use arg mb
   if Arg(1,'o') = 1 then mb = "LEFT"
   else mb = mb~translate~space(0)
   if mb = "LEFT" then return Wnd_Desktop("KSTAT", self~DlgHandle,1) /* VK_LBUTTON */
   if mb = "MIDDLE" then return Wnd_Desktop("KSTAT", self~DlgHandle,4) /* VK_MBUTTON */
   if mb = "RIGHT" then return Wnd_Desktop("KSTAT", self~DlgHandle,2) /* VK_RBUTTON */


::method Dump
    use arg adm
    if Arg() == 1 then do
        say "Dump of current dialog administration"
        say "====================================="
        call DumpAdmin "ADMDUMP", adm
        say  "Slot: " admdump.Slot
        say  "hThread: " admdump.hThread
        say  "hDialog: " admdump.hDialog
        say  "hMenu: " admdump.hMenu
        say  "BkgBrush: " admdump.BkgBrush
        say  "BkgBitmap: " admdump.BkgBitmap
        say  "TopMost: " admdump.TopMost
        say  "CurrentChild: " admdump.CurrentChild
        say  "DLL: " admdump.Dll
        say  "Queue: " admdump.queue
        say  "BmpButtons: " admdump.BmpButtons
        do i = 1 to admdump.BmpButtons
            say "    " admdump.BmpTab.i.id "->" admdump.BmpTab.i.normal "," admdump.BmpTab.i.focused "," admdump.BmpTab.i.selected "," admdump.BmpTab.i.disabled
        end
        say  "Messages: " admdump.Messages
        do i = 1 to admdump.Messages
            say "    " admdump.MsgTab.i.msg "," admdump.MsgTab.i.param1 "," admdump.MsgTab.i.param2 "==>" admdump.MsgTab.i.method
        end
        say  "DataItems: " admdump.DataItems
        do i = 1 to admdump.DataItems
            say "    " admdump.DataTab.i.id "("admdump.DataTab.i.type","admdump.DataTab.i.category")"
        end
        say  "ColorItems: " admdump.ColorItems
        do i = 1 to admdump.ColorItems
            say "    " admdump.ColorTab.i.id "(" admdump.ColorTab.i.Foreground "," admdump.ColorTab.i.Background ")"
        end
    end
    else do
        say "Dump of administration state"
        say "============================"
        call DumpAdmin "ADMDUMP"
        d = Digits()
        NUMERIC DIGITS 15
        do i = 1 to admdump.0
            say  "AdmBlock: " admdump.i.AdmBlock
            say  "Slot: " admdump.i.Slot
            say  "hThread: " admdump.i.hThread
            say  "hDialog: " admdump.i.hDialog
            if admdump.i.hDialog \=0 then say "Window title:" self~WindowTitle(x2d(admdump.i.hDialog))
            say  "hMenu: " admdump.i.hMenu
            say  "BkgBrush: " admdump.i.BkgBrush
            say  "BkgBitmap: " admdump.i.BkgBitmap
            say  "TopMost: " admdump.i.TopMost
            say  "CurrentChild: " admdump.i.CurrentChild
            say  "DLL: " admdump.i.Dll
            say  "Queue: " admdump.i.queue
            say  "BmpButtons: " admdump.i.BmpButtons
            say  "Messages: " admdump.i.Messages
            say  "DataItems: " admdump.i.DataItems
            say  "ColorItems: " admdump.i.ColorItems
            say "--------------------------"
        end
        NUMERIC DIGITS d
    end

/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/* Basic Dialog Class                                                         */
/*                                                                            */
/******************************************************************************/



/******************************************************************************/
/* The class WindowExtensions implements methods that are common to all windows, */
/* no matter whether they are dialogs or dialog items. It's an extension to   */
/* the WindowBase mixin-class                                                 */
/******************************************************************************/

::class 'WindowExtensions' public MIXINCLASS object

::method HideFast unguarded
   return WndShow_Pos("S",self~hwnd, "HIDE FAST")

::method ShowFast unguarded
   return WndShow_Pos("S",self~hwnd, "NORMAL FAST")

::method Display unguarded
   use arg disptype = "NORMAL"
   if disptype = "" then disptype = "NORMAL"; else disptype = disptype~translate
   if self~isA(.DialogControl) & (disptype~wordpos("MIN") <> 0 | disptype~wordpos("MAX") <> 0) then do
     if self~oDlg~isA(.PlainBaseDialog) then
       forward to (self~oDlg)
     else
       return 1
   end
   return WndShow_Pos("S",self~hwnd, disptype)

::method setFont unguarded external "LIBRARY oodialog winex_setFont"
::method getFont unguarded external "LIBRARY oodialog winex_getFont"

::method GetRect unguarded
   return WindowRect("GET", self~Hwnd)

::method GetClientRect unguarded
   use arg hwnd
   if Arg(1,'o') = 1 then hwnd = self~hwnd
   return WindowRect("GET", hwnd, "CLIENT")

::method SetRect unguarded
   use arg x, y, width, high, showOptions
   if Arg(5,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   return WndShow_Pos("P",self~hwnd, x, y, width, high, showOptions)

::method Update unguarded
   return WndShow_Pos("S",self~Hwnd, "UPDATE")

   /* This method draws the window */
::method Draw unguarded
   return WindowRect("RDW", self~hwnd, "")

::method Redraw unguarded
   return WndShow_Pos("S",self~hwnd, "REDRAW")

::method RedrawClient unguarded
   use arg erasebkg
   if Arg(1, 'o') = 1 then erasebkg = 0
   return WindowRect("RDW", self~Hwnd, erasebkg)

::method GetDC unguarded
   return HandleDC_Obj("G", self~hwnd)

::method FreeDC unguarded
   use arg dc
   return HandleDC_obj("F", self~hwnd, dc)

::method HScrollPos unguarded
    return WndShow_Pos("B", self~hwnd, 0)

::method SetHScrollPos unguarded
    use arg pos, redraw
    if Arg(2, 'o') = 1 then redraw = 1
    return WndShow_Pos("B", self~hwnd, 0, pos, redraw)

::method VScrollPos unguarded
    return WndShow_Pos("B", self~hwnd, 1)

::method SetVScrollPos unguarded
    use arg pos, redraw
    if Arg(2, 'o') = 1 then redraw = 1
    return WndShow_Pos("B", self~hwnd, 1, pos, redraw)

::method Scroll unguarded
    use arg x, y
    return WndShow_Pos("M", self~hwnd, x, y)

::method ScreenToClient unguarded
    use arg x,y
    return Wnd_Desktop("COORD", self~hwnd, x, y, "S2C")

::method ClientToScreen unguarded
    use arg x,y
    return Wnd_Desktop("COORD", self~hwnd, x, y, "C2S")

::method SetForegroundWindow unguarded
    use arg hwnd
    return Wnd_Desktop("TOP", hwnd)

::method ForegroundWindow unguarded
   return Wnd_Desktop("FG")

::method SetCursorPos unguarded
   use arg x, y
   return Wnd_Desktop("CURSOR", x, y)

::method CursorPos unguarded
   return Wnd_Desktop("CURSOR")

::method RestoreCursorShape unguarded
   use arg cursor
   return Wnd_Desktop("CURSOR", self~hwnd, "R", cursor)

::method Cursor_Arrow unguarded
   return Wnd_Desktop("CURSOR", self~hwnd, "R", 0)

::method Cursor_AppStarting unguarded
   return Wnd_Desktop("CURSOR", self~hwnd, "S", 32650)

::method Cursor_Cross unguarded
   return Wnd_Desktop("CURSOR", self~hwnd, "S", 32515)

::method Cursor_No unguarded
   return Wnd_Desktop("CURSOR", self~hwnd, "S", 32648)

::method Cursor_Wait unguarded
   return Wnd_Desktop("CURSOR", self~hwnd, "S", 32514)

-- CreateFont() has always been broken, maintained for program compatibility.
-- CreateFontEx() is a correct implementation.
::method CreateFont unguarded external "LIBRARY oodialog winex_createFont"
::method createFontEx unguarded external "LIBRARY oodialog winex_createFontEx"

   /* Deletes a font */
::method DeleteFont unguarded
   use arg hFont
   return self~DeleteObject(hFont)

   /* This method deletes a graphic object */
::method DeleteObject unguarded
   use arg obj
   return HandleDC_Obj("D", obj)

   /* This method loads a bitmap from file into memory */
   /* It returns a handle to the bitmap */

::method LoadBitmap unguarded
   use arg bmpFilename, option
   if Arg(2, 'o') = 1 then option = ""; else option = option~translate
   return LoadRemoveBitmap(0 /* NULL Adm */, bmpFilename, "LOAD" || option)

   /* This method will free a memory bitmap */

::method RemoveBitmap unguarded
   use arg hBitmap
   return LoadRemoveBitmap(0 /* NULL Adm */, hBitmap, "REMOVE")

::method CreateBrush unguarded
    use arg color, brushSpecifier
    if Arg() = 0 then return HandleDC_Obj("B")
    if Arg(1, "o") = 1 then color = 1
    if Arg(2, "o") = 1 then return HandleDC_Obj("B", color)
    brushSpecifier = brushSpecifier~translate
    if brushSpecifier = "UPDIAGONAL" | brushSpecifier = "DOWNDIAGONAL" | brushSpecifier = "CROSS" | brushSpecifier = "DIAGCROSS",
    | brushSpecifier = "HORIZONTAL" | brushSpecifier = "VERTICAL" then return HandleDC_Obj("B", color, brushSpecifier)
    else return HandleDC_Obj("B", 0 /* NULL ADM */, color, brushSpecifier)  /* brushSpecifier must be filename */

   /* This method allows you to write text to a window in the given font, in */
   /* the given size, and to the given position */
   /* argument 'fontStyle' can be one or more off: */
   /*    "THIN"            "UNDERLINE"    */
   /*    "EXTRALIGHT"      "ITALIC"       */
   /*    "LIGHT"           "STRIKEOUT"    */
   /*    "MEDIUM"                */
   /*    "SEMIBOLD"              */
   /*    "EXTRABOLD"             */
   /*    "BOLD"                  */
   /*    "HEAVY"                 */

::method Write unguarded
   argarr = Arg(1, "A")
   newarg = .array~new(argarr~Items+1)
   do i = 1 to Arg(); newarg[i+1] = argarr[i]; end
   newarg[1] = self~hwnd
   forward message (WriteToWindow) arguments (newarg)

   /* This method allows you to write text to a device context to a given */
   /* position */

::method WriteDirect unguarded
   use arg dc, xPos, yPos, text
   return WriteText(dc, xPos, yPos, text)               /* 4 arguments means don't use font */

   /* This method loads a graphic object into device context */

::method ObjectToDC unguarded
   use arg dc, obj
   return HandleDC_Obj("S", dc, obj)

   /* Loads font into device context of a window. */
   /* Use 'createFont' method to get a font handle */

::method FontToDC unguarded
   use arg dc, hFont
   return self~ObjectToDC(dc, hFont)

   /* Set the font color of a device context. */
   /* Use 'createFont' method to get a font handle */

::method FontColor unguarded
   use arg color, dc
   if Arg(1,'o') = 1 then color = 1
   return DrawGetSet("COL", dc, color)

::method getTextSizeScreen  external "LIBRARY oodialog winex_getTextSizeScreen"

   /* This method will create a pen in the given color and style */
   /* It returns a handle to a pen object */
::method CreatePen unguarded
   use arg width, style, color
   if Arg(1, "o") = 1 then width = 1
   if Arg(2, "o") = 1 then style = "SOLID"; else style = style~translate
   if Arg(3, "o") = 1 then color = 0
   return HandleDC_Obj("P", width, style, color)

   /* This method will draw a rectangle to the given device context */
   /* Use one of the following methods to get a valid DC: */
   /* 'GetDC', 'GetWindowDC', 'GetButtonDC' */

::method Rectangle unguarded
   use arg dc, x, y, cx, cy, fill
   if Arg(6, "o") = 1 then fill = ''
   if fill~left(1)~translate = 'F' then
        return DCDraw("REC", dc, x, y, cx, cy)
   ret = DrawGetSet("SDP", dc, x, y)
   ret = DCDraw("LN", dc, cx, y)
   ret = DCDraw("LN", dc, cx, cy)
   ret = DCDraw("LN", dc, x, cy)
   return DCDraw("LN", dc, x, y)

   /* This method draws a line within the device context */

::method DrawLine unguarded
   use arg dc, fromX, fromY, toX, toY
   if Arg(2, 'o') = 0 | Arg(3, 'o') = 0 then ret = DrawGetSet("SDP",dc, fromX, fromY)
   return DCDraw("LN",dc, toX, toY)

   /* This method draws a pixel within the device context */

::method DrawPixel unguarded
   use arg dc, x, y, color
   return DCDraw("PT",dc, X, Y, color)

   /* This method gets the color of a pixel within the device context */

::method GetPixel unguarded
   use arg dc, x, y
   return DrawGetSet("GPT", dc, X, Y)

   /* fill with current brush, border determined by color */
::method FillDrawing unguarded
   use arg dc, x, y, color
   return DCDraw("FL", dc, X, Y, color)

::method TransparentText unguarded
   use arg dc
   return DrawGetSet("SBK", dc, "TRANSPARENT")

::method OpaqueText unguarded
   use arg dc
   return DrawGetSet("SBK", dc, "OPAQUE")

   /* This method draws an arc / circle  */

::method DrawArc unguarded
   use arg dc, left, top, right, bottom, startx, starty, endx, endy
   return DCDraw("ARC",dc, left, top, right, bottom, startx, starty, endx, endy)

   /* This method draws a pie shape */

::method DrawPie unguarded
   use arg dc, left, top, right, bottom, xrad1, yrad1, xrad2, yrad2
   return DCDraw("PIE",dc, left, top, right, bottom, xrad1, yrad1, xrad2, yrad2)

   /* This method draws an arc / circle using an angle */

::method DrawAngleArc unguarded
   use arg dc, movex, movey, x, y, radius, startangle, sweepangle
   if Arg(2, 'o') = 0 | Arg(3, 'o') = 0 then ret = DrawGetSet("SDP", dc, movex, movey)
   return DCDraw("ANG",dc, x, y, radius, startangle, sweepangle)

::method SetArcDirection unguarded
   use arg dc, direction
   return DrawGetSet("SAD", dc, (direction~translate~wordpos("CLOCKWISE") > 0))

::method GetArcDirection unguarded
   use arg dc
   ret = DrawGetSet("GAD",dc)
   if ret = 1 then return "CLOCKWISE"
   else return "COUNTERCLOCKWISE"

::method LogRect2AbsRect unguarded
   parse arg l, t, r, b
   ret.left = l*self~FactorX
   ret.right = r*self~FactorX
   ret.top = t*self~FactorY
   ret.bottom = b*self~FactorY
   return ret.

::method AbsRect2LogRect unguarded
   parse arg l, t, r, b
   ret.left = l/self~FactorX
   ret.right = r/self~FactorX
   ret.top = t/self~FactorY
   ret.bottom = b/self~FactorY
   return ret.

/** class: BaseDialog
 * Implements methods that are common to all 3 types of dialogs, ResDialog,
 * RcDialog, and UserDialog.
 */
::class 'BaseDialog' public subclass PlainBaseDialog inherit DialogExtensions WindowExtensions

::method scrollNow attribute   -- Indicator whether or not scrolling is active

::method init
   forward class (super) continue
   if result <> 0 then return result

   self~scrollNow = 0   -- a kind of semaphore to handle multiple text scrolling
   self~initCode = 0

   self~bkgBitmap = 0     -- no background bitmap
   self~bkgBrushBmp = 0   -- no background bitmap
   self~menuBar = .nil
   return 0


::class 'ResourceImage' public

::method init external "LIBRARY oodialog ri_init"
::method release external "LIBRARY oodialog ri_release"
::method handle external "LIBRARY oodialog ri_handle"
::method isNull external "LIBRARY oodialog ri_isNull"
::method systemErrorCode external "LIBRARY oodialog ri_systemErrorCode"
::method getImage external "LIBRARY oodialog ri_getImage"
::method getImages external "LIBRARY oodialog ri_getImages"


::class 'Image' public

::method toID class external "LIBRARY oodialog image_toID_cls"
::method getImage class external "LIBRARY oodialog image_getImage_cls"
::method fromFiles class external "LIBRARY oodialog image_fromFiles_cls"
::method fromIDs class external "LIBRARY oodialog image_fromIDs_cls"
::method userIcon class external "LIBRARY oodialog image_userIcon_cls"

::method colorRef class external "LIBRARY oodialog image_colorRef_cls"
::method getRValue class external "LIBRARY oodialog image_getRValue_cls"
::method getGValue class external "LIBRARY oodialog image_getGValue_cls"
::method getBValue class external "LIBRARY oodialog image_getBValue_cls"

::method init external "LIBRARY oodialog image_init"
::method release external "LIBRARY oodialog image_release"
::method handle external "LIBRARY oodialog image_handle"
::method isNull external "LIBRARY oodialog image_isNull"
::method systemErrorCode external "LIBRARY oodialog image_systemErrorCode"

::class 'ImageList' public
::method create class external "LIBRARY oodialog il_create_cls"

::method init external "LIBRARY oodialog il_init"
::method release external "LIBRARY oodialog il_release"
::method add external "LIBRARY oodialog il_add"
::method addMasked external "LIBRARY oodialog il_addMasked"
::method addIcon external "LIBRARY oodialog il_addIcon"
::method addImages external "LIBRARY oodialog il_addImages"
::method getCount external "LIBRARY oodialog il_getCount"
::method getImageSize external "LIBRARY oodialog il_getImageSize"
::method duplicate external "LIBRARY oodialog il_duplicate"
::method remove external "LIBRARY oodialog il_remove"
::method removeAll external "LIBRARY oodialog il_removeAll"
::method handle external "LIBRARY oodialog il_handle"
::method isNull external "LIBRARY oodialog il_isNull"
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/* Resource Dialog Class                                                      */
/*                                                                            */
/******************************************************************************/

::class 'ResDialog' subclass BaseDialog public

::method setFontAttrib private external "LIBRARY oodialog resdlg_setFontAttrib_pvt"

::method init
   expose library resource dlgData.
   use arg library, resource, dlgData., includeFile

   if arg(4, 'E') then ret = self~init:super(library, resource, dlgData., includeFile)
   else ret = self~init:super(library, resource, dlgData.)
   if ret \= 0 then return ret

   if arg(3,'o') then self~useStem = 0; else self~useStem = 1
   if \ resource~datatype("N") then resource = self~resolveSymbolicId(resource)
   if resource < 1 then self~initCode = -1

   return self~initCode

-- Create a Windows dialog out of the resource stored in Library.
::method startIt
   expose library resource dlgData.
   use arg icon = 0, modal = 0

   icon = self~resolveIconID(icon)

   if modal <> 0 & modal <> 1 then do
     if modal~translate = "NOTMODAL" then modal = 1
     else modal = 0
   end

   self~dlgHandle = StartDialog(self~adm, library, resource, self~autoDetect, 1, icon, modal)
   if self~dlgHandle \= 0 then do
      self~setFontAttrib
      /* if autodetection is on, create the data attributes */
      if self~autoDetect = 1 then do
         id = 1; i=0
         do while id > 0
            parse value DataTable(self~adm,"GET", i) with id typ cat
            hnd = self~getItem(id)
            if (hnd \= 0) then do
                txt = Wnd_Desktop("TXT",hnd)
                self~addAttribute(id, txt)
            end
            i = i +1
         end
      end
      self~initDialog
   end

   return self~dlgHandle


::method stopIt
   forward class (super) continue
   ret = result
   if self~bkgBrushBmp \= 0 then self~deleteObject(self~bkgBrushBmp)
   if self~bkgBitmap \= 0 then self~removeBitmap(self~bkgBitmap)
   return ret

::method setMenu
  use strict arg id
  menuBar = .BinaryMenuBar~new(self, id, self, 0, .true)
  if menuBar~initCode == 0 then self~menuBar = menuBar
  return menuBar~initCode

/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/* User Dialog Class                                                          */
/*                                                                            */
/******************************************************************************/
/* include the base dialog class */


/* This class extends the 'BaseDialog' class */
::class 'UserDialog' subclass BaseDialog public inherit DynamicDialog

::method init
   use arg DlgData., includeFile

   newArgs = .array~new(2)
   newArgs[1] = 0
   newArgs[2] = 0
   if arg(1, 'E') then newArgs[3] = dlgData.
   if Arg(2, 'E') then newArgs[4] = includeFile
   forward class (super) arguments (newArgs) continue

   if result = 0 then forward message "DynamicInit"
   else return result

::method StopIt
   forward class (super) continue
   forward message "StopDynamic" continue
   ret = result
   if self~BkgBrushBmp \= 0 then self~DeleteObject(self~BkgBrushBmp)
   if self~BkgBitmap \= 0 then self~RemoveBitmap(self~BkgBitmap)
   return ret

/******************************************************************************/
/* Menu/Action bar methods.                                                   */

::method SetMenu
    menuBar = self~menuBar

    if menuBar == .nil then return -2

    if menuBar~isA(.UserMenuBar) then do
      ret = menuBar~complete
      if ret < 0 then return ret
    end

    return menuBar~attachTo(self)

::method CreateMenu
    use strict arg count = 100

    menuBar = .UserMenuBar~new(self, count)
    if menuBar~initCode <> 0 then return menuBar~initCode

    self~menuBar = menuBar
    return 0;

::method AddPopupMenu
    use strict arg name, opt = ""
    if self~menuBar == .nil then return -2
    return self~menuBar~addPopup(name, , opt)

::method AddMenuItem
    use strict arg name, id, opt = "", msgToRaise = .nil
    if self~menuBar == .nil then return -2
    return self~menuBar~addItem(name, id, opt, msgToRaise)

::method AddMenuSeparator
    if self~menuBar == .nil then return -2
    return self~menuBar~addSeparator

::method LoadMenu protected
  use strict arg resfile, menuid = .nil, loadopts = "", count = 50

  connect = .false
  if loadopts~caselessWordPos("CONNECTITEMS") <> 0 then connect = .true

  menuBar = .ScriptMenuBar~new(resFile, menuid, self, 0, count, connect)
  if menuBar~initCode <> 0 then return menuBar~initCode

  self~menuBar = menuBar
  return 0
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 2007-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Windows Dialog Interface for Open Object Rexx (ooRexx)
 *
 * Resource Script Dialog Class (RcDialog)
 *
 * Provides support for creating a dialog defined in a resource script file,
 * (most commonly a *.rc file, but *.dlg is also common.)
 */

::class 'RcDialog' subclass UserDialog public

::method init
  use arg rcFile, idDlg, initData., includeFile, options = "", expected = 200

  newArgs = .array~new(2)
  if arg(3, 'E') then newArgs[1] = arg(3)
  if arg(4, 'E') then newArgs[2] = arg(4)
  forward class (super) arguments (newArgs) continue
  if result <> 0 then return result

  self~initCode = self~load(rcFile, idDlg, options, expected)

return self~initCode

/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 2008-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/

/* Windows Dialog Interface for Open Object Rexx (ooRexx)
 *
 * Menu Classes
 *
 * Provides support for all types of Windows Menus.
 */


/** PLEASE NOTE:
 *
 * The classes and methods in this file are purposively NOT DOCUMENTED.  Do NOT
 * use these classes or methods because the classes, methods, and / or arugments
 * WILL CHANGE in the next release of ooRexx. If you do use these classes in
 * your own code, you do so at you own risk.
 *
 */

/* class: Menu - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
     A mix in class implementing functionality that is common to all menus.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'Menu' public mixinclass Object

::attribute hMenu set private
::attribute hMenu get

::attribute dlg private

::attribute initCode set private
::attribute initCode get

-- The resource ID of a menu.
::attribute wID set private
::attribute wID get

::attribute connectionRequested private
::attribute connectionQ private

::method menuInit private
  use strict arg symbolSrc

  self~hMenu = 0
  self~dlg = .nil
  self~initCode = 0
  self~wID = .nil

  self~connectionRequested = .false
  self~connectionQ = .queue~new

  select
    when symbolSrc~isA(.directory) then self~constDir = symbolSrc

    when symbolSrc~isA(.ResourceUtils) then self~constDir = symbolSrc~constDir

    when symbolSrc~isA(.string) then do
      self~ConstDir = .directory~new
      self~ParseIncludeFile(symbolSrc)
    end

    otherwise self~constDir = .directory~new
  end

  return 0

::method maybeResolveID private
  use strict arg id

  if id <> .nil then do
    id = self~resolveResourceID(id)
    if id = -1 then do
      self~initCode = id
      return .false
    end
    self~wID = id
  end

  return .true

/* getCount() returns the number of items in a menu.  The default is to get the
 * count of items in this menu, but passing in a raw menu handle will also work.
 */
::method getCount
  use arg obj

  select
    when arg(1, 'O') then return WinMenu("COUNT", self~hMenu)
    when self~isValidMenuHandle(obj) then return WinMenu("COUNT", obj)
    otherwise return -1
  end
  -- End select

/** isValidMenu()  Returns true if *this* Menu object represents a menu that the
 * operating system says is valid, otherwise false.
 */
::method isValidMenu
  return WinMenu("ISMENU", self~hMenu)

/** isValidMenuHandle()  Returns true if the specified handle represents a menu
 * that the operating system says is valid, otherwise false.
 */
::method isValidMenuHandle
  use strict arg handle
  return WinMenu("ISMENU", handle)

/** isSubMenu()  Returns true if the menu item specified is a menu item that
 * opens a submenu, othewise false.
 */
::method isSubMenu
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return .false

  return GetMII("ISSUB", self~hMenu, itemID, byPosition)

/** isCommandItem()  Returns true if the menu item specified is a command menu
 * item, i.e., not a menu item that opens a submenu or a separator, othewise
 * false.
 */
::method isCommandItem
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return .false

  return GetMII("ISITEM", self~hMenu, itemID, byPosition)

/** isSeparator()  Returns true if the menu item is a separator, otherwise false.
 * Note that with extended menus, it is possible to assign resource IDs even to
 * separators.  Therefore it is possible to query both by ID and position.
 */
::method isSeparator
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return .false

  return GetMII("ISSEP", self~hMenu, itemID, byPosition)

/** getHandle()  Returns the raw menu handle of this menu.
 */
::method getHandle
  return self~hMenu

/** getMenuHandle()  Get the raw menu handle of a submenu of this menu.
 */
::method getMenuHandle
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return -1

  return GetMII("SUB", self~hMenu, itemID, byPosition)

/** releaseMenuHandle()  Releases (frees) the operating system resources used by
 * a menu.  The menu is specified by its raw handle.  To release the operating
 * system resources for an ooDialog Menu object, use the destroy() method.
 */
::method releaseMenuHandle
  use strict arg handle
  return WinMenu("DEL", handle)

/** getPopup()  Returns a .PopupMenu that represents the specified submenu.
 * Check the PopupMenu initCode to determine success.
 */
::method getPopup unguarded
  use strict arg itemID, byPosition = .false, symbolSrc = .nil

  -- We want to *always* return a PopupMenu object, so even if there is an error
  -- with the itemID or with getMenuHandle(), we still pass the value on to
  -- PopupMenu~new.  .PopupMenu~init() can handle the incorrect values (if any)
  -- and will create the object with a non-zero initCode.

  itemID = self~resolveItemID(itemID, byPosition)

  -- This is the id to be passed on to PopupMenu~new.
  if byPosition then id = .nil
  else id = itemID

  hSubMenu = GetMII("SUB", self~hMenu, itemID, byPosition)

  return .PopupMenu~new(id, hSubMenu, symbolSrc)

::method insertPopup unguarded
  use strict arg before, popup, text, id, stateOpts = "", typeOpts = "", byPosition = .false

  id = self~resolveResourceID(id)
  if id == -1 then return -1

  before = self~resolveItemID(before, byPosition)
  if before == -1 then return -1

  if \ popup~isA(.PopupMenu) then return -2

  ret = InsertMII("POP", self~hMenu, before, byPosition, popup~getHandle, text, id, stateOpts~upper, typeOpts~upper)
  return self~maybeRedraw(ret)

::method insertItem unguarded
  use strict arg before, text, id, stateOpts = "", typeOpts = "", byPosition = .false, connect = .false

  id = self~resolveResourceID(id)
  if id == -1 then return -1

  before = self~resolveItemID(before, byPosition)
  if before == -1 then return -1

  ret = InsertMII("ITEM", self~hMenu, before, byPosition, text, id, stateOpts~upper, typeOpts~upper)

  if connect then do
    msgToRaise = text~changeStr('&', "")~space(0)
    if self~dlg <> .nil then do
      self~connectItem(id, msgToRaise, self~dlg)
    end
    else do
      self~connectionRequested = .true
      self~connectionQ~queue(.MapItem~new(id, msgToRaise))
    end
  end

  return self~maybeRedraw(ret)

::method insertSeparator unguarded
  use strict arg before, byPosition = .false

  before = self~resolveItemID(before, byPosition)
  if before == -1 then return -1

  ret = InsertMII("SEP", self~hMenu, before, byPosition)
  return self~maybeRedraw(ret)

::method maybeRedraw private
  use strict arg ret
  if ret <> 0 | self~dlg == .nil then return ret
  return WinMenu("DRAW", self~dlg~dlgHandle)

/** deleteSubMenu()
 * Removes the submenu from this menu and frees the hMenu.  To be able to reuse
 * the the submenu use the removeSubMenu() method.
 */
::method deleteSubMenu
  use strict arg id, byPosition = .false
  forward to (self~delete)

/** removeSubMenu()
 * Remove the submenu from this menu and return it, as a .PopupMenu to the
 * caller.
 */
::method removeSubMenu
  use strict arg id, byPosition = .false, symbolSrc = .nil

  subMenu = self~getPopup(id, byPosition, symbolSrc)
  if subMenu~initCode <> 0 then return subMenu

  self~delete(id, byPosition)
  return subMenu

/** removeItem() remove a menu item from this menu.
 */
::method removeItem
  use strict arg id, byPosition = .false
  forward to (self~delete)

::method removeSeparator
  use strict arg id, byPosition = .false
  forward to (self~delete)

::method delete private
  use strict arg id, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return .false

  ret = WinMenu("DELMENU", self~hMenu, itemID, byPosition)
  return self~maybeRedraw(ret)

::method isEnabled
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return .false

  return GetMII("ISTATE", self~hMenu, itemID, byPosition)~wordPos("ENABLED") <> 0

::method isGrayed
  use strict arg itemID, byPosition = .false
  forward to (self~isEnabled)

::method enable
  use strict arg itemIDs, byPosition = .false, enable = .true

  if itemIDs~isA(.Collection) then do
    status = 0
    do id over itemIDs
      ret = self~doEnable(id, byPosition, enable)
      if ret < 0, ret <> -1 then return ret
      if ret == -1 then status = -1
    end
  end
  else do
    status = self~doEnable(itemIDs, byPosition, enable)
  end

  return status

::method disable
  use strict arg itemIDs, byPosition = .false
  return self~enable(itemIDs, byPosition, .false)

::method gray
  use strict arg itemIDs, byPosition = .false
  return self~enable(itemIDs, byPosition, .false)

::method doEnable private
  use strict arg id, byPosition, enable

  id = self~resolveItemID(id, byPosition)
  if id == -1 then return -1

  keyword = "ENABLED"
  if \ enable then keyword = "DISABLED"

  return SetMII("ISTATE", self~hMenu, id, byPosition, keyword)

::method isChecked
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return .false

  return GetMII("ISTATE", self~hMenu, itemID, byPosition)~wordPos("CHECKED") <> 0

::method check
  use strict arg itemIDs, byPosition = .false, check = .true

  if itemIDs~isA(.Collection) then do
    status = 0
    do id over itemIDs
      ret = self~doCheck(id, byPosition, check)
      if ret < 0, ret <> -1 then return ret
      if ret == -1 then status = -1
    end
  end
  else do
    status = self~doCheck(itemIDs, byPosition, check)
  end

  return status

::method uncheck
  use strict arg itemIDs, byPosition = .false
  return self~check(itemIDs, byPosition, .false)

::method doCheck private
  use strict arg id, byPosition, check

  id = self~resolveItemID(id, byPosition)
  if id == -1 then return -1

  keyword = "CHECKED"
  if \ check then keyword = "UNCHECKED"

  return SetMII("ISTATE", self~hMenu, id, byPosition, keyword)

::method checkRadio
  use strict arg idStart, idEnd, idCheck, byPosition = .false
  idStart = self~resolveItemID(idStart, byPosition)
  idEnd   = self~resolveItemID(idEnd, byPosition)
  idCheck = self~resolveItemID(idCheck, byPosition)
  if (idStart == -1) | (idEnd == -1) | (idCheck == -1) then return -1

  return WinMenu("CHECKRADIO", self~hMenu, idStart, idEnd, idCheck, byPosition)

::method getState
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return -1

  return GetMII("ISTATE", self~hMenu, itemID, byPosition)

::method getType
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return -1

  return GetMII("TYPE", self~hMenu, itemID, byPosition)

/* Set or get a menu item's text */
::method getText
  use strict arg itemID, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return -1

  return GetMII("TEXT", self~hMenu, itemID, byPosition)

::method setText
  use strict arg itemID, text, byPosition = .false

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return -1

  return SetMII("TEXT", self~hMenu, itemID, byPosition, text)

/* getID()  Get an item ID, by definition this is by position.  If the
 * ID is already known, there is no point in querying for it.
 */
::method getID
  use strict arg posID
  return GetMII("ID", self~hMenu, posID, .true)

/* setID()  Set an item ID, by position is the defualt.  This function can
 * also be used to change an existing ID, so the by ID form is supported.  Note
 * that with extended menus all items can have IDs, even separators.
 */
::method setID
  use strict arg itemID, id, byPosition = .true

  itemID = self~resolveItemID(itemID, byPosition)
  if itemID == -1 then return -1

  id = self~resolveResourceID(id)
  if id == -1 then return -1

  return setMII("ID", self~hMenu, itemID, byPosition, id)

/* resolveItemID()  Convenience function to resolve an argument that can be
 * either a resource ID or a position ID.
 */
::method resolveItemID private
  use strict arg itemID, byPosition

  if byPosition then do
    if itemID~isA(.string), itemID~datatype('W'), itemID > -1 then return itemID
    else return -1
  end

  return self~resolveResourceID(itemID)


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
  Methods that affect an entire menu, rather than a menu item.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

/* Set or get the menu help context ID */
::method setHelpID
  use strict arg id, recurse = .false  -- recurse means apply to submenus also.

  id = self~resolveResourceID(id)
  if id == -1 then return -1

  -- Be sure recurse is a boolean.
  if recurse \== .true, recurse \== .false then recurse = .true

  return SetMI("HELP", self~hMenu, id, recurse)

::method getHelpID
  return GetMI("HELP", self~hMenu)

::method destroy
  if self~isA(.MenuBar), self~dlg <> .nil, then self~detach
  ret = WinMenu("DEL", self~hMenu)
  self~hMenu = 0
  return ret

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
  Message connection methods.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::method checkConnectionRequest private
  use strict arg dlg

  if self~connectionRequested then do
    self~connectionRequested = .false
    do map over self~connectionQ
      self~connectItem(map~key, map~item, dlg)
    end
    self~connectionQ~empty
  end

/** connectItem()  Connects the menu command item with the specified ID to the
 * specified method.
 */
::method connectItem
  use strict arg id, msgToRaise, dlg = (self~dlg)

  id = self~resolveResourceID(id)
  if id == -1 then return -1

  if \ dlg~isA(.BaseDialog) then return -3

  if id < 3 | id = 9 then return 0

  ret = dlg~addUserMsg(msgToRaise, 0x00000111, 0xFFFFFFFF, id, 0x0000FFFF, 0, 0)
  if ret == 1 then return -4

  return 0

/** connectItemsToMsg()  Connects a collection of menu command items to the
 * single specified method.
 */
::method connectItemsToMsg
  use strict arg itemIDs, msgToRaise, dlg = (self~dlg)

  if \ itemIDs~isA(.Collection) then return -2

  status = 0
  do id over itemIDs
    ret = self~connectItem(id, msgToRaise, dlg)
    if ret < 0, ret <> -1 then return ret
    if ret == -1 then status = -1
  end

  return status

::method connectAllItems external "LIBRARY oodialog menu_connectAllItems"

-- Connect WM_CONTEXTMENU messages to a method.
::method connectContextMenu
  use strict arg msgToRaise, hwnd = .nil, dlg = (self~dlg)

  if \ dlg~isA(.BaseDialog) then return -3

  if hwnd \== .nil then
    return dlg~addUserMsg(msgToRaise, 0x0000007B, 0xFFFFFFFF, hwnd, 0xFFFFFFFF, 0, 0, 0x00000201)
  else
    return dlg~addUserMsg(msgToRaise, 0x0000007B, 0xFFFFFFFF, 0, 0, 0, 0, 0x00000201)

::method test external "LIBRARY oodialog menu_test"

/* class: MenuBar- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
     A mix in class implementing functionality that is common to all menu bars.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'MenuBar' public mixinclass Menu

/** attachTo()
 * Attaches this menu bar to the specified dialog window.  This method is not
 * intended to be used when this menu is already attached to a window.  Use the
 * replace method for that situation.
 *
 * However, a menu can only have one owner window.  So, if this menu is already
 * attached to a dialog, it is first detached.
 */
::method attachTo
  use strict arg dlg
  if \ dlg~isA(.BaseDialog) then return -3

  if self~dlg <> .nil then self~detach

  oldMenu = WinMenu("ASSOC", self~hMenu, dlg~hwnd)
  if oldMenu < 0 then return oldMenu

  dlg~linkMenu(self)
  self~dlg = dlg

  self~checkConnectionRequest(dlg)

  return oldMenu

::method replace
  use strict arg dlg
  if \ dlg~isA(.BaseDialog) then return -3

::method detach
  dlg = self~dlg
  if dlg == .nil then return -1

  ret = WinMenu("DET", dlg~hwnd)
  dlg~unlinkMenu
  self~dlg = .nil
  return ret

/** unInit()
 * A menu that is not assigned to a window must be destroyed by the application
 * when it closes.  On the other hand, menus assigned to a window are destroyed
 * by the system when the window is destroyed.
 */
::method unInit
  if self~dlg <> .nil then return
  self~destroy

::method failedInit private
  use strict arg ret
  self~destroy
  self~initCode = ret
  return ret


/* class: BinaryMenuBar- - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
     A menu bar menu created from a binary resource, or created as an empty
     menu.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'BinaryMenuBar' public subclass Object inherit MenuBar ResourceUtils

::method init
  use strict arg src = .nil, id = .nil, symbolSrc = .nil, helpID = 0, attach = .false, autoConnect = .false

  if self~menuInit(symbolSrc) <> 0 then return self~initCode
  if \ self~maybeResolveID(id) then return self~initCode

  select
    when src~isA(.ResDialog) then do
      if self~wID == .nil then do
        self~initCode = -1
        return -1
      end

      hMenu = WinMenu("LOAD", 0, src~adm, self~wID)
    end

    when src~isA(.string) then do
      if src~datatype('W') then do
        -- This is to create a BinaryMenuBar from a hMenu.  But, is this really
        -- of any use?  It could be used of creating a System menu, but a system
        -- menu might be better off as its own class.
        if WinMenu("ISMENU", src) then hMenu = src
        else hMenu = -1
      end
      else do
        if self~wID == .nil then do
          self~initCode = -1
          return -1
        end

        hMenu = WinMenu("LOAD", 1, src, self~wID)
      end
    end

    when src == .nil then hMenu = WinMenu("CREATE", 0)

    otherwise hMenu = -1
  end
  -- End select

  if hMenu < 0 then do
    self~initCode = hMenu
    return hMenu
  end

  self~hMenu = hMenu
  if helpID <> 0 then self~setHelpID(helpID)

  select
    when attach & src~isA(.ResDialog) then do
      ret = self~attachTo(src)
      if ret < 0 then return self~failedInit(ret)
    end

    when attach & symbolSrc~isA(.BaseDialog) then do
      ret = self~attachTo(symbolSrc)
      if ret < 0 then return self~failedInit(ret)
    end

    otherwise nop
  end
  -- End select

  return 0


/* class: PopupMenu- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
     A popup menu.  (Popup menus are also called, at various times, submenus,
     drop down menus, or context menus.)
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'PopupMenu' public subclass Object inherit Menu ResourceUtils

::method init
  use strict arg id = .nil, handle = .nil, symbolSrc = .nil, helpID = 0

  if self~menuInit(symbolSrc) <> 0 then return self~initCode
  if \ self~maybeResolveID(id) then return self~initCode

  select
    when handle == .nil then do
      handle = WinMenu("CREATE", 1)
    end

    when handle < 0 then nop

    otherwise do
      if \ WinMenu("ISMENU", handle) then handle = -2
    end
  end
  -- End select

  if handle < 0 then do
    self~initCode = handle
    return handle
  end

  self~hMenu = handle
  if helpID <> 0 then self~setHelpID(helpID)

  return 0

/** show() displays a context menu and returns after the user has selected an
 * item or canceled.  When the user selects an item, a WM_COMMAND message with
 * the id of the selection is generated.  If the user cancels, nothing happens.
 * 0 indicates success, less than 0 is the negated system error code.
 */
::method show unguarded
  use strict arg x, y, owner = (self~dlg), hAlign = "RIGHT", vAlign = "BOTTOM", bothButtons = .false, miscOpts = ""

  if \ owner~isA(.BaseDialog) then return -3
  if self~dlg \== owner then do
    -- TODO Revisit  Question: Do we want to set the assigned dialog to the owner ??
    self~dlg = owner
  end

  if bothButtons then mode = "RIGHT"
  else mode = ""

  return TrackPopup(mode, self~hMenu, owner~dlgHandle, x, y, hAlign~upper, vAlign~upper, miscOpts~upper)

/** track() displays a contex menu and returns after the user has selected an
 * item or canceled the menu.  No notification (no WM_COMMAND message) is sent
 * to the owner window.  The selected id is returned, or 0 if the menu is
 * canceled or an error happens.
 */
::method track unguarded
  use strict arg x, y, owner = (self~dlg), hAlign = "RIGHT", vAlign = "BOTTOM", bothButtons = .false, miscOpts = ""

  if \ owner~isA(.BaseDialog) then return -3
  if self~dlg \== owner then do
    -- TODO Revisit  Question: Do we want to set the assigned dialog to the owner ??
    self~dlg = owner
  end

  if bothButtons then mode = "NONOTIFY RIGHT"
  else mode = "NONOTIFY"

  return TrackPopup(mode, self~hMenu, owner~dlgHandle, x, y, hAlign~upper, vAlign~upper, miscOpts~upper)

::method assignTo unguarded
  use strict arg dlg
  if \ dlg~isA(.BaseDialog) then return -3

  oldDlg = self~dlg
  self~dlg = dlg

  self~checkConnectionRequest(dlg)
  return oldDlg

/* class: MenuTemplate - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
    A mix in class supplying the funtionality to create a menu template in
    memory.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'MenuTemplate' public mixinclass ResourceUtils

::method memInit private
  expose basePtr currentPtr memHandle isFinal
  use strict arg count, helpID

  isFinal = .false

  parse value MemMenu("INIT", count, helpID) with memHandle basePtr currentPtr

  if memHandle < 0 then do
    self~finalize
    return memHandle
  end

  return 0

::method isFinished
  expose isFinal
  return isFinal

::method finish
  expose basePtr currentPtr memHandle isFinal

  if isFinal | memHandle == 0 then return -2

  hMenu = WinMenu("LOAD", 2, memHandle, basePtr)
  self~finalize
  return hMenu

::method addPopup
  expose currentPtr isFinal
  use strict arg name, id = 0, opt = "", helpID = 0

  if isFinal then return -2

  id = self~checkId(id)
  if  id == -1 then return id

  -- Rather than fail on a bad help ID, just use 0.
  helpID = self~checkId(helpID)
  if  helpID == -1 then helpID = 0

  currentPtr = MemMenu("ADD", currentPtr, id, name, "POPUP " || opt~upper, helpID)

  return 0

::method addItem
  expose currentPtr isFinal
  use strict arg name, id, opt = "", msgToRaise = .nil

  if isFinal then return -2

  id = self~CheckId(id)
  if  id == -1 then return id

  currentPtr = MemMenu("ADD", currentPtr, id, name, opt~upper)

  if msgToRaise <> .nil then do
    self~connectionRequested = .true
    self~connectionQ~queue(.MapItem~new(id, msgToRaise))
  end

  return 0

/* With extended menus, separators can have IDs.  This is useful ...  */
::method addSeparator
  expose currentPtr isFinal
  use strict arg id = 0, opt = ""

  if isFinal then return -2

  -- Rather than fail on a bad ID, just use 0.
  id = self~checkId(id)
  if  id == -1 then id = 0

  currentPtr = MemMenu("ADD", currentPtr, id, "", "SEPARATOR")
  return 0

::method finalize private
  expose basePtr currentPtr memHandle isFinal

  basePtr = 0
  currentPtr = 0
  memHandle = 0
  isFinal = .true


/* class: UserMenuBar- - - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
     A menu bar menu whose initial menu template is created in memory though
     user (programmer) defined statements.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'UserMenuBar' public subclass Object inherit MenuBar MenuTemplate

-- The context help ID for a menu.  Assigning the help ID in the
-- MENUEX_TEMPLATE_HEADER does not work as Microsoft documents it. This is used
-- as a work around.  It is not maintained to always reflect with the real id
-- is; use self~getHelpID() to determine the actual help ID.
::attribute helpID private

::method init
  use strict arg symbolSrc = .nil, count = 100, helpID = 0

  if self~menuInit(symbolSrc) <> 0 then return self~initCode

  -- Rather than fail on a bad help ID, just use 0.
  helpID = self~checkId(helpID)
  if  helpID == -1 then helpID = 0

  ret = self~memInit(count, helpID)
  if ret < 0 then do
    self~initCode = ret
    return ret
  end

  -- Work around.  Setting the help ID in the in-memory template does not work
  -- as documented.
  self~helpID = helpID

  return 0

::method complete
  hMenu = self~finish
  if hMenu < 0 then return hMenu

  self~hMenu = hMenu
  if self~helpID <> 0 then self~setHelpID(self~HelpID)

  return 0


/* class: ScriptMenuBar- - - - - - - - - - - - - - - - - - - - - - - - - - - -*\
     A menu bar menu whose initial menu template is created in memory by parsing
     a resouce script file.
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/
::class 'ScriptMenuBar' public subclass Object inherit MenuBar MenuTemplate

::method init
  use strict arg rcFile, id = .nil, symbolSrc = .nil, helpID = 0, count = 50, connect = .false, attach = .false

  if self~menuInit(symbolSrc) <> 0 then return self~initCode
  if \ self~maybeResolveID(id) then return self~initCode

  helpID = self~checkId(helpID)
  if  helpID == -1 then helpID = 0

  ret = self~memInit(count, helpID)
  if ret < 0 then do
    self~initCode = ret
    return ret
  end

  ret = self~load(rcFile, self~wID, connect, count)
  if ret <> 0 then do
    self~initCode = ret
    return ret
  end

  hMenu = self~finish
  if hMenu < 0 then do
    self~initCode = hMenu
    return hMenu
  end

  self~hMenu = hMenu

  if helpID <> 0 then self~setHelpID(helpID)

  if attach & symbolSrc~isA(.BaseDialog) then do
    ret = self~attachTo(symbolSrc)
    if ret < 0 then return self~failedInit(ret)
  end

  return 0

::method load private
   use strict arg resfile, menuid, connect, count

   file = SysSearchPath("PATH",resfile)
   if file == "" then do
      say "Resource file not found -->" resfile
      return 1
   end

   f = .stream~new(file)
   op = f~open(read)
   if op \= "READY:" then do
      say "Resource file not readable -->" op
      return 1
   end

   found = 0; n = 0
   fl = f~lines
   do while found = 0 & fl > 0
      do while n = 0 & fl > 0
         s = f~linein; fl = fl - 1
         /* Add any sybmolic IDs defined in the file to the ConstDir.  Only
          * check for #define at the top of the file.  We check for symbolic IDS
          * here because menu resources could be defined in a separate file.
          */
         if s~wordpos("#define") > 0 then do
            s = s~translate(" ", "9"x)
            parse var s "#define " symb iid
            self~ConstDir[symb~space(0)~translate] = iid
         end
         n = s~wordpos("MENU")
      end

      if (self~checkfile(f) = 1) then return 1

      if menuid == .nil  then found = 1
      else do
         if s~word(n-1)~translate = menuid~translate then found = 1
         else if self~CheckId(s~word(n-1)) = menuid then found = 1
         else n = 0
      end
   end

   s = f~linein; fl = fl - 1
   do while s~wordpos("BEGIN") = 0 & s~pos("{") = 0 & fl > 0
      s = f~linein; fl = fl - 1
   end

   if (self~checkFile(f) = 1) then return 1

   rcarray = .array~new(50)
   bracket = 1
   cur = 0
   endcount = 0

   prevs = ""
   s = f~linein; fl = fl - 1
   do while bracket > 0 & fl >= 0
      if s~wordpos("END") > 0 | s~pos("}") > 0 then
      do
         bracket = bracket - 1;
         endcount = endcount + 1
         cur = cur + 1
         rcarray[cur] = s
      end
      else if s~wordpos("BEGIN") > 0 | s~pos("{") > 0 then do
         bracket = bracket + 1;
         cur = cur + 1
         rcarray[cur] = s
      end
      else if s~strip \= "" then do
         cur = cur + 1
         rcarray[cur] = s
      end
      s = f~linein; fl = fl - 1
   end

   f~close
   arrcount = cur;

   if (count < cur - endcount) then count = cur - endcount +5;

   self~processingLoad = 1

   do i = 1 to arrcount
      s = rcarray[i]

      select
         when s~wordpos("POPUP") > 0 then do
            parse var s type '"'name'"'    "," opt

            j = i + 1;
            bracket = 0
            do until bracket = 0
              if rcarray[j]~wordpos("BEGIN") > 0 | rcarray[j]~pos("{") > 0 then
                bracket = bracket + 1
              else if rcarray[j]~wordpos("END") > 0 | rcarray[j]~pos("}") > 0 then
                bracket = bracket - 1
              j = j +1
            end

            if rcarray[j]~wordpos("END") > 0 | rcarray[j]~pos("}") > 0 then opt = opt || " END"
            self~addPopup(name, , opt)
         end

         when s~wordpos("SEPARATOR") > 0 then do
            parse var s type sep opt
            if rcarray[i+1]~wordpos("END") > 0 | rcarray[i+1]~pos("}") > 0 then opt = opt || " END"
            self~addSeparator( , opt)
         end

         when s~wordpos("MENUITEM") > 0 then do
            parse var s type '"'name'"'    "," id "," opt
            if rcarray[i+1]~wordpos("END") > 0 | rcarray[i+1]~pos("}") > 0 then opt = opt || " END"
            if connect then
               self~addItem(name, id, opt, name~changeStr('&', "")~space(0))
            else
               self~addItem(name, id, opt)
         end

         when s~wordpos("END") > 0 | s~pos("}") > 0 | s~wordpos("BEGIN") > 0 | s~pos("{") > 0 then nop;

         otherwise do
            self~errorFile(f, s)
            return 1
         end
     end /*select 1*/
  end /* do while */
  self~processingLoad = 0

  return 0

::class 'MapItem' public

::attribute key set private
::attribute key get

::attribute item set private
::attribute item get

::method init
  expose key item
  use strict arg key, item

/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/* Category Dialog Class                                                      */
/*                                                                            */
/******************************************************************************/

/* This class creates and controls a dialog that has more than one panel. */
/* It is similar to the 'notebook' control available in OS/2 or the PropertySheet */
/* available in the Windows 95 user interface. You can switch among */
/* diferent pages by clicking radio buttons. Each page has its own */
/* window controls. You can setup a page out of a dialog resource script. */

::class 'CategoryDialog' subclass UserDialog public

   /* arg style=[ "DROPDOWN" | "TOPLINE" | "NOBORDER" | "WIZARD"] */

 -- A directory that contains internal data.  This should be private, but I'm not sure of the
 -- consequences to existing programs if it is changed.  MM
::attribute catalog unguarded protected
::method StaticID attribute protected  /*  */
::method CatX attribute private
::method CatY attribute private
::method CatCX attribute private
::method CatStyle attribute private
::method CatLabel attribute private
::method CatMax attribute private
::method Wizard attribute private

::method "!MarkAsCatDlg!"   /* this is to mark the class as a category dialog */

   /* arg style=[ "DROPDOWN" | "TOPLINE" | "NOBORDER" | "WIZARD"] */

::method Init
   use arg DlgData., cdx, cdy, cdcx, cdstyle, cdtable, cdlabel, cdmax, includeFile

   newArgs = .array~new(2)
   if Arg(1, 'E') then newArgs[1] = Arg(1)
   if Arg(9, 'E') then newArgs[2] = Arg(9)
   forward class (super) arguments (newArgs) continue
   if result <> 0 then return result

   self~catalog = .directory~new
   self~catalog['count'] = 0
   self~catalog['page'] = .directory~new
   self~catalog['page']['font'] = ""
   self~catalog['page']['fsize'] = 0
   self~catalog['page']['style'] = ""
   self~catalog['page']['btnwidth'] = 0
   self~catalog['page']['leftbtntext'] = "&Backward"
   self~catalog['page']['rightbtntext'] = "&Forward"
   self~catalog['page']['expected'] = 200

   if Arg(1,'o') = 1 then self~UseStem = 0; else self~UseStem = 1
   if Arg(2,'o') = 1 then self~catx = 10; else self~catX = cdx
   if Arg(3,'o') = 1 then self~caty = 4; else self~caty = cdy
   if Arg(4,'o') = 1 then self~catcx = 0; else self~catcx = cdcx
   if Arg(5,'o') = 1 then self~catstyle = ""; else self~catstyle = cdstyle~translate
   if Arg(6,'o') = 1 then cdtable = ""
   if Arg(7,"o") = 1 then self~catlabel = "Page:"; else self~catlabel = cdlabel
   if Arg(8,"o") = 1 then self~catmax = 0; else self~catmax = cdmax

   if cdtable \= '' then
   do
      self~catalog['names'] = .array~new(10)
      parse var cdtable name ret
      i = 0
      do while name \= ""
         i = i + 1
         self~catalog['names'][i] = name
         parse var ret name ret
      end
      self~catalog['count'] = i
   end

   self~InitCategories()    /* user overwrite */

   if cdtable = '' then
   do
      if self~catalog['count'] = 0 then
      do
         i = 0
         do ar over self~catalog['names']
            i = i +1
         end
         self~catalog['count'] = i
      end
   end

   if self~catstyle~wordpos("WIZZARD") > 0 then
   do
      self~Wizard = 1
      self~catstyle = self~catstyle~changestr("WIZZARD","")
   end
   else if self~catstyle~wordpos("WIZARD") > 0 then
   do
      self~Wizard = 1
      self~catstyle = self~catstyle~changestr("WIZARD","")
   end
   else self~Wizard = 0

   self~catalog['id'] = .array~new(self~catalog['count'],100)
   self~catalog['base'] = .array~new(self~catalog['count'])
   self~catalog['handles'] = .array~new(self~catalog['count'])
   self~staticID = 9600

   return 0


::method StartIt   /* don't call parent startit */
   if self~DlgHandle = 0 then return 0
   self~InitDialog
   return self~DlgHandle

::method DefineDialog protected
   if self~baseptr = 0 then return 0
   if self~catstyle~wordpos('DROPDOWN') = 0 then
      do i = 9001 to (9000 + self~catalog['count'])
         self~ConnectControl(i, "CHANGEPAGE")
      end
   else self~ConnectList(9001, "CHANGEPAGE")

   do i = 1 to self~catalog['count']
      catnames.i = self~catalog['names'][i]
   end

   self~catalog['category'] = 0
   n = self~catstyle~wordpos("TOPLINE")
   if n > 0 then self~catmax = 1
   m = self~catstyle~wordpos("DROPDOWN")

   if (n > 0 | m > 0) & self~catstyle~wordpos('NOBORDER') = 0 then self~catstyle = self~catstyle||" NOBORDER"

   if self~catalog['page']['h'] = .Nil then self~catalog['page']['h'] = self~SizeY - 40

   size = self~getTextSizeDlg("Hg")
   textHeight = size~height

   if self~catcx = 0 & n = 0 then  /* dropdown or none */
   do
      do i = 1 to self~catalog['count']
         if self~catalog['page']['font'] <> "" then
              size = self~getTextSizeDlg(catnames.i,self~catalog['page']['font'],self~catalog['page']['fsize'])
         else
              size = self~getTextSizeDlg(catnames.i)
         self~catcx = max(self~catcx,size~width)
      end
      if n = 0 then self~catcx = self~catcx + 20
   end

   if n > 0 | m > 0 then do  /* topline or dropdown */
      if self~catalog['page']['x'] = .Nil then self~catalog['page']['x'] = 1
      if m > 0 then do
        if self~catalog['page']['y'] = .Nil then self~catalog['page']['y'] = self~caty*2+textHeight+4
      end
      else if self~catalog['page']['y'] = .Nil then self~catalog['page']['y'] = self~caty+textHeight+5

      if self~catalog['page']['w'] = .Nil then self~catalog['page']['w'] = self~SizeX - 2
   end
   else do
      if self~catmax = 0 then do
        if self~catalog['page']['x'] = .Nil then self~catalog['page']['x'] = self~catX+10+self~catcx
      end
      else
        if self~catalog['page']['x'] = .Nil then self~catalog['page']['x'] = self~catX+10+self~catcx * ((self~catalog['count']-1)%self~catmax+1)

      if self~catalog['page']['y'] = .Nil then self~catalog['page']['y'] = 5

      if self~catalog['page']['w'] = .Nil then self~catalog['page']['w'] = self~SizeX - (self~catX+self~catcx+20)
   end

   self~CategoryPage

   if m > 0 then
   do
      if self~catmax = 0 then self~catmax = 5
          if self~catalog['count'] < self~catmax then self~catmax = self~catalog['count']
      if self~catmax < self~catalog['count'] then
           if self~catlabel='' then
                self~AddComboBox(9001,,self~catX,self~caty,self~catcx,(self~catmax+1)*(textHeight+4),,
                                 self~catstyle~delword(m, 1) || " CAT VSCROLL")
           else self~AddComboInput(9001,, self~catX, self~caty,, self~catcx, self~catmax,,
                                   self~catlabel, self~catstyle~delword(m, 1) || " CAT VSCROLL")
      else if self~catlabel='' then
                self~AddComboBox(9001,,self~catX,self~caty,self~catcx,(self~catmax+1)*(textHeight+4),,
                                 self~catstyle~delword(m, 1) || " CAT")
           else self~AddComboInput(9001,, self~catX, self~caty,, self~catcx, self~catmax,,
                                   self~catlabel, self~catstyle~delword(m, 1) || " CAT")
   end
   else do
      if (n >0) then newstyle = self~catstyle~delword(n, 1)
                else newstyle = self~catstyle
      if self~catalog['page']['font'] <> "" then
         self~AddRadioStem(9001, self~catX, self~caty, self~catcx, catnames., self~catmax,,
                           newstyle || " CAT",,self~catalog['page']['font'],self~catalog['page']['fsize'])
      else
         self~AddRadioStem(9001, self~catX, self~caty, self~catcx, catnames., self~catmax, newstyle || " CAT")
   end

   self~DlgHandle = UsrCreateDialog(self~Adm, "PARENT", self~DialogItemCount, 0, self~BasePtr, self~AutoDetect, 1, 0, 0)
   self~activePtr = 0
   if self~DlgHandle = 0 then return 0

   do i = 1 to self~catalog['count']
      self~DialogItemCount = 0
      self~catalog['category'] = i
      p = self~catalog['page']
      self~CreateCategoryDialog(p['x'], p['y'], p['w'], p['h'],p['font'], p['fsize'], p['style'] || " CHILD HIDDEN", p['expected'])
      msg = "self~" || self~catalog['names'][i]~space(0)
      interpret(msg)
      ret = UsrCreateDialog(self~Adm, "CHILD", self~DialogItemCount, self~catalog['base'][i], self~DlgHandle, i)
      self~activePtr = 0
      self~catalog['handles'][i] = ret
   end

   /* This method returns the current page */

::method CurrentCategory unguarded
   return self~catalog['category']

   /* This method adds controls to the base window of a 'CategoryDialog' */

::method CategoryPage protected

   size = self~getTextSizeDlg("Hg")
   textHeight = size~height

   if self~catstyle~wordpos("TOPLINE") > 0 then
      self~addBlackFrame(0, 2, self~SizeX, textHeight + (self~caty+2))
   else if self~catstyle~wordpos("DROPDOWN") > 0 then
      self~addBlackFrame(0, 2, self~SizeX, textHeight + (self~caty*2)+1)

   lbtext = self~catalog['page']['leftbtntext']
   rbtext = self~catalog['page']['rightbtntext']
   if lbtext~pos(" ") > 0 then lbtext = '"' || lbtext || '"'
   if rbtext~pos(" ") > 0 then rbtext = '"' || rbtext || '"'
   if (self~catalog['page']['btnwidth'] > 0) then
   do
      self~AddButtonGroup(self~SizeX-self~catalog['page']['btnwidth']*2-15,,
         self~SizeY - 15,self~catalog['page']['btnwidth'],, "&Ok 1 OK &Cancel 2 CANCEL", 1, "FIX DEFAULT")
      if self~Wizard = 1 then
         self~AddButtonGroup(5,self~SizeY - 15,self~catalog['page']['btnwidth'],,
                             ,lbtext" 11 PreviousPage" rbtext" 12 NextPage", 1, "FIX")
   end
   else do
      self~AddOkCancelRightBottom
      if self~Wizard = 1 then
         self~AddButtonGroup(5, self~SizeY - 15,,, lbtext" 11 PreviousPage" rbtext" 12 NextPage", 1, "FIX")
   end
   self~addBlackFrame(0, self~SizeY - 20, self~SizeX, 1)

   /* */

::method CreateCategoryDialog protected
   use arg x, y, cx, cy, fontName, fontSize, options = "", expected = 200

   if expected > 1 then expected = 200
   options = options~translate

   if arg(8, "O") | fontName == "" then fontName = self~fontName
   if arg(9, "O") | fontSize = 0 then fontSize = self~fontSize

   ret = UsrDefineDialog(x, y, cx, cy, "", "", fontName, fontSize,,
                                                   options, expected)

   parse var ret base Ptr
   self~catalog['base'][self~catalog['category']] = base
   self~activePtr = Ptr
   return Ptr

   /* */

::method InitDialog protected
   /* set the mark to the first radio button */
   if self~catstyle~wordpos('DROPDOWN') > 0 then
   do
      do i = 1 to self~catalog['count']
         self~AddComboEntry(9001, self~catalog['names'][i])
      end
      self~SetComboLine(9001, self~catalog['names'][1])
   end
   else self~SendMessageToItem(9001,"0x000000F1",1,0)
   if self~Wizard = 1 then self~DisableItem(11)

   /* call InitXXX for each category if such a method exists */
   do i = 1 to self~catalog['count']
      self~catalog['category'] = i
      msg = "Init" || self~catalog['names'][i]~space(0)
      if self~HasMethod(msg) = 1 then interpret("self~"||msg)   /* does user class provide the Init method? If so call it */
   end

   /* activate first page */
   self~catalog['category'] = 1
   self~ShowWindow(self~catalog['handles'][self~catalog['category']])

   /* */

::method InitCategories protected
   return

   /* */

::method GetDefaultOpts protected
   use arg options
   if self~catalog['category'] = 0 then
   do
      if Arg(1,"o") = 1 | options = "OPTIONS" | options = .Nil then options = "FIX";
      else do
          if options~wordpos("FIX") = 0 then options = options || " FIX"
      end
   end
   else if Arg(1,"o") = 1 | options = "OPTIONS" | options = .Nil then options = ""
   return options~translate

   /* This method returs the currently selected page. Return value 1 means */
   /* the first page is selected. */

::method GetSelectedPage unguarded
   if self~catstyle~wordpos('DROPDOWN') = 0 then
   do
      np = 9001
      do while self~GetRadioButton(np) = 0 & np < (9001 + self~catalog['count'])
         np = np + 1
      end
   end
   else do
      cur = self~GetComboLine(9001)
      np = 1
      do while np <= ( self~catalog['count']) & cur \= self~catalog['names'][np]
         np = np + 1
      end
          if np > self~catalog['count'] then return 0
          np = np + 9000
   end
   return np-9000

   /* 'ChangePage' switch to the page 'NewPage' and shows it. */
   /* If 'NewPage is omitted the currently selected page will be activated - */
   /* (if not allready done). */


::method ChangePage unguarded
   use arg NewPage

   oldPage = self~catalog['category']
   /* if ChangePage is called automatically, 2 arg are passed (WParam and LParam) */
   if Arg(2,'o') = 0 | NewPage > 9000 then NewPage = self~GetSelectedPage
   else do
      if self~catstyle~wordpos("DROPDOWN") = 0 then do
          /* SetRadioButton only works for registered radio buttons */
          /* uncheck the old radio button and check the new one */
          self~SendMessageToItem(9000+OldPage,"0x000000F1",0,0)
          self~SendMessageToItem(9000+NewPage,"0x000000F1",1,0)
      end
      else self~SetCurrentComboIndex(9001, NewPage)
   end
   if (NewPage = self~catalog['category']) |,
      (NewPage < 1 | NewPage> self~catalog['count']) then return 0
   self~HideWindowFast(self~catalog['handles'][self~catalog['category']])
   self~catalog['category'] = NewPage
   if self~Wizard = 1 then
   do
      if NewPage = 1 then
      do
         self~DisableItem(11)
         self~EnableItem(12)
      end; else
      if NewPage = self~catalog['count'] then
      do
         self~EnableItem(11)
         self~DisableItem(12)
      end; else
      do
         self~EnableItem(11)
         self~EnableItem(12)
      end
   end
   self~ShowWindow(self~catalog['handles'][self~catalog['category']])

   self~PageHasChanged(oldPage, NewPage)
   return NewPage


::method PageHasChanged unguarded
   use arg Oldpage, NewPage
   return

::method PreviousPage unguarded
   oldPage = self~CurrentCategory
   if oldPage > 1 then self~ChangePage(oldPage-1)

::method NextPage unguarded
   oldPage = self~CurrentCategory
   if oldPage < self~catalog['count'] then self~ChangePage(oldPage+1)

  /* The following methods */

::method ConnectEntryLine
   use arg id, fname
   forward message "AddAttribute" continue
   if result = -1 then return -1              /* new id in result */
   return DataTable(self~Adm,"ADD",result,0, self~catalog['category']);

::method ConnectComboBox
   use arg id, fname, opts
   forward message "AddAttribute" continue
   id = result; if result = -1 then return -1              /* new id in result */
   if opts~translate~wordpos("LIST") = 0 then type = 0; else type = 5
   return DataTable(self~Adm,"ADD",id,type, self~catalog['category']);

::method ConnectCheckBox
   use arg id, fname
   forward message "AddAttribute" continue
   if result = -1 then return -1              /* new id in result */
   return DataTable(self~Adm,"ADD",result,1, self~catalog['category']);

::method ConnectRadioButton
   use arg id, fname
   forward message "AddAttribute" continue
   if result = -1 then return -1              /* new id in result */
   return DataTable(self~Adm,"ADD",result,2, self~catalog['category']);

::method ConnectListBox
   use arg id, fname
   forward message "AddAttribute" continue
   if result = -1 then return -1              /* new id in result */
   return DataTable(self~Adm,"ADD",result,3, self~catalog['category']);

::method ConnectMultiListBox
   use arg id, fname
   forward message "AddAttribute" continue
   if result = -1 then return -1              /* new id in result */
   return DataTable(self~Adm,"ADD",result,4, self~catalog['category']);

::method ConnectSeparator
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return DataTable(self~Adm,"ADD",id,9, self~catalog['category']);

::method BoxMessage unguarded private
   use arg category, id, msg, wP, lP
   if category~Datatype("N") = 0 then category = self~catalog['category']  /* assume current page is addressed */
   if category > 9000 then forward class (super)
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return SendWinMsg("PTR", self~catalog['handles'][category], id, msg, wP, lP)

::method AddCategoryComboEntry unguarded
   use arg id, data, category
   if Arg(2,"o") = 1 then return -1
   return self~BoxMessage(category, id, 0x00000143, 0, "T" || data)

::method InsertCategoryComboEntry unguarded
   use arg id, item, data, category
   if Arg(3,"o") = 1 then return -1
   if Arg(2,"o") = 1 then item = self~GetCurrentCategoryComboIndex(id, category)
   return self~BoxMessage(category, id, 0x0000014A, item-1, "T" || data)

/* don't use string but the index */
::method DeleteCategoryComboEntry unguarded
   use arg id, index, category
   if Arg(2,"o") = 1 then index = self~GetCurrentCategoryComboIndex(id, category)
   return self~BoxMessage(category, id, 0x00000144, index-1, 0)

::method FindCategoryComboEntry unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | data = "DATA" then return -1
   item = self~BoxMessage(category, id, 0x0000014C, 0, "T" || data)
   if item > 0 then return item + 1; else return 0

::method GetCurrentCategoryComboIndex unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x00000147, 0, 0)+1

::method SetCurrentCategoryComboIndex unguarded
   use arg id, ndx, category
   if arg(2,'o') = 1 then ndx = 0
   return self~BoxMessage(category, id, 0x0000014E, ndx-1, 0)

::method GetCategoryComboItems unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x00000146, 0, 0)

::method GetCategoryComboEntry unguarded
   use arg id, ndx, category
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   len = SendWinMsg("DLG", self~catalog['handles'][category], id, 0x00000149, ndx-1,0)
   if len <= 0 then return ""
   else return SendWinMsg("PTR", self~catalog['handles'][category], id, 0x00000148, ndx-1,"G" || len+1)

::method ChangeCategoryComboEntry unguarded
   use arg id, item, data, category
   if Arg(2,"o") = 1 then item = self~GetCurrentCategoryComboIndex(id, category)
   if item <= 0 then return -1
   self~DeleteCategoryComboEntry(id, item, category)
   return self~InsertCategoryComboEntry(id, item, data, category)

::method CategoryComboAddDirectory unguarded
   use arg id, drvpath, fattr, category
   READWRITE  = 0
   READONLY   = 1
   HIDDEN     = 2
   SYSTEM     = 4
   DIRECTORY  = 16
   ARCHIVE    = 32
   interpret( "opts = "fattr~translate('+',' ') )
   return self~BoxMessage(category, id, 0x00000145, opts, "T" || drvpath)

::method CategoryComboDrop unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x0000014B, 0, 0)


::method AddCategoryListEntry unguarded
   use arg id, data, category
   if Arg(2,"o") = 1 then return -1
   return self~BoxMessage(category, id, 0x00000180, 0, "T" || data)

::method InsertCategoryListEntry unguarded
   use arg id, item, data, category
   if Arg(3,"o") = 1 then return -1
   if Arg(2,"o") = 1 then item = self~GetCurrentCategoryListIndex(id, category)
   return self~BoxMessage(category, id, 0x00000181, item-1, "T" || data)

/* don't use string but the index */
::method DeleteCategoryListEntry unguarded
   use arg id, index, category
   if Arg(2,"o") = 1 then index = self~GetCurrentCategoryListIndex(id, category)
   return self~BoxMessage(category, id, 0x00000182, index-1, 0)

::method FindCategoryListEntry unguarded
   use arg id, data, category
   if Arg(2,"o") = 1 then return -1
   item = self~BoxMessage(category, id, 0x0000018F, 0, "T" || data)
   if item > 0 then return item + 1; else return 0

::method GetCurrentCategoryListIndex unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x00000188, 0, 0) + 1

::method SetCurrentCategoryListIndex unguarded
   use arg id, ndx, category
   if arg(2,'o') = 1 then ndx = 0
   return self~BoxMessage(category, id, 0x00000186, ndx-1, 0)

::method GetCategoryListItems unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x0000018B, 0, 0)

::method GetCategoryListEntry unguarded
   use arg id, ndx, category
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   len = SendWinMsg("DLG", self~catalog['handles'][category], id, 0x0000018A, ndx-1,0)
   if len <= 0 then return ""
   else return SendWinMsg("PTR", self~catalog['handles'][category], id, 0x00000189, ndx-1,"G" || len+1)

::method ChangeCategoryListEntry unguarded
   use arg id, item, data, category
   if Arg(2,"o") = 1 then item = self~GetCurrentCategoryListIndex(id, category)
   if item <= 0 then return -1
   self~DeleteCategoryListEntry(id, item, category)
   return self~InsertCategoryListEntry(id, item, data, category)

::method SetCategoryListTabulators unguarded
   if arg() < 3 then return 1
   id = arg(1)
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   cs = self~catalog['handles'][arg(arg())]", "id
   do i=2 to arg()-1
      cs = cs", "arg(i)
   end
   interpret("call SetLBTabStops "cs)


::method CategoryListAddDirectory unguarded
   use arg id, drvpath, fattr, category
   READWRITE  = 0
   READONLY   = 1
   HIDDEN     = 2
   SYSTEM     = 4
   DIRECTORY  = 16
   ARCHIVE    = 32
   interpret( "opts = "fattr~translate('+',' ') )
   return self~BoxMessage(category, id, 0x0000018D, opts, "T" || drvpath)


::method CategoryListDrop unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x00000184, 0, 0)


::method GetCategoryEntryLine unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category], 0)

::method GetCategoryListLine unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category], 3)

::method GetCategoryComboLine unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category], 0)

::method GetCategoryCheckBox unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category], 1)

::method GetCategoryRadioButton unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category], 2)

::method GetCategoryMultiList unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category], 4)

::method GetCategoryValue unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalGetItemData(id, self~catalog['handles'][category])

::method GetCategoryAttrib unguarded
   use arg aname, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   i = self~DataConnection~First
   do while (i \= .NIL)
      if self~DataConnection[i] = aname then
      do
         s = "self~"aname"= self~GetValue("i", category)"
         interpret(s)
         return
      end
      i = self~DataConnection~Next(i)
   end


::method SetCategoryStaticText unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 0)

::method SetCategoryEntryLine unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 0)

::method SetCategoryListLine unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 3)

::method SetCategoryComboLine unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 0)

::method SetCategoryCheckBox unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 1)

::method SetCategoryRadioButton unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 2)

::method SetCategoryMultiList unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category], 4)

::method SetCategoryValue unguarded
   use arg id, data, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return self~InternalSetItemData(id, data, self~catalog['handles'][category])

::method SetCategoryAttrib unguarded
   use arg aname, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   i = self~DataConnection~First
   do while (i \= .NIL)
      if self~DataConnection[i] = aname then
      do
         s = "self~SetCategoryValue("i", self~"aname", category)"
         interpret(s)
         return
      end
      i = self~DataConnection~Next(i)
   end


::method EnableCategoryItem unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   h = self~GetItem(id, self~catalog['handles'][category])
   if h = 0 then return 1
   return Wnd_Desktop("ENABLE", h, 1)

::method DisableCategoryItem unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   h = self~GetItem(id, self~catalog['handles'][category])
   if h = 0 then return 1
   return Wnd_Desktop("ENABLE",h, 0)

::method HideCategoryItem unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   h = self~GetItem(id, self~catalog['handles'][category])
   if h = 0 then return 1
   return WndShow_Pos("S",h, "HIDE")

::method ShowCategoryItem unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   h = self~GetItem(id, self~catalog['handles'][category])
   if h = 0 then return 1
   return WndShow_Pos("S",h, "NORMAL")

::method GetItem unguarded
   use arg id, hDlg
   if Arg(2,'o') = 1 then hDlg = self~DlgHandle
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return 0
   hnd = HandleDlg("ITEM", id, hDlg)
   if hnd = 0 & Arg(2,'o') = 1 then do
       hnd = HandleDlg("ITEM", id, self~catalog['handles'][self~catalog['category']])  /* try current page */
       if hnd = 0 then do i = 1 to self~catalog['count'] while hnd = 0   /* try all pages */
            hnd = HandleDlg("ITEM", id, self~catalog['handles'][i])
       end
   end
   return hnd

::method SendMessageToCategoryItem unguarded
   parse arg id,a2,a3,a4, category
   if Arg(5,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   return SendWinMsg("DLG", self~catalog['handles'][category],id,a2,a3,a4)

::method SetCategoryItemFont unguarded
   use arg id, hFont, bRedraw, category     /* WM_SETFONT */
   if Arg(4,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   if Arg(3, 'o') = 1 then bRedraw = 1
   return SendWinMsg("ANY",self~GetItem(id, self~catalog['handles'][category]), "0x0030", hFont, bRedraw)

::method FocusCategoryItem unguarded
   use arg id, category
   if Arg(2,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   return SendWinMsg("ANY",self~catalog['handles'][category], "0x0028", self~GetItem(id, self~catalog['handles'][category]), 1)

::method ResizeCategoryItem unguarded
   use arg id, width, high, showOptions, category
   if Arg(5,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   if Arg(4,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   hnd = self~GetItem(id, self~catalog['handles'][category])
   if hnd = 0 then return -1
   return WndShow_Pos("P",hnd, 0, 0, width * self~FactorX, high* self~FactorY,,
                           "NOMOVE "||showOptions)

   /* This method will move a dialog item to an other position */

::method MoveCategoryItem unguarded
   use arg id, xPos, yPos, showOptions, category
   if Arg(5,'o') = 1 | category~DataType("N") = 0 then category = self~catalog['category']  /* try current */
   if Arg(4,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   h = self~GetItem(id, self~catalog['handles'][category])
   if h = 0 then return -1
   return WndShow_Pos("P",h, xPos * self~FactorX, yPos * self~FactorY, 0, 0,,
                           "NOSIZE "||showOptions)


/* set/get the horizontal scroll width of a list box */

::method SetCategoryListWidth unguarded
   use arg id, dlgunits, category
   if Arg(2,'o') = 1 | dlgunits~datatype("N") = 0 then return -1
   return self~BoxMessage(category, id, 0x00000194, dlgunits*self~FactorX, 0)

::method GetCategoryListWidth unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x00000193, 0, 0)/self~FactorX

::method SetCategoryListColumnWidth unguarded
   use arg id, dlgunits, category
   if Arg(2,'o') = 1 | dlgunits~datatype("N") = 0 then return -1
   return self~BoxMessage(category, id, 0x00000195, dlgunits*self~FactorX, 0)

::method SetCategoryListItemHeight unguarded
   use arg id, dlgunits, category
   if Arg(2,'o') = 1 | dlgunits~datatype("N") = 0 then return -1
   return self~BoxMessage(category, id, 0x000001A0, 0, dlgunits*self~FactorY)

::method GetCategoryListItemHeight unguarded
   use arg id, category
   return self~BoxMessage(category, id, 0x000001A1, 0, 0)/self~FactorY


/* These methods are necessary for Win32 controls when used in a cetegory dialog */
::method ConnectTreeControl
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1                /* current category */
   return DataTable(self~Adm,"ADD", result,6, self~catalog['category']);         /* new id in result  6 == Tree Type */

::method ConnectListControl
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1                /* current category */
   return DataTable(self~Adm,"ADD",result,7, self~catalog['category']);         /* new id in result  7 == List Type */

::method ConnectSliderControl
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1                /* current category */
   return DataTable(self~Adm,"ADD",result,8, self~catalog['category']);         /* new id in result  8 == Track Bar Type */

::method ConnectTabControl
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1                /* current category */
   return DataTable(self~Adm,"ADD",result,9, self~catalog['category']);         /* new id in result  9 == Tab Type */
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2012 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/*                                                                            */
/* Animated button class                                                      */
/*                                                                            */
/******************************************************************************/

::class 'AnimatedButton' public

::method ParentDlg attribute
::method stopped attribute

::method init
     expose spritewin spos. sprite.

     if ARG() > 2 then
        use arg bid, bmpfrom, bmpto, movx, movy, bmpsizex, bmpsizey, dely, ix, iy, dlg
     else do
        use arg istring, dlg
        parse var istring bid "," bmpfrom"," bmpto"," movx"," movy"," bmpsizex"," bmpsizey"," dely"," ix"," iy
     end

     self~stopped = 0
     if bmpfrom~hasmethod("HASINDEX") then
         sprite.from = bmpfrom
     else do
         if bmpfrom~datatype("N") = 1 & bmpto \= 0 then sprite.from = bmpfrom
         else sprite.from = .local[bmpfrom]
     end
     sprite.to = bmpto
     sprite.movex = movx
     sprite.movey = movy
     sprite.sizex = bmpsizex
     sprite.sizey = bmpsizey
     sprite.delay = dely
     sprite.buttonid = bid
     sprite.smooth = 1
     sprite.step = 1

     self~ParentDlg = dlg
     spritewin = self~ParentDlg~GetItem(sprite.buttonid)
     parse value self~ParentDlg~GetWindowRect(spritewin) with spos.left spos.top spos.right spos.bottom

     spos.bottom = spos.bottom - spos.top
     spos.right = spos.right - spos.left
     spos.top = 0
     spos.left = 0

     spos.x = ix
     spos.y = iy


::method movepos unguarded
     expose spritewin spos. sprite.
     use arg move.x, move.y
     oldpos.x = spos.x
     oldpos.y = spos.y
     reset.x = 0
     reset.y = 0
     ss.x = 0
     ss.y = 0
     sr.x = sprite.sizex
     sr.y = sprite.sizey
     if sprite.smooth \= 1 then
     do
        ss.x = sprite.sizex
        ss.y = sprite.sizey
        sr.x = 0
        sr.y = 0
     end

     if (move.x > 0) then
     do
       if (spos.x + move.x + ss.x) >= spos.right then reset.x = 1
       else spos.x = spos.x + move.x
     end
     else if (move.x < 0) then do
       if (spos.x + move.x + sr.x) <= spos.left then reset.x = 1
       else spos.x = spos.x + move.x
     end

     if (move.y > 0) then
     do
        if (spos.y + move.y + ss.y) >= spos.bottom then reset.y = 1
        else spos.y = spos.y + move.y
     end
     else if (move.y < 0) then do
        if (spos.y + move.y + sr.y*(sprite.to\=0) ) <= spos.top then reset.y = 1
        else spos.y = spos.y + move.y
     end

     if self~ParentDlg~finished = 1 then return 1

     if (reset.x + reset.y) > 0 then do
        if reset.x = 1 then do
           if (move.x > 0) then
           do
              if self~hitright = 1 then spos.x = spos.left - sr.x
           end
           else if (move.x < 0) then do
              if self~hitleft = 1 then spos.x = spos.right - ss.x
           end
        end
        if reset.y = 1 then do
           if (move.y > 0) then
           do
              if self~hitbottom = 1 then spos.y = spos.top - sr.y*(sprite.to\=0)
           end
           else if (move.y < 0) then do
              if self~hittop = 1 then spos.y = spos.bottom - ss.y
           end
        end
        if self~ParentDlg~finished = 1 then return 1
        self~ParentDlg~ChangeBitmapButton(sprite.buttonid, 0)
        /* self~ParentDlg~ClearRect(spritewin, oldpos.x, oldpos.y, (oldpos.x + sprite.sizex), (oldpos.y + sprite.sizey)) */
     end
     if self~ParentDlg~finished = 1 then return 1
     self~ParentDlg~DisplaceBitmap(sprite.buttonid, spos.x, spos.y);


::method setpos unguarded
     expose spritewin spos. sprite.
     use arg new.x, new.y

     if self~ParentDlg~finished = 1 then return 1
     self~ParentDlg~ChangeBitmapButton(sprite.buttonid, 0)
     self~ParentDlg~ClearRect(spritewin, spos.x, spos.y, (spos.x + sprite.sizex), (spos.y + sprite.sizey))
     spos.x = new.x
     spos.y = new.y
     self~ParentDlg~DisplaceBitmap(sprite.buttonid, spos.x, spos.y);

::method getpos
     expose spos.
     use arg mypos.
     mypos.x = spos.x
     mypos.y = spos.y


::method moveseq unguarded
     expose spritewin spos. sprite.

     if sprite.to > 0 then
     do
        opts = ''
        do actsprite = sprite.from by sprite.step to sprite.to
           if (sprite.movex \= 0) | (sprite.movey \=0) then
              self~movepos(sprite.movex, sprite.movey)
           if self~ParentDlg~finished = 1 then return 1
           self~ParentDlg~ChangeBitmapButton(sprite.buttonid, actsprite,,,,opts)
           if (sprite.delay \= 0) then ret = msSleep(sprite.delay)
        end
     end
     else do
        opts = "INMEMORY"
        do actsprite over sprite.from
           if (sprite.movex \= 0) | (sprite.movey \=0) then
              self~movepos(sprite.movex, sprite.movey)

           if self~ParentDlg~finished = 1 then return 1
           self~ParentDlg~ChangeBitmapButton(sprite.buttonid, actsprite,,,,opts)
           if (sprite.delay \= 0) then ret = msSleep(sprite.delay)
        end
     end


::method moveto unguarded
     expose spos. sprite.
     use arg newx, newy

     if (sprite.movex > 0) then mx = 1; else mx = 0
     if (sprite.movey > 0) then my = 1; else my = 0
     if (mx = 1) & (my = 1) then
     do until (spos.x > newx) & (spos.y > newy)
        self~moveseq
     end
     else if (mx = 0) & (my = 0) then
     do until (spos.x < newx) & (spos.y < newy)
        self~moveseq
     end
     else if (mx = 1) & (my = 0) then
     do until (spos.x > newx) & (spos.y < newy)
        self~moveseq
     end
     else if (mx = 0) & (my = 1) then
     do until (spos.x < newx) & (spos.y > newy)
        self~moveseq
     end


::method run unguarded
   do until(self~stopped = 1) | (self~ParentStopped) | self~ParentDlg~finished = 1
      self~moveseq
   end
   return 0

::method Parentstopped
   return self~ParentDlg~finished

::method stop
   self~stopped = 1

::method setfromto
   expose sprite.
   use arg from, ito
   sprite.from = from
   sprite.to = ito

::method setmove
   expose sprite.
   use arg xx,yy
   sprite.movex = xx
   sprite.movey = yy

::method setdelay
   expose sprite.
   use arg da
   sprite.delay = da

::method setsmooth
   expose sprite.
   use arg sm
   sprite.smooth = sm

::method setstep
   expose sprite.
   use arg st
   sprite.step = st

::method getsprite
   expose sprite.
   use arg osprite.
   do k over sprite.
      osprite.k = sprite.k
   end

::method setsprite
   expose sprite.
   use arg isprite.
   do k over isprite.
      sprite.k = isprite.k
   end

::method hitright unguarded
   return 1

::method hitleft unguarded
   return 1

::method hitbottom unguarded
   return 1

::method hittop unguarded
   return 1
/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 2012 Rexx Language Association. All rights reserved.         */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                                         */
/*                                                                            */
/* Redistribution and use in source and binary forms, with or                 */
/* without modification, are permitted provided that the following            */
/* conditions are met:                                                        */
/*                                                                            */
/* Redistributions of source code must retain the above copyright             */
/* notice, this list of conditions and the following disclaimer.              */
/* Redistributions in binary form must reproduce the above copyright          */
/* notice, this list of conditions and the following disclaimer in            */
/* the documentation and/or other materials provided with the distribution.   */
/*                                                                            */
/* Neither the name of Rexx Language Association nor the names                */
/* of its contributors may be used to endorse or promote products             */
/* derived from this software without specific prior written permission.      */
/*                                                                            */
/* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS        */
/* "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          */
/* LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS          */
/* FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT   */
/* OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,      */
/* SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED   */
/* TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA,        */
/* OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY     */
/* OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING    */
/* NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS         */
/* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.               */
/*                                                                            */
/*----------------------------------------------------------------------------*/
/******************************************************************************/
/*                                                                            */
/*       Windows Dialog Interface for Object REXX                             */
/* Standard Dialog Classes                                                    */
/*                                                                            */
/******************************************************************************/
/* ========================================================================= */
::class 'DlgArea' public
/* ========================================================================= */
/*  define an area of your dialog to place controls on                       */
/*                                                                           */
/*  parms x, y, width, height [,margin]                                      */
/*                                                                           */
/*  x & y take offset as an argument,                                        */
/*  a -ve argument is from the right/bottom                                  */
/*                                                                           */
/*  methods & attributes                                                     */
/*  x()        offset from left (right if -ve)                               */
/*  y()        offset from top  (bottom if -ve)                              */
/*  w()        width                                                         */
/*  h()        height                                                        */
/*  wr         width remaining                                               */
/*  hr         height remaining                                              */
/*  l          left    }                                                     */
/*  t          top     }  Dimensions                                         */
/*  b          bottom  }  within margins                                     */
/*  r          right   }                                                     */
/*  move()     Specify new x,y coordinates                                   */
/*  resize()   Specify new width and height                                  */
/*                                                                           */
/*                                                                           */
/*  methods                                                                  */
/*  x & y  can take an offset argument which may be absolute or a percentage */
/*  w & h  can take a percentage as argument                                 */
/*         a parm of R to W or H means the remainder of the width or height  */
/*                                                                           */
/*  so to add a button 80% across your margined dialog for the remainder     */
/*  a = .DlgArea~new(10,10,150,110,5)                                        */
/*  self~AddButton(a~x(80%),10,a~w(20%),a~h(10%),'Text','Method')            */
/*    or not using percentages is equivalent to                              */
/*  self~AddButton(a~x(112),10,a~w * 0.2,a~h(10),'Text','Method')            */
/*    or not using the object would be equivalent to                         */
/*  self~AddButton(117,10,28,10,'Text','Method')                             */
/*                                                                           */
/* ------------------------------------------------------------------------- */
/*               :                                  ^                        */
/*              top                                 :                        */
/*              <v.......width.............>     bottom                      */
/*              +--------------------------+   ^    :                        */
/* ......left..>|                       r  |   :    :                        */
/*              |                       v  |   :    :                        */
/*              |  +--------------------+  |   :    :                        */
/*              |  |+.x.>             ^ |  |   :    :                        */
/*              |  |:                 : |  |   :    :                        */
/*              |  |y                 : |  | height :                        */
/*              |  |:                 h |  |   :    :                        */
/*              |  |v                 : |  |   :    :                        */
/*              |  |<........w........+>|  |   :    :                        */
/*              |  |                  v |  |   :    :                        */
/*             ^|b>+--------------------+  |   :    :                        */
/*             m|                          |   :    :                        */
/*             v+--------------------------+   v    v                        */
/*              <m>                                                          */
/*.....right...............................>                                 */
/*                                                                           */
/*                                                                           */
/* ------------------------------------------------------------------------- */

/* ------------------------------------------------------------------------- */
::method Init
/* ------------------------------------------------------------------------- */
arg left,top,width,height,margin

if margin~datatype('n')
then self~Margin = margin~trunc
else self~margin = max(min(height % 7,5),1)

if top~datatype('n')
then Self~top       = top~trunc
else Self~top       = 10

if left~datatype('n')
then Self~left      = left~trunc
else Self~left      = 10

if width~datatype('n')
then Self~width     = width~trunc
else Self~width     = 100

if height~datatype('n')
then Self~height    = height~trunc
else Self~height    = 100


self~T = self~Top  + self~Margin
self~L = self~left + Self~Margin
self~B = self~top  + self~height - self~margin
self~R = self~Left + Self~Width  - self~Margin

self~LastX = 0
self~LastY = 0

/* ------------------------------------------------------------------------- */
::method X
/* ------------------------------------------------------------------------- */
arg offset
self~LastX='0'||Offset

if offset~pos('%') > 0
then do
   offset = offset~changestr('%','')
   if offset~datatype('n')
   then do
      if offset < 0 then offset = 100 + offset          /* from right margin */
      return trunc(self~left + self~margin + (self~w * (offset / 100)))
   end
   else return self~left + self~margin
end
else if offset~datatype('n')
     then if offset >= 0
          then return trunc(self~left + self~margin + offset) /* x from left */
          else return trunc(self~left + self~width - self~margin + offset) /* right */
else return self~left + self~margin

/* ------------------------------------------------------------------------- */
::method Y
/* ------------------------------------------------------------------------- */
arg offset
self~LastY = '0'||offset

if offset~pos('%') > 0
then do
   offset = offset~changestr('%','')
   if offset~datatype('n')
   then do
      if offset < 0 then offset = 100 + offset          /* from right margin */
      return trunc(self~top + self~margin + (self~h * (offset / 100)))
   end
   else return self~top + self~margin
end
else if offset~datatype('n')
     then if offset >= 0
          then return trunc(self~top + self~margin + offset)
          else return trunc(self~top + self~height - self~margin + offset)
else return self~top + self~margin

/* ------------------------------------------------------------------------- */
::method H
/* ------------------------------------------------------------------------- */
arg pc

if pc = 'R' then return self~hr                       /* height is remainder */

h = self~height - (2 * self~margin)

if pc~pos('%') > 0
then return trunc(h * (pc~changestr('%','') / 100))
else return trunc(h)

/* ------------------------------------------------------------------------- */
::method HR
/* ------------------------------------------------------------------------- */

if self~LastY~pos('%') = 0                                 /* Y was absolute */
then return self~Top + Self~Height - Self~Margin - Self~LastY
else return self~h(100 - self~LastY~changestr('%','')||'%')       /* Y was % */

/* ------------------------------------------------------------------------- */
::method W
/* ------------------------------------------------------------------------- */
arg pc

if pc = 'R' then return self~wr                       /* width  is remainder */

w = self~width - (2 * self~margin)

if pc~pos('%') > 0
then return trunc(w * (pc~changestr('%','') / 100))
else return w

/* ------------------------------------------------------------------------- */
::method WR
/* ------------------------------------------------------------------------- */

if self~LastX~pos('%') = 0                                 /* X was absolute */
then return self~Left + Self~Width - Self~Margin - Self~LastX
else return self~w(100 - self~LastX~changestr('%','')||'%')       /* X was % */

/* ----------------------- compatibility methods --------------------------- */
::method cx    ; return self~w(arg(1))         -- Compatibility version of w
::method cy    ; return self~h(arg(1))         -- Compatibility version of h
::method sizeX ; return self~width
::method sizeY ; return self~height
/* ------------------------------------------------------------------------- */
::method T              Attribute
::method L              Attribute
::method B              Attribute
::method R              Attribute
::method Top            Attribute
::method Left           Attribute
::method Width          Attribute
::method Height         Attribute
::method Margin         Attribute
::method LastX          Attribute Private
::method LastY          Attribute Private

/* ========================================================================= */
::Class 'DlgAreaU' SubClass DlgArea Public -- DlgAreaUniversal - Whole of Dialog
/* ========================================================================= */
/*  define the Area of the calling Dialog                                    */
/*                                                                           */
/*  parms callingDialog [,margin]                                            */
/*                                                                           */
/*  methods & attributes                                                     */
/*  resize     Aids in Dynamically Resizing Dialogs - See Documentation      */
/*  CorrectionFactor                                - See Documentation      */
/*                                                                           */
::method Init
/* ------------------------------------------------------------------------- */
Expose DlgObjList

use arg DlgObj,Margin,NoResize,NoMove

if NoResize~String \= 'a Set' then NoResize = .Set~new
self~NoResize = NoResize
if NoMove~String   \= 'a Set' then NoMove   = .Set~new
self~NoMove   = NoMove

if \margin~datatype('n') then margin = 5

Self~CorrectionFactor = 1.05

self~init:super(0,0,DlgObj~SizeX,DlgObj~SizeY,margin)

self~OriginalWidth  = DlgObj~SizeX
self~OriginalHeight = DlgObj~SizeY
self~LastError      = .nil

                                  /* Analyze the callers DefineDialog Method */
                                       /* Remove comments prefixed by --     */
DDSourceArr=DlgObj~class~method('DefineDialog')~source
DDSBuff=.mutableBuffer~new
do line over DDSourceArr
   parse var line line '--' .       -- strip out this style inline comments
   DDSBuff~Append(' '||line)
end /* DO */
DDSource = DDSBuff~String ; Drop DDSBuff
                                       /* remove comments within /*  */      */
do forever
   commEnd = DDsource~pos('*'||'/')
   if commEnd = 0 then leave
   commStart = DDSource~SubStr(1,CommEnd-1)~lastPos('/'||'*')
   if comStart=0
   then do
      self~LastError='Error in DlgAreaU Class: Unbalanced comments in DefineDialog class of' Dlg
      leave
   end /* DO */

   parse var DDSource front =(commStart) . =(commEnd) . +2 back
   DDSource = front back
end /* DO */

DDSource1 = DDSource           /* copy of de-commented DefineDialog source   */

                               /* re-create DlgArea objects within scope     */
do forever
   parse upper var DDSource1 front '.DLGAREA'DAMsg'('DAparms')'DDSource1
   do while DAParms~Countstr('(') > DAParms~CountStr(')')
      parse var DDSource1 fragment ')' DDSource1
      DAParms = DAParms||')'||fragment
   end /* DO */
   if DAParms = '' then leave          /* we have got them all!              */

                                     /* parse out the refferer variable name */
   parse value front~reverse with . '='DAVar .
   DAVar = DAVar~reverse~strip
   DAObj = DAVar
   sourceline = DAVar'=.DLGAREA'||DAMsg||'('||DAParms||')'
   select
      when DAMsg~Space(0) = 'U~NEW'  /* This is the DlgAreaU (self) obj def  */
         then interpret DAVar'=.DlgArea~new(0,0,'||self~w||','||self~h||','||self~margin||')'
      when DAMsg~Space(0) = '~NEW'   /* This is a DlgArea Obj definition     */
         then interpret DAVar'=.DlgArea~new('daparms')'
      otherwise
         self~LastError='DlgAreaU Class: Unable to parse:' SourceLine
   end /* select */
end

                                    /* Now we parse out resizable widgets    */
DlgObjList=.List~new
delim = '00'x

signal on syntax name parseerror
Parseerroroccured = 1 /* provisionally */

do forever
   parse upper var DDSource front 'ADD'ObjName'('ObjParms')'DDSource1
   do while ObjParms~Countstr('(') > ObjParms~CountStr(')')
      parse var DDSource1 fragment ')' DDSource1
      ObjParms = ObjParms||')' fragment
   end /* DO */

   if ObjParms = '' then leave
   if front~space(0)~right(5)='SELF~'
   then do
      sourcelinetxt = 'SELF~ADD'||Objname||'('||Objparms||')'
      DDSource=DDSource1
      select
         when 'BITMAPBUTTON BUTTON PROGRESSBAR SCROLLBAR'~Wordpos(ObjName~strip) >0
            then parse var ObjParms ObjId','ObjX','ObjY','ObjW','ObjH','.
         when 'CHECKBOX COMBOBOX ENTRYLINE LISTBOX LISTCONTROL PASSWORDLINE RADIOBUTTON SLIDERCONTROL TABCONTROL'~Wordpos(ObjName~strip) > 0
            then parse var ObjParms ObjId','.','ObjX','ObjY','ObjW','ObjH','.
         when 'BLACKFRAME BLACKRECT GRAYRECT GRAYFRAME TREECONTROL WHITEFRAME WHITERECT'~Wordpos(ObjName~strip) > 0
            then parse var ObjParms ObjX','ObjY','ObjW','ObjH','.','ObjId','.
         when 'GROUPBOX TEXT'~Wordpos(ObjName~strip) > 0
            then parse var ObjParms ObjX','ObjY','ObjW','ObjH','.','.','ObjId','.
         otherwise
            ObjId = ''                    /* This type is on the ignore list */
      end /* select */
      if ObjId \= ''
      then interpret 'DlgObjList~Insert(ObjId "@"' ObjX '"@"' ObjY '"@"' ObjW '"@"' ObjH ')'
   end /* DO */
   else parse var DDSource 'ADD'DDSource            /* skip past this add */
end /* DO */

Parseerroroccured = 0
parseerror:
signal off syntax

if parseerroroccured then self~lastError = 'DlgAreaU class could not parse' sourcelinetxt

/* ------------------------------- ignore list -----------------------------
ATTRIBUTE AUTOSTARTMETHOD BUTTONGROUP CATEGORYCOMBOENTRY CATEGORYLISTENTRY
CHECKBOXSTEM CHECKGROUP COMBOENTRY COMBOINPUT FULLSEQ INPUT INPUTGROUP
INPUTSTEM LISTENTRY MENUITEM MENUSEPERATOR OKCANCELLEFTBOTTOM OKCANCELLEFTTOP
OKCANCELRIGHTBOTTOM OKCANCELREGHTTOP POPUPMENU RADIOGROUP RADIOSTEM ROW
SEQUENCE STYLE USERMSG
   ------------------------------------------------------------------------- */
/* ------------------------------------------------------------------------- */
::method Resize
/* ------------------------------------------------------------------------- */
/* this method needs to be called as follows:                                */
/* in your dialog init method place this line after call to the superclass   */
/* self~connectResize('OnResize')                                            */
/* the method resize would contain the following                             */
/* expose u /* our topmost dlgAreaClass exposed in definedialog */           */
/* use arg dummy, sizeinfo    /* sizeinfo contains information about the     */
/*                                                  new width and height */  */
/* if self~PeekDialogMessage~left(8)\="OnResize" then u~resize(self,sizeinfo)*/
/* ------------------------------------------------------------------------- */
expose DlgObjList
  use arg dlg,data
  dlg~SizeX = .DlgUtil~loWord(data) % dlg~FactorX
  dlg~SizeY = .DlgUtil~hiWord(data) % dlg~FactorY

  wFactor   = (dlg~SizeX / self~OriginalWidth ) * Self~CorrectionFactor
  hFactor   = (dlg~SizeY / self~OriginalHeight) * Self~CorrectionFactor


  do dlgObjDef over DlgObjList
     parse var dlgObjdef DOid'@'DOx'@'DOy'@'DOw'@'DOh
     if \Self~NoResize~HasIndex(DOid~strip)
     then dlg~ResizeItem(Doid,DOw * wFactor,DOh * hFactor,"NOREDRAW")
     if \Self~NoMove~HasIndex(DOid)
     then dlg~MoveItem(DOid,DOx * wFactor,DOy * hFactor,"NOREDRAW")
  end /* DO */
  dlg~update
/* ------------------------------------------------------------------------- */

::method OriginalWidth    Attribute Private
::method OriginalHeight   Attribute Private
::method NoResize         Attribute
::method NoMove           Attribute
::method CorrectionFactor Attribute
::method LastError        Attribute
