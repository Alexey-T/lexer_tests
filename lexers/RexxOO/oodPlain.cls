/******************************************************************************/
/*                                                                            */
/*               oodPlain.CLS - OODialog Class Definition File                */
/*               Windows Dialog Interface for Open Object REXX                */
/*                                                                            */
/******************************************************************************/


::requires 'oodialog' LIBRARY

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
/* Public routines                                                            */
/*                                                                            */
/******************************************************************************/

/* Routine play() will search for a sound file in the PATH and play it */
::routine play public
   use arg fileName, asyn = "NO"

   if Arg(1, 'o') then return StopSoundFile()

   file = SysSearchPath("SOUNDPATH",fileName)
   if file = "" then return 1

   if asyn~translate = "LOOP" then return PlaySoundFileInLoop(file)
   else return PlaySoundFile(file, asyn)


::routine infoDialog public
   use arg info
   return InfoMessage(info)


::routine errorDialog public
   use arg info
   return ErrorMessage(info)


::routine askDialog public
   use arg info, defaultButton
   if arg(2,'e') then do
       defaultButton = defaultButton~left(1)~translate
       if defaultButton <> 'N', defaultButton <> 'Y' then do
           raise syntax 40.904 array('AskDialog', '2', '[Yes, No]', arg(2))
       end
   end
   if arg(2,'o') then return YesNoMessage(info)
   else return YesNoMessage(info, defaultButton)


::routine fileNameDialog public
   use arg selfile = "", parentwnd = 0, filter, load, title, defextension = "TXT", amulti, sepchar = ' '

   if load == 0 | load~left(1)~translate = "S" then load = 0; else load = 1
   if load == 1 & amulti~left(1)~translate = "M" then amulti = 1; else amulti = 0

   if arg() < 3 then return GetFileNameWindow(selfile, parentwnd)
   else if Arg(3, 'O') then return GetFileNameWindow(selfile, parentwnd, arg(3), load, arg(5), defextension, amulti, sepchar)
   else return GetFileNameWindow(selfile, parentwnd, arg(3) || '0'x, load, arg(5), defextension, amulti, sepchar)


::routine findWindow public
   use arg info
   return FindTheWindow(info)

::routine msSleep public
   use strict arg ms
   return SleepMS(ms)

::routine screenSize public
   parse value GetScreenSize() with dx dy px py
   return .array~of(dx,dy,px,py)

::routine systemMetrics public
   use strict arg index
   return .DlgUtil~getSystemMetrics(index)


/*-------------------------------- shortcut routines ---------------------------------*/


::routine TimedMessage public
   use arg msg, title, duration, earlyReply = .false
   if \ duration~datatype('W') then return -1
   if \ earlyReply~datatype('O') then earlyReply = .false
   dlg = .TimedMessage~new(msg,title,duration, earlyReply)
   dlg~execute
   if duration < 0 then return dlg
   if \ earlyReply then drop dlg
   return 0

::routine InputBox public
   use arg msg, title, default, size
   dlg = .InputBox~new(msg,title,default,size)
   ret = dlg~execute
   drop dlg
   return ret

::routine IntegerBox public
   use arg msg, title, default, size
   dlg = .IntegerBox~new(msg,title,default,size)
   ret = dlg~execute
   drop dlg
   return ret

::routine PasswordBox public
   use arg msg, title, default, size
   dlg = .Passwordbox~new(msg,title,default,size)
   ret = dlg~execute
   drop dlg
   return ret

::routine MultiInputBox public
   use arg msg, title, labels, datas, len
   if Arg(5,"o") = 1 then len = 0
   num = labels~items
   do i=1 to num
      j = i+100
      lab.i = labels[i]
      dat.j = datas[i]
   end
   dlg = .MultiInputBox~new(msg, title, lab., dat., len)
   if dlg~execute \= 1 then do; drop dlg; return .NIL; end
   ret = .array~new(num)
   do i=1 to num
      attr = labels[i]~changestr(' ','')~changestr('&','')
      Interpret 'ret[i] = dlg~'attr
   end
   drop dlg
   return ret

::routine ListChoice public
   use arg msg, title, list, lx, ly, predev
   if Arg(4,"o") = 1 then lx = 0
   if Arg(5,"o") = 1 then ly = 0
   if Arg(6,"o") = 1 then predev = ""
   num = list~items
   do i=1 to num
      lst.i = list[i]
   end
   dlg = .ListChoice~new(msg, title, lst., lx, ly, predev)
   res = dlg~execute
   drop dlg
   if res = '' then return .nil
   else return res

::routine MultiListChoice public
   use arg msg, title, list, lx, ly, predev
   if Arg(4,"o") = 1 then lx = 0
   if Arg(5,"o") = 1 then ly = 0
   if Arg(6,"o") = 1 then predev = ""
   num = list~items
   do i=1 to num
      lst.i = list[i]
   end
   dlg = .MultiListChoice~new(msg, title, lst., lx, ly, predev)
   res = dlg~execute
   if res = '' then do; drop dlg; return .NIL; end
   ret = .array~new
   do i=1 to words(res)
      ret[i] = list[word(res,i)]
   end
   drop dlg
   return ret

::routine CheckList public
   use arg msg, title, labels, checks, lx = 0, max = 0
   if Arg(4,"o") then checks = .array~new
   num = labels~items
   do i=1 to num
      j = i+100
      lab.i = labels[i]
      if checks~hasIndex(i) then do
         if checks[i]~datatype('O') then dat.j = checks[i]
         else dat.j = 0
      end
      else dat.j = 0
   end
   dlg = .CheckList~new(msg, title, lab., dat., lx, max)
   if dlg~execute \= 1 then do; drop dlg; return .NIL; end
   ret = .array~new
   do j=101 to 100+num
      ret[j-100] = dat.j
   end
   drop dlg
   return ret

::routine SingleSelection public
   use arg msg, title, labels, data, lx, max
   if Arg(5,"o") = 1 then lx = 0
   if Arg(6,"o") = 1 then max= 0
   num = labels~items
   do i=1 to num
      lab.i = labels[i]
   end
   dlg = .SingleSelection~new(msg, title, lab., data, lx, max)
   res = dlg~execute
   drop dlg
   return res
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
/*                                                                            */
/******************************************************************************/



::class 'DlgUtil' public
::method init class external "LIBRARY oodialog dlgutil_init_cls"
::method version class external "LIBRARY oodialog dlgutil_version_cls"
::method comctl32Version class external "LIBRARY oodialog dlgutil_comctl32Version_cls"
::method hiWord class external "LIBRARY oodialog dlgutil_hiWord_cls"
::method loWord class external "LIBRARY oodialog dlgutil_loWord_cls"
::method and class external "LIBRARY oodialog dlgutil_and_cls"
::method or class external "LIBRARY oodialog dlgutil_or_cls"
::method getSystemMetrics class external "LIBRARY oodialog dlgutil_getSystemMetrics_cls"
::method handleToPointer class external "LIBRARY oodialog dlgutil_handleToPointer_cls"
::method test class external "LIBRARY oodialog dlgutil_test_cls"

-- Needed to maintain backward compatibilty with 3.2.0.  The external function GetSysMetrics()
-- was foolishly documented.
::routine GetSysMetrics public
  use strict arg index
  return .DlgUtil~getSystemMetrics(index)

::class 'Rect' public
::method init external "LIBRARY oodialog rect_init"
::method left external "LIBRARY oodialog rect_left"
::method 'left=' external "LIBRARY oodialog rect_setLeft"
::method top external "LIBRARY oodialog rect_top"
::method 'top=' external "LIBRARY oodialog rect_setTop"
::method right external "LIBRARY oodialog rect_right"
::method 'right=' external "LIBRARY oodialog rect_setRight"
::method bottom external "LIBRARY oodialog rect_bottom"
::method 'bottom=' external "LIBRARY oodialog rect_setBottom"

::class 'Point' public
::method init external "LIBRARY oodialog point_init"
::method x external "LIBRARY oodialog point_x"
::method 'x=' external "LIBRARY oodialog point_setX"
::method y external "LIBRARY oodialog point_y"
::method 'y=' external "LIBRARY oodialog point_setY"

::class 'Size' public
::method init external "LIBRARY oodialog size_init"
::method width external "LIBRARY oodialog size_cx"
::method 'width=' external "LIBRARY oodialog size_setCX"
::method height external "LIBRARY oodialog point_y"
::method 'height=' external "LIBRARY oodialog point_setY"

/******************************************************************************/
/* The class WindowBase implements methods that are common to all windows,    */
/* if it is a dialog or a dialog item                                         */
/******************************************************************************/

::class 'WindowBase' public MIXINCLASS object

::method Hwnd attribute
::method InitCode attribute        /* attribute to get the result of init */

    /* Size and Dialog Unit Factor */
::method SizeX attribute
::method SizeY attribute
::method FactorX attribute
::method FactorY attribute

::method Unknown unguarded
   use arg msgname
   call errorDialog msgname || " is not a method of " || self

::method init_WindowBase
   use arg hwnd

   self~SizeX = 0
   self~SizeY = 0
   parse value GetDialogFactor() with x y
   self~FactorX=x
   self~FactorY=Y

   if arg(1,'o') then self~Hwnd = 0
   else do
       self~hwnd = hwnd
       parse value Wnd_Desktop("RECT", hwnd) with x y cx cy
       self~SizeX = cx % self~FactorX
       self~SizeY = cy % self~FactorY
   end

   return 0


::method AssignWindow unguarded
   use arg hwnd
   self~Hwnd = hwnd
   parse value Wnd_Desktop("RECT", hwnd) with x y cx cy
   self~SizeX = cx % self~FactorX
   self~SizeY = cy % self~FactorY


::method Enable unguarded
   return Wnd_Desktop("ENABLE",self~Hwnd, 1)

::method Disable unguarded
   return Wnd_Desktop("ENABLE",self~Hwnd, 0)

::method Hide unguarded
   return WndShow_Pos("S", self~Hwnd, "HIDE")

::method Show unguarded
   return WndShow_Pos("S",self~Hwnd, "NORMAL")

::method IsVisible unguarded
   return WinAPI32Func("G", "WNDSTATE", "V", self~hwnd)

::method IsEnabled unguarded
   return WinAPI32Func("G", "WNDSTATE", "E", self~hwnd)

::method Resize unguarded
   use arg width, hight, showOptions
   if Arg(3,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   return WndShow_Pos("P",self~hwnd,0,0,width*self~FactorX,hight*self~FactorY,"NOMOVE "||showOptions )

::method Move unguarded
   use arg xPos, yPos, showOptions
   if Arg(3,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   return WndShow_Pos("P",self~hwnd, xPos*self~FactorX,yPos*self~FactorY,0,0,"NOSIZE "||showOptions)

::method GetSize unguarded
   parse value Wnd_Desktop("RECT",self~Hwnd) with x y cx cy
   return (cx - x) % self~FactorX || " " || (cy - y) % self~FactorY

::method GetPos unguarded
   parse value Wnd_Desktop("RECT",self~Hwnd) with x y cx cy
   return (x%self~FactorX) || " " || (y % self~FactorY)

::method "Title=" unguarded
   use arg text
   call Wnd_Desktop "SETTXT", self~hwnd, text

::method SetTitle unguarded
   use arg text
   call Wnd_Desktop "SETTXT", self~hwnd, text

::method Title unguarded
   return Wnd_Desktop("TXT", self~hwnd)

::method GetID unguarded
   return Wnd_Desktop("ID", self~hwnd)

::method getStyleRaw unguarded external "LIBRARY oodialog wb_getStyleRaw"
::method getExStyleRaw unguarded external "LIBRARY oodialog wb_getExStyleRaw"

-- DEPRECATED
::method getSystemMetrics unguarded
   use strict arg index
   return .DlgUtil~getSystemMetrics(index)


::class 'ResourceUtils' public mixinclass object

/* Directory object containing symbolic constants */
::attribute constDir get
::attribute constDir set private

::attribute processingLoad private    -- in LoadItems ?

::method ParseIncludeFile
  use strict arg hFile

  -- Revisit. Should an error msg box be put up if the file is not found?
  hFile = SysSearchPath("PATH", hFile)
  if hFile == "" then return 1

  f = .stream~new(hFile)
  f~open(read)

  do while f~state == "READY"
    line = f~linein~strip('L')

    if line~abbrev("#ifdef") then do
      self~skipThroughIfDefs(f, line)
      iterate
    end

    if line~abbrev("#define") & line~words == 3 then do
      parse var line def symbol numericID .
      if numericID~datatype('W') then self~ConstDir[symbol~translate] = numericID
    end
  end
  f~close
  return 0

::method skipThroughIfDefs private
  use arg fObj, line

  if \ line~abbrev("#ifdef") & \ line~abbrev("#ifndef") then return
  ifDefStack = .queue~new
  ifDefStack~push(line)
  do while fObj~state == "READY"
    l = fObj~linein~strip("L")
    select
      when l~abbrev("#endif") then ifDefStack~pull
      when l~abbrev("#ifdef") then ifDefStack~push(l)
      when l~abbrev("#ifndef") then ifDefStack~push(l)
      otherwise nop
    end
    if ifDefStack~items == 0 then leave
  end

::method Checkfile private
   use arg f
   if (f~lines = 0) then do
      f~close
      self~ProcessingLoad = 0
      ret = errorDialog("Error reading resource file!" f)
      return 1
   end
   return 0

::method Errorfile private
   use arg f, s
   f~close
   self~ProcessingLoad = 0
   ret = errorDialog("Error reading resource file:" f "("s")")

::method CheckId
  use strict arg id

  if id~datatype("W") then return id

  if \ self~ConstDir~isA(.directory) then return self~idError(id)

  if self~ConstDir[id~space(0)~translate] = .nil then return self~idError(id)

  return self~ResolveSymbolicId(id)

::method idError private
  use strict arg id
  j = errorDialog("Adding dialog resource:" id "undefined" || '0d0a'x || -
                   "non-numeric identification number.")
  return -1

::method ResolveSymbolicId unguarded
   use strict arg id
   id = self~ConstDir[id~space(0)~translate]
   if id == .nil then return -1
   return id

/**
 * Return a symbolic ID from the ConstDir that matches the numeric ID.
 * The docs will advise users of ooDialog to use unique numbers for all
 * resources.
 */
::method ResolveNumericID unguarded
  use arg numericID
  if \ numericID~datatype('W') then return -1
  do symbolicID over self~ConstDir~allIndexes
    if self~ConstDir[symbolicID] == numericID then return symbolicID
  end
  return -1

::method resolveIconID private
  use strict arg icon

  if \ icon~Datatype('W') then icon = self~ResolveSymbolicId(icon)
  if icon = -1 then icon = 0
  else if icon >= 1, icon <= 4 then icon += 10
  return icon

::method resolveResourceID unguarded
  use strict arg id
  if id~datatype("W"), id > -1 then return id
  return self~ResolveSymbolicId(id)

::method mergeSymbols
  use strict arg otherSymbolSrc

  select
    when otherSymbolSrc~isA(.directory) then do
      if self~constDir == otherSymbolSrc then return 0
      self~constDir~putAll(otherSymbolSrc)
      otherSymbolSrc~putAll(self~constDir)
    end

    when otherSymbolSrc~isA(.ResourceUtils) then do
      if self~constDir == otherSymbolSrc~constDir then return 0
      self~constDir~putAll(otherSymbolSrc~constDir)
      otherSymbolSrc~constDir~putAll(self~constDir)
    end

    otherwise return -1
  end
  -- End select
  return 0

/******************************************************************************/
/* The class PlainBaseDialog implements methods that are common to a dialog   */
/* by Resource Workshop (ResDialog) and to a dialog created by using Add...   */
/* methods (UserDialog).                                                      */
/******************************************************************************/

::class 'PlainBaseDialog' public inherit WindowBase ResourceUtils

::method init class external "LIBRARY oodialog pbdlg_init_cls"
::method setDefaultFont class external "LIBRARY oodialog pbdlg_setDefaultFont_cls"
::method getFontName class external "LIBRARY oodialog pbdlg_getFontName_cls"
::method getFontSize class external "LIBRARY oodialog pbdlg_getFontSize_cls"


::method Adm attribute protected   /* external adminstration buffer */
::method AutoDetect attribute      /* Automatic data field detection on/off */
::method DataConnection attribute protected /* store connections between ids and class attributes */
::method UseStem attribute protected   /* flag whether to use a stem to set/get data or not */
::method DlgData private
   expose DlgData.
   return DlgData.

::method Finished attribute
::method ParentDlg attribute protected

::attribute fontName get
::attribute fontName set private

::attribute fontSize get
::attribute fontSize set private

  /* A queue containing the methods that will be started concurrently before */
  /* execution of the dialog */
::method AutomaticMethods attribute protected

::method ChildDialogs attribute

/** PlainBaseDialog::init()  Performs the necessary intialization, including the
 * set up of the dialog management block, for all dialogs.
 */
::method Init protected
   expose Library Resource DlgData.
   use arg Library, Resource, DlgData., includeFile

   self~Adm = 0
   self~ParentDlg = .Nil

   /* Initialize the WindowBase */
   self~initCode = self~init_WindowBase

   /* prepare the dialog management for a new dialog */
   self~Adm = HandleDialogAdmin()  /* allocate a new dialog table */
   if self~Adm = 0 then do
      self~Deinstall
      self~InitCode = 1
      return 1
   end

   /* call methods to get dialog settings */
   self~InitAutoDetection
   /* create array storing connection between ids and attributes */
   self~DataConnection = .array~new(50)
   /* there was no stem argument so don't use stems */
   if Arg(3, 'o') = 1 then self~UseStem = 0; else self~UseStem = 1
   self~finished = 0
   self~AutomaticMethods = .queue~new    /* create a new queue */
   self~AddUserMsg("OK", 0x00000111, 0xFFFFFFFF, 1, 0xFFFFFFFF, 0, 0)
   self~AddUserMsg("Cancel", 0x00000111, 0xFFFFFFFF, 2, 0xFFFFFFFF, 0, 0)
   self~AddUserMsg("Help", 0x00000111, 0xFFFFFFFF, 9, 0xFFFFFFFF, 0, 0)
   self~ProcessingLoad = 0
   self~DlgHandle = 0
   self~ChildDialogs = .list~new

   self~ConstDir = .directory~new
   self~ConstDir["IDOK"] = 1
   self~ConstDir["IDCANCEL"] = 2
   self~ConstDir["IDHELP"] = 9

   -- Common in Microsoft compatible resource script files
   self~ConstDir["IDC_STATIC"] = -1

   -- ooDialog supplied resources, defined in oodResources.h
   self~ConstDir["IDI_DLG_OODIALOG"] = 11
   self~ConstDir["IDI_DLG_APPICON"] = 12
   self~ConstDir["IDI_DLG_APPICON2"] = 13
   self~ConstDir["IDI_DLG_OOREXX"] = 14
   self~ConstDir["IDI_DLG_DEFAULT"] = 12

   self~fontName = .PlainBaseDialog~getFontName
   self~fontSize = .PlainBaseDialog~getFontSize

   if arg(4, 'E') then
     self~ParseIncludeFile(includeFile)

   return 0

::method DlgHandle unguarded
   return self~hwnd

::method "DlgHandle=" unguarded
   use arg hwnd
   self~hwnd=hwnd

   /* This method will be called after the Windows dialog has been created. */
   /* It is useful to set data fields and initialize combo & list boxes */

::method InitDialog unguarded protected
   return 0

   /* This method will dispatch messages from dialog until OK or Cancel */
   /* push button has been pushed or ESC or Enter has been presed */

::method Run unguarded protected
   use strict arg sleeptime = 1
   if \ sleepTime~datatype('W') then sleepTime = 1

   do while self~finished = 0
      self~HandleMessages(sleeptime)
   end

   /* this method will create the dialog, show it (see method show), start */
   /* automatic methods and destroy the dialog. Data will be set and */
   /* received */

::method Execute unguarded
   expose DlgData.
   use strict arg sh = "NORMAL", icon = 0, sleeptime = 1, nomodal = 0
   icon = self~resolveIconId(icon)
   if \ sleeptime~Datatype('W') then sleeptime = 1
   if self~startit(icon, nomodal) \= 0 then
   do
      if self~UseStem = 1 then self~SetDataStem(DlgData.); else self~SetData
      self~show(sh)
      do while self~AutomaticMethods~Items > 0
         p = self~AutomaticMethods~pull
         m = self~AutomaticMethods~pull
         c = self~AutomaticMethods~pull
         o = c~new(p, self)
         o~start(m)
      end
      self~run(sleeptime)
      if self~InitCode = 1 then do
         if (self~UseStem = 1) then self~GetDataStem(DlgData.)
             self~GetData
      end
      if self~Adm \= 0 then self~stopit
      return self~InitCode
   end
   else self~stopit
   return 0

   /* This method deletes the Windows dialog */

::method Stopit
   use arg caller
   if self~DlgHandle = 0 | self~Adm = 0 then return -1
   self~Leaving
   ret =  HandleDlg("STOP", self~DlgHandle)
   self~DlgHandle = 0
   self~Adm = 0

   /* we ourselves are a child dialog and not called by the parent's stopit */
   if self~ParentDlg \= .Nil & caller \= "PARENT" then self~ParentDlg~ChildDied(self)

   /* if we have child dialogs, stop all of them */
   if self~ChildDialogs \= .Nil then do
       if self~ChildDialogs~items > 0 then
       do child over self~ChildDialogs
          child~Stopit("PARENT")
       end
       self~ChildDialogs = .Nil
   end
   return ret


   /* remove a child dialog from the chain */
::method ChildDied
   use arg child
   if self~ChildDialogs \= .Nil then do
       ndx = self~ChildDialogs~first
       do while ndx \= .Nil
          if self~ChildDialogs[ndx] == child then do
              self~ChildDialogs~remove(ndx)
              ndx = .Nil
          end
          else ndx = self~ChildDialogs~next(ndx)
       end
   end


   /* This method returns the handle of the current dialog */

::method Get unguarded
   return HandleDlg("HNDL")

::method GetSelf unguarded
   return HandleDlg("HNDL", self~Adm)


   /* This method returns the handle of a dialog item */

::method GetItem unguarded
   use arg id, hDlg
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return 0
   if Arg(2,'o') = 1 then hDlg = self~DlgHandle
   return HandleDlg("ITEM",id, hDlg)

::method GetControlID unguarded
   use strict arg hWnd
   return WinAPI32Func("G", "ID", hWnd)

   /* This method shows the dialog */

::method Show unguarded
   use arg option = "NORMAL"
   if option = "" then option = "NORMAL"; else option = option~translate
   if option~wordpos("SHOWTOP") > 0 then self~ToTheTop
   return WndShow_Pos("S",self~DlgHandle, option)


   /* Use this method to make the dialog the front dialog */

::method ToTheTop unguarded
   return Wnd_Desktop("TOP",self~DlgHandle)


::method Minimize unguarded
   return WndShow_Pos("S",self~DlgHandle, "MIN")

::method Maximize unguarded
   return WndShow_Pos("S",self~DlgHandle, "MAX")

::method IsMaximized unguarded
   return WinAPI32Func("G", "WNDSTATE", "Z", self~DlgHandle)

::method IsMinimized unguarded
   return WinAPI32Func("G", "WNDSTATE", "I", self~DlgHandle)

::method Restore unguarded
   return WndShow_Pos("S",self~DlgHandle, "RESTORE")

-- Send the DM_REPOSITION  message to the dialog.
::method EnsureVisible unguarded
   return SendWinMsg("ANY", self~DlgHandle, "0x0402", 0, 0)


-- Connect WM_HELP messages (F1) to a method.
::method ConnectHelp
   use strict arg msgToRise
   return self~addUserMsg(msgToRise, "0x00000053", "0xFFFFFFFF", 0, 0, 0, 0, 0x00000101)

/* The following methods create a connection between a class method */
/* and a dialog item (dialog control). The connected method will be */
/* called whenever the dialog item will be activated (i.g. a push */
/* button will be pushed) */

   /* Connect a push button with a method */

::method ConnectButton
   use arg id, msgToRise
   if Arg(2, 'o') = 1 then msgToRise = ""
   if id~DataType("N") = 0 then do
       if self~ProcessingLoad = 1 then msgToRise = id
       id = self~ResolveSymbolicId(id)
       if id = -1 then return -1
   end
   if id < 3 | id = 9 then return 0
   return self~AddUserMsg(msgToRise, 0x00000111, 0xFFFFFFFF, id, 0xFFFFFFFF, 0, 0)


   /* Connect any dialog control with a method */

::method ConnectControl
   use arg id, msgToRise
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return self~AddUserMsg(msgToRise, 0x00000111, 0xFFFFFFFF, id, 0x0000FFFF, 0,0)


   /* Connect a list with a method */

::method ConnectList
   use arg id, msgToRise
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return self~AddUserMsg(msgToRise, 0x00000111, 0xFFFFFFFF, '0x0001'||id~d2x(4), 0xFFFFFFFF, 0, 0)

   /* Connect a list left double-click with a method */

::method ConnectListLeftDoubleClick
   use arg id, msgToRise
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return self~AddUserMsg(msgToRise, 0x00000111, 0xFFFFFFFF, '0x0002'||id~d2x(4), 0xFFFFFFFF, 0, 0)


   /* Connect any Windows message with a class method */

::method AddUserMsg protected unguarded
   use arg msgToRise, msgWindows, filt1, wP, filt2, lP, filt3, tag
   if Arg(3, 'o') = 1 | filt1 = "FILT1" then filt1 = 0xFFFFFFFF
   if Arg(5, 'o') = 1 | filt2 = "FILT2" then filt2 = 0
   if Arg(7, 'o') = 1 | filt3 = "FILT3" then filt3 = 0
   if Arg(4, 'o') = 1 | wP = "WP" then wP = 0
   if Arg(6, 'o') = 1 | lP = "LP" then lP = 0
   if msgToRise~space(0) = "" then return 1
   if Arg(8, 'o') = 1 then return AddUserMessage(self~Adm, msgWindows, filt1, wP, filt2, lP, filt3, msgToRise)
   else return AddUserMessage(self~Adm, msgWindows, filt1, wP, filt2, lP, filt3, msgToRise, tag)

::method ConnectFKeyPress unguarded
   use strict arg msgToRaise
   if \ msgToRaise~isA(.String) then return -1
   return WinAPI32Func("K", 'C', self~adm, self~dlgHandle, msgToRaise~translate, "FKEYS", "NONE")

::method ConnectKeyPress unguarded
   use strict arg msgToRaise, keys, filter = ""
   if \ msgToRaise~isA(.String) | \ keys~isA(.String) | \ filter~isA(.String) then return -1
   if filter == "" then return WinAPI32Func("K", 'C', self~adm, self~dlgHandle, msgToRaise~translate, keys~space(0))
   return WinAPI32Func("K", 'C', self~adm, self~dlgHandle, msgToRaise~translate, keys~space(0), filter~translate)

::method DisconnectKeyPress unguarded
   use arg msgToRaise
   if arg(1, 'O') then return WinAPI32Func("K", 'R', self~adm)
   if \ msgToRaise~isA(.String) then return -1
   return WinAPI32Func("K", 'R', self~adm, self~dlgHandle, msgToRaise~translate)

::method HasKeyPressConnection unguarded
   use arg msgToRaise
   if arg(1, 'O') then ret = WinAPI32Func("K", 'Q', self~adm)
   else do
      if \ msgToRaise~isA(.String) then ret = .false
      else ret = WinAPI32Func("K", 'Q', self~adm, msgToRaise~translate)
   end
   if \ ret~datatype('O') then return .false
   else return ret

/* The following methods are to connect an item of the data stem with */
/* a data field of the dialog. The data stem will start with number 1 */
/* and the dialog fields will be assigned in the same order they are  */
/* called */

::method ConnectEntryLine
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1
   return DataTable(self~Adm,"ADD", result,0);                /* new id in result */

::method ConnectComboBox
   use arg id, attributeName, opts
   forward message "AddAttribute" continue
   id = result; if id = -1 then return -1              /* new id in result */
   if opts~translate~wordpos("LIST") = 0 then type = 0; else type = 5
   return DataTable(self~Adm,"ADD",id,type);

::method ConnectCheckBox
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1
   return DataTable(self~Adm,"ADD",result,1);                /* new id in result */

::method ConnectRadioButton
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1
   return DataTable(self~Adm,"ADD",result,2);                /* new id in result */

::method ConnectListBox
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1
   return DataTable(self~Adm,"ADD",result,3);                /* new id in result */

::method ConnectMultiListBox
   use arg id, attributeName
   forward message "AddAttribute" continue
   if result = -1 then return -1
   return DataTable(self~Adm,"ADD",result,4);                /* new id in result */

   /* Use this method to separate two radiobutton groups */

::method ConnectSeparator
   use arg id
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return DataTable(self~Adm,"ADD",id,9);

   /* This method adds an attribute to the object. The attribute is associated */
   /* with the windows control 'id' */

::method AddAttribute protected
   use arg id, attributeName
   if Arg(2,'o') = 1 then attributeName = "DATA"id~space(0)
   if id~DataType("N") = 0 then do
       if self~ProcessingLoad = 1 then attributeName = id
       id = self~ResolveSymbolicId(id)
   end
   if id < 1 then return -1
   if attributeName~space(0) = "" | self~HasMethod(attributeName) = 1 then
       attributeName = "DATA"id~space(0)
   else do
      attributeName = attributeName~space(0)~changestr('&','')~changestr(':','') /* remove blanks, &, and : */
      if attributeName~datatype('V')=0 then attributeName = "DATA"id~space(0)
   end

   self~DataConnection[id] = attributeName
   self~setmethod(attributeName, "expose "attributeName"; return "attributeName)
   self~setmethod(attributeName || "=", "expose "attributeName"; use arg "attributeName)
   interpret("self~"attributeName"=''")   /* initial value = "" */
   return id


   /* This method changes the title of a window */

::method SetWindowTitle unguarded
   use arg hwnd, text
   return Wnd_Desktop("SETTXT", hwnd, text)

::method WindowTitle unguarded
   use arg handle
   return Wnd_Desktop("TXT", handle)

   /* This method returns the title of the item 'id'  */
::method ItemTitle unguarded
   use arg id
   return Wnd_Desktop("TXT", self~GetItem(id))

   /* This changes the text of a static text control */

::method SetStaticText unguarded
   use arg id, dataString
   return self~InternalSetItemData(id, dataString, self~DlgHandle, 0)

   /* This method loads the data attributes into the dialog */

::method SetData unguarded
   i = self~DataConnection~First
   do while (i \= .NIL)
      s = "self~" || self~DataConnection[i]
      s = "InternDlgData."i || " = " || s
      interpret(s)
      i = self~DataConnection~Next(i)
   end
   ret = SetStemData(self~Adm, "InternDlgData.")

   /* This method receives data from dialog and copies them to the */
   /* associated attributes */

::method GetData unguarded
   ret = GetStemData(self~Adm, "InternDlgData.")

   i = self~DataConnection~First
   do while (i \= .NIL)
      s = "self~" || self~DataConnection[i]
      s = s ||"= InternDlgData."i
      interpret(s)
      i = self~DataConnection~Next(i)
   end


   /* This method returns an array that is filled with all the dialog  */
   /* item's data */

::method MakeArray unguarded

   a = .Array~new(self~DataConnection~items)
   i = self~DataConnection~First
   do j=1 while (i \= .NIL)
      cml="a["j"]=self~"self~DataConnection~at(i)
      interpret( cml )
      i = self~DataConnection~Next(i)
   end
   return a


  /* This method sets all windows controls to the value of the given */
  /* 'dataStem' stem variable */

::method SetDataStem protected unguarded
   use arg dataStem.
   do k over dataStem.
      if k~dataType('W') then numericID = k
      else numericID = self~ResolveSymbolicID(k)
      InternDlgData.numericID = dataStem.k
      if InternDlgData.numericID = "INTERNDLGDATA."numericID then InternDlgData.numericID = ""
   end
   ret = SetStemData(self~Adm, "InternDlgData.")

   /* This method gets the values of all the dialog's windows controls */
   /* and copies them to 'dataStem'. */

::method GetDataStem protected unguarded
   use arg dataStem.
   ret = GetStemData(self~Adm, "InternDlgData.")
   do k over InternDlgData.
      dataStem.k = InternDlgData.k
      symbolicID = self~resolveNumericID(k)
      if symbolicID \== -1 then dataStem.symbolicID = InternDlgData.k
   end


::method BoxMessage unguarded private
   use arg id, msg, wP, lP
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   return SendWinMsg("PTR", Self~DlgHandle, id, msg, wP, lP)

   /* This method adds a string to the combobox */

::method AddComboEntry unguarded
   use arg id, string
   if Arg(2,'o') = 1 | string = "DATA" then return -1
   return self~BoxMessage(id, 0x00000143, 0, "T" || string) + 1

   /* This method inserts a string to the combobox */

::method InsertComboEntry unguarded
   use arg id, item, string
   if Arg(3,'o') = 1 | string = "DATA" then return -1
   if Arg(2,"o") = 1 then item = self~GetCurrentComboIndex(id)
   return self~BoxMessage(id, 0x0000014A, item-1, "T" || string) + 1

   /* This method deletes a string from the combo box. */
   /* For argument 'index' don't use string but the index instead */
   /* Use 'FindComboEntry' to retrieve the index of an item. */

::method DeleteComboEntry unguarded
   use arg id, index
   if Arg(2,"o") = 1 then index = self~GetCurrentComboIndex(id)
   return self~BoxMessage(id, 0x00000144, index-1, 0)


   /* This method finds a string in the combo box. It returns the */
   /* corresponding index */

::method FindComboEntry unguarded
   use arg id, string
   if Arg(2,'o') = 1 | string = "DATA" then return -1
   item = self~BoxMessage(id, 0x0000014C, 0, "T" || string)
   if item >= 0 then return item + 1; else return 0


   /* This returns the index of the currently selected item */

::method GetCurrentComboIndex unguarded
   use arg id
   return self~BoxMessage(id, 0x00000147, 0, 0)+1


   /* Sets the selection index (0 means nothing selected) */

::method SetCurrentComboIndex unguarded
   use arg id, ndx
   if arg(2,'o') = 1 then ndx = 0
   return self~BoxMessage(id, 0x0000014E, ndx-1, 0)

   /* Returns the number of items within the combo box */

::method GetComboItems unguarded
   use arg id
   return self~BoxMessage(id, 0x00000146, 0, 0)

   /* Returns the text string of entry index */

::method GetComboEntry unguarded
   use arg id, ndx
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   len = SendWinMsg("DLG", Self~DlgHandle, id, 0x00000149, ndx-1,0)
   if len <= 0 then return ""
   else return SendWinMsg("PTR", Self~DlgHandle, id, 0x00000148, ndx-1,"G" || len+1)

   /* Changes the value of 'item' to 'string' */

::method ChangeComboEntry unguarded
   use arg id, item, string
   if Arg(2,"o") = 1 then item = self~GetCurrentComboIndex(id)
   if item <= 0 then return -1
   self~DeleteComboEntry(id, item)
   return self~InsertComboEntry(id, item, string)


   /* add all filenames in the directory to the combo box */
   /* argument fileAttributes can be none, one or more of (seperated by blanks): */

::method ComboAddDirectory unguarded
   use arg id, drvpath, fileAttributes
   READWRITE  = 0
   READONLY   = 1
   HIDDEN     = 2
   SYSTEM     = 4
   DIRECTORY  = 16
   ARCHIVE    = 32
   interpret( "opts = "fileAttributes~translate('+',' ') )
   return self~BoxMessage(id, 0x00000145, opts, "T" || drvpath) + 1


   /* Deletes all strings of the combo box */

::method ComboDrop unguarded
   use arg id
   return self~BoxMessage(id, 0x0000014B, 0, 0)


/* The following methods are to manipulate a list box. */
/*  For remarks see the combo box methods */


   /* Adds 'data' (a string) to the list box 'id' */

::method AddListEntry unguarded
   use strict arg id, data
   return self~BoxMessage(id, 0x00000180, 0, "T" || data) + 1

   /* Inserts 'data' (a string) into the list box 'id'. The new item */
   /* will be inserted after that with 'index' */

::method InsertListEntry unguarded
   use arg id, index, data
   if Arg(3,'o') = 1 | data = "DATA" then return -1
   if Arg(2,"o") = 1 then index = self~GetCurrentListIndex(id)
   return self~BoxMessage(id, 0x00000181, index-1, "T" || data) + 1

   /* Deletes an item from a list box. */
   /* For argument 'index' don't use string but the index */

::method DeleteListEntry unguarded
   use arg id, index
   if Arg(2,"o") = 1 then index = self~GetCurrentListIndex(id)
   return self~BoxMessage(id, 0x00000182, index-1, 0)

   /* Returns the index of 'dataString' in the list box 'id' */

::method FindListEntry unguarded
   use arg id, dataString
   if Arg(2,'o') = 1 | dataString = "DATASTRING" then return -1
   index = self~BoxMessage(id, 0x0000018F, 0, "T" || dataString)
   if index >= 0 then return index + 1; else return 0

   /* Returns the index of the currently selected list box item */

::method GetCurrentListIndex unguarded
   use arg id
   return self~BoxMessage(id, 0x00000188, 0, 0) + 1

   /* Sets the selection index (0 means nothing selected) */

::method SetCurrentListIndex unguarded
   use arg id, ndx
   if arg(2,'o') = 1 then ndx = 0
   return self~BoxMessage(id, 0x00000186, ndx-1, 0)

   /* Returns the number of items within the list box */

::method GetListItems unguarded
   use arg id
   return self~BoxMessage(id, 0x0000018B, 0, 0)

   /* Returns the text string of entry index */

::method GetListEntry unguarded
   use arg id, ndx
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   len = SendWinMsg("DLG", Self~DlgHandle, id, 0x0000018A, ndx-1,0)
   if len <= 0 then return ""
   else return SendWinMsg("PTR", Self~DlgHandle, id, 0x00000189, ndx-1,"G" || len+1)

   /* Changes the item at 'index' with 'dataString' */

::method ChangeListEntry unguarded
   use arg id, index, dataString
   if Arg(2,"o") = 1 then index = self~GetCurrentListIndex(id)
   if index <= 0 then return -1
   self~DeleteListEntry(id, index)
   return self~InsertListEntry(id, index, dataString)

   /* Sets tabulators for a list box. First arg is the list box' id */

::method SetListTabulators unguarded
   if arg() < 2 then return -1
   id = arg(1)
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   cs = self~DlgHandle", "id
   do i=2 to arg()
      cs = cs", "arg(i)
   end
   interpret("call SetLBTabStops "cs)


   /* Adds all filenames in the directory 'drvPath' to the list box */
   /* argument 'fileAttributes' can be none, one or more of (seperated by blanks): */

::method ListAddDirectory unguarded
   use arg id, drvPath, fileAttributes
   READWRITE  = 0
   READONLY   = 1
   HIDDEN     = 2
   SYSTEM     = 4
   DIRECTORY  = 16
   ARCHIVE    = 32
   interpret( "opts = "fileAttributes~translate('+',' ') )
   return self~BoxMessage(id, 0x0000018D, opts, "T" || drvpath) + 1

   /* Removes all items from list box 'id' */

::method ListDrop unguarded
   use arg id
   return self~BoxMessage(id, 0x00000184, 0, 0)


/* The following methods are to get the data entry from a single */
/*   dialog item */

::method InternalGetItemData unguarded
   use arg id, handle, type
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if arg(2,"o") = 1 then return GetItemData(self~Adm, id)
   else if arg(3,"o") = 1 then return GetItemData(self~Adm, id, handle)
   else return GetItemData(self~Adm, id, handle, type)


   /* Returns the content of the entry line control with id 'id' */

::method GetEntryLine unguarded
   use arg id
   return self~InternalGetItemData(id, self~DlgHandle, 0)

   /* Returns the content of the currently selected item in a list box */

::method GetListLine unguarded
   use arg id
   return self~InternalGetItemData(id, self~DlgHandle, 3)

   /* Returns the content of the currently selected item in a combo box */

::method GetComboLine unguarded
   use arg id
   return self~InternalGetItemData(id, self~DlgHandle, 0)

   /* Returns 1 if the check box 'id' is selected, that is it has a check */
   /* mark. Otherwise it returns 0 */

::method GetCheckBox unguarded
   use arg id
   return self~InternalGetItemData(id, self~DlgHandle, 1)

   /* Returns 1 if the radio button 'id' is selected, otherwise it returns 0 */

::method GetRadioButton unguarded
   use arg id
   return self~InternalGetItemData(id, self~DlgHandle, 2)

   /* This method can be applied to a multiple choice list box. */
   /* It returns a string containing all the selected items' index. */
   /* The numbers are seperated by spaces. You can parse it using 'parse var' */

::method GetMultiList unguarded
   use arg id
   return self~InternalGetItemData(id, self~DlgHandle, 4)

   /* This method gets the value of a dialog item. You don't have to know */
   /* what kind of item it is when it has been connected before */

::method GetValue unguarded
    use arg id
    return self~InternalGetItemData(id, self~DlgHandle)

   /* Sets 'attributeName' with the value of the associated dialog field. */
   /* Returns  nothing! */

::method GetAttrib unguarded
   use arg attributeName
   i = self~DataConnection~First
   do while (i \= .NIL)
      if self~DataConnection[i] = attributeName~space(0) then
      do
         s = "self~"attributeName"= self~GetValue("i")"
         interpret(s)
         return
      end
      i = self~DataConnection~Next(i)
   end


 /* The following methods are to set the data entry of a single */
 /* dialog item */

::method InternalSetItemData unguarded
   use arg id, dataString, handle, type
   if id~DataType("N") = 0 then id = self~ResolveSymbolicId(id)
   if id = -1 then return -1
   if arg(3,"o") = 1 then return SetItemData(self~Adm, id, dataString)
   else if arg(4,"o") = 1 then return SetItemData(self~Adm, id, dataString, handle)
   else return SetItemData(self~Adm, id, dataString, handle, type)

   /* Puts 'dataString' into the entry line control 'id' */

::method SetEntryLine unguarded
   use arg id, dataString
   return self~InternalSetItemData(id, dataString, self~DlgHandle, 0)

   /* Puts 'dataString' into the list box 'id' */

::method SetListLine unguarded
   use arg id, dataString
   return self~InternalSetItemData(id, dataString, self~DlgHandle, 3)

   /* Puts 'dataString' into the combo box 'id' */

::method SetComboLine unguarded
   use arg id, dataString
   return self~InternalSetItemData(id, dataString, self~DlgHandle, 0)

   /* Puts a check mark to the check box if 'data' equals 1 */
   /* and remove the check mark if 'data' equals 0 */

::method SetCheckBox unguarded
   use arg id, data
   return self~InternalSetItemData(id, data, self~DlgHandle, 1)

   /* Checks the radio button if 'data' equals 1 */
   /* and unchecks it if 'data' equals 0 */

::method SetRadioButton unguarded
   use arg id, dataString
   return self~InternalSetItemData(id, dataString, self~DlgHandle, 2)

   /* Use this method to select one ore more lines in a multiple choice */
   /* list box. Provide the index of all lines you want to select */
   /* (seperated by blanks) in argument 'data'. */

::method SetMultiList unguarded
   use arg id, data
   return self~InternalSetItemData(id, data, self~DlgHandle, 4)

   /* This method sets the value of a dialog item. You don't have */
   /* to know what kind of item it is if it has been connected before */

::method SetValue unguarded
   use arg id, dataString
   return self~InternalSetItemData(id, dataString)

   /* Put the value of 'attributeName' into the associated dialog field */

::method SetAttrib unguarded
   use arg attributeName
   i = self~DataConnection~First
   do while (i \= .NIL)
      if self~DataConnection[i] = attributeName~space(0) then
      do
         s = "self~SetValue("i", self~"attributeName")"
         interpret(s)
         return
      end
      i = self~DataConnection~Next(i)
   end


   /* Returns 1 (true) if Windows dialog is still existing */

::method IsDialogActive unguarded
   return HandleDlg("ACTIVE", self~DlgHandle)


   /* This method will handle one dialog message synchronously */

::method HandleMessages unguarded protected
   use strict arg sleeptime = 1
   if self~Adm = 0 then do
       self~finished = 2
       return
   end
   if \ sleeptime~dataType("W") then sleeptime = 1
   msg = GetDlgMsg(self~Adm)
   if msg~pos("1DLGDELETED1") > 0 then do
      if self~finished = 0 then self~finished = 2
   end
   else do
      if msg \= "" then interpret("self~"msg)
      else call msSleep sleeptime
   end

   /* This method will clear all pending dialog messages */

::method ClearMessages unguarded protected
   if self~Adm = 0 then return
   msg = GetDlgMsg(self~Adm)
   do while msg \= "" & self~Adm \= 0
       if msg~pos("1DLGDELETED1") > 0 then do
          if self~finished = 0 then self~finished = 2
       end
       msg = GetDlgMsg(self~Adm)
   end


 /* The following methods are to enable/disable and show/hide dialog items */
 /* The extension fast means that the method will not redraw the item */

::method EnableItem unguarded
   use arg id
   h = self~GetItem(id)
   if h = 0 then return -1
   return Wnd_Desktop("ENABLE",h, 1)

::method DisableItem unguarded
   use arg id
   h = self~GetItem(id)
   if h = 0 then return -1
   return Wnd_Desktop("ENABLE",h, 0)

::method HideItem unguarded
   use arg id
   h = self~GetItem(id)
   if h = 0 then return -1
   return WndShow_Pos("S",h, "HIDE")

::method ShowItem unguarded
   use arg id
   h = self~GetItem(id)
   if h = 0 then return -1
   return WndShow_Pos("S",h, "NORMAL")

::method HideWindow unguarded
   use arg hwnd
   return WndShow_Pos("S",hwnd, "HIDE")

::method ShowWindow unguarded  /* used for the category dialog */
   use arg hwnd
   return WndShow_Pos("S",hwnd, "NORMAL", self~Adm)

   /* resize a dialog including border */
::method Resize unguarded
   use arg width, hight, showOptions
   if Arg(3,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   parse value Wnd_Desktop("RECT", self~hwnd) with x y cx cy
   /* diffsx and diffsy are needed for the title and the frame */
   diffsx = cx - x - (self~SizeX * self~FactorX)
   diffsy = cy - y - (self~SizeY * self~FactorY)
   self~SizeX = width
   self~SizeY = hight
   return WndShow_Pos("P", self~DlgHandle, 0, 0, width * self~FactorX + diffsx,,
                          hight* self~FactorY + diffsy,"NOMOVE "||showOptions )

::method FocusItem unguarded
   use arg id
   return SendWinMsg("ANY", self~DlgHandle, "0x0028", self~GetItem(id), 1)

::method TabToNext unguarded
   return Wnd_Desktop("SETFOC", self~DlgHandle, 0, "N")

::method TabToPrevious unguarded
   return Wnd_Desktop("SETFOC", self~DlgHandle, 1, "P")

::method getTextSize unguarded
  forward message "getTextSizeDlg" continue
  return result~width result~height

::method getTextSizeDlg unguarded external "LIBRARY oodialog pbdlg_getTextSizeDlg"


 -- 2 methods to set / remove the WS_TABSTOP / WS_GROUP styles for any control.
::method SetTabStop unguarded
   use strict arg id, wantStop = .true
   if \ id~datatype("W") then id = self~ResolveSymbolicId(id)
   if id == -1 then return -1
   if \ wantStop~datatype('O')then return -3
   return HandleControlEx(self~DlgHandle, id, "X", "TAB", wantStop)

::method SetGroup unguarded
   use strict arg id, wantGroup = .true
   if \ id~datatype("W") then id = self~ResolveSymbolicId(id)
   if id == -1 then return -1
   if \ wantGroup~datatype('O')then return -3
   return HandleControlEx(self~DlgHandle, id, "X", "GROUP", wantGroup)

::method Center unguarded
   use arg showOptions
   if Arg(1,"o") = 1 then showOptions = ""; else showOptions = showOptions~translate
   parse value self~GetSize with dcx dcy
   s = screenSize()
   return WndShow_Pos("P",self~DlgHandle, (s[3] - dcx * self~FactorX) %2, (s[4] - dcy * self~FactorY) %2, 0, 0,,
                           "NOSIZE "||showOptions)


   /* This method will be called to get the default setting for the */
   /* automatic data field detection. If you disable the ADD, you'll */
   /* have to use the Connect... methods to assign a dialog item */
   /* to a data stem field */

::method InitAutoDetection protected /* autodetection is enabled by default */
   self~AutoDetection

::method NoAutoDetection    /* disable autodetection */
   self~AutoDetect = 0

::method AutoDetection      /* enable autodetection */
   self~AutoDetect = 1

 /* The following methods are abstract methods that will be called */
 /* whenever the push buttons 1, 2, or 9 will be pressed */
 /* Method 'OK' will call method 'Validate' */
 /* If self~finished = 1 the dialog is destroyed, if 0 the dialog */
 /* execution will be continued */
 /* OK returns 1 if the dialog is going to be destroyed */

::method OK unguarded
   self~finished = self~Validate
   self~InitCode = 1
   return self~finished

 /* If self~finished = 1 the dialog is destroyed, if 0 the dialog */
 /* execution will be continued */

::method Cancel unguarded
   self~finished = 1
   if self~finished = 1 then self~InitCode = 2
   return self~finished

::method Help unguarded
   return 0

::method Leaving
   return

/** validate()
 * This method is an abstract method that will be called to determin whether the
 * dialog can be closed or not. This function will be called by method OK.  If
 * the return value is 1 the dialog can be closed, if it is 0 the dialog
 * values are invalid.
 */
::method validate unguarded
   return 1

/** deinstall()
 * Clean up memory allocated for this dialog.
 */
::method deInstall
   if self~Adm \= 0 then ret = HandleDialogAdmin(self~Adm)
   self~Adm = 0

::method uninit
   -- Check if the dialog table still needs to be freed.
   if self~Adm \= 0 then ret = HandleDialogAdmin(self~Adm)
   self~Adm = 0
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
/* Dynamic Dialog Class                                                       */
/*                                                                            */
/******************************************************************************/

::class 'DynamicDialog' Mixinclass Object public

    /* attributes for internal use */
::method BasePtr attribute protected
::method activePtr attribute protected
::method DialogItemCount attribute protected


::method DynamicInit private
   self~activePtr = 0
   self~DialogItemCount = 0
   if Arg(1,'o') = 1 then self~UseStem = 0; else self~UseStem = 1
   return 0

   /*  */
::method InitAutoDetection protected
   self~AutoDetection

   /* This method creates the dialog you previously defined via Add... methods */
   /* You can set the font and the number of expected items */
   /* argument 'options' can be one of:
         "CHILD"       -  create a child dialog
         "NOMENU"      -  create a dialog without menu
         "VISIBLE"     -  make the dialog visible before executing
         "SYSTEMMODAL" -  create a dialog that is always on top
         "NOTMODAL"    -  create a dialog with a normal window frame
         "THICKFRAME"  -  create a dialog with a thick frame */

::method Create
   use arg x, y, cx, cy, title, options = "", dlgClass, fontName, fontSize, expected = 200

   -- options must be all upper case
   options = options~translate

   if arg(7,"O") | dlgClass = 0 then dlgClass = ""

   if arg(8, "O") then fontName = self~fontName
   else self~fontName = fontName

   if arg(9, "O") then fontSize = self~fontSize
   else self~fontSize = fontSize

   ret = UsrDefineDialog(x, y, cx, cy, dlgclass, title, fontName, fontSize,,
                                      options, expected)
   parse var ret base ptr
   self~BasePtr = base
   self~activePtr = ptr
   if ptr = 0 then call HandleDlg "STOP"
   self~SizeX = cx
   self~SizeY = cy
   self~DefineDialog
   return ptr  /* self~activePtr could have been set to 0 for a CategoryDialog */

   /* This method will create the dialog and place it in a centered position */
   /* See 'Create' for possible 'options' */

::method CreateCenter
   arga = Arg(1,"A")
   newarg = .array~new(Arg()+2)
   do i = Arg() to 1 by -1; if arga~hasindex(i) = 1 then newarg[i+2] = arga[i]; end   /* shift arguments up by 2 */
   -- Let OS center the dialog.
   if \ newarg~hasindex(6) then newarg[6] = "CENTER"
   else newarg[6] = newarg[6] || " CENTER"
   -- When dialog has centered style, the OS will ignore the x, y co-ordinates
   newarg[1] = 0
   newarg[2] = 0
   forward message "Create" Arguments (newarg)


   /* This method is provided to be used in any subclass of 'UserDialog' */

::method DefineDialog protected
   return 0


   /* This method creates the dialog on the data of a given resource file   */
   /* (extention '.rc'). It calls 'LoadFrame' and 'LoadItems' */
   /* argument 'loadOptions' can be one or more of:                         */
   /*    "CENTER"      : this will create the dialog on a centered position */
   /*    "CONNECTBUTTONS" :  */
   /*    "CONNECTRADIOS"  */
   /*    "CONNECTCHECKS" */

::method Load
   forward message "LoadFrame" continue
   hnd = result
   if hnd \= 0 then forward message "LoadItems" continue; else call HandleDlg "STOP"
   return (hnd=0)


::method InternGetRCStyle
   use arg style, kind, spec_kind
   ret = ""
   if style~wordpos("NOT WS_VISIBLE") > 0 then ret = "HIDDEN"
   else if style~wordpos("WS_VISIBLE") > 0 then ret = "VISIBLE"
   if style~wordpos("WS_DISABLED") > 0 & style~wordpos("NOT WS_DISABLED") == 0 then ret = ret || " DISABLED"
   if style~wordpos("WS_GROUP") > 0 & style~wordpos("NOT WS_GROUP") == 0 then ret = ret || " GROUP"

   if kind = "DLG" then do
      if style~wordpos("DS_CENTER") > 0 then ret = ret ||" CENTER"
      if style~wordpos("DS_SYSMODAL") > 0 then ret = ret ||" SYSTEMMODAL"
      if style~wordpos("DS_MODALFRAME") = 0 then ret = ret ||" NOTMODAL"
      if style~wordpos("WS_SYSMENU") = 0 then ret = ret ||" NOMENU"
      if style~wordpos("WS_THICKFRAME") > 0 then ret = ret ||" THICKFRAME"
      if style~wordpos("WS_MINIMIZEBOX") > 0 | style~wordpos("WS_GROUP") > 0 then ret = ret ||" MINIMIZEBOX"
      if style~wordpos("WS_MAXIMIZEBOX") > 0 | style~wordpos("WS_TABSTOP") > 0  then ret = ret ||" MAXIMIZEBOX"
      if style~wordpos("WS_OVERLAPPED") > 0 then ret = ret ||" OVERLAPPED"
   end
   else if kind = "BTN" then do
       if style~wordpos("WS_OWNERDRAW") > 0 | style~wordpos("BS_OWNERDRAW") > 0 then ret = ret || " OWNER"
       if ret~wordpos("OWNER") = 0 then if style~wordpos("BS_AUTO3STATE") > 0 then ret = ret || " 3STATE"
       if style~wordpos("BS_DEFPUSHBUTTON") > 0 then ret = ret || " DEFAULT"
       if style~wordpos("BS_LEFTTEXT") > 0 | style~wordpos("BS_RIGHTBUTTON") > 0 then ret = ret || " LEFTTEXT"
       if style~wordpos("BS_BITMAP") > 0 then ret = ret || " BITMAP"
       if style~wordpos("BS_ICON") > 0 then ret = ret || " ICON"
       if style~wordpos("BS_LEFT") > 0 then ret = ret || " LEFT"
       if style~wordpos("BS_RIGHT") > 0 then ret = ret || " RIGHT"
       if style~wordpos("BS_CENTER") > 0 then ret = ret || " HCENTER"
       if style~wordpos("BS_TOP") > 0 then ret = ret || " TOP"
       if style~wordpos("BS_BOTTOM") > 0 then ret = ret || " BOTTOM"
       if style~wordpos("BS_VCENTER") > 0 then ret = ret || " VCENTER"
       if style~wordpos("BS_PUSHLIKE") > 0 then ret = ret || " PUSHLIKE"
       if style~wordpos("BS_MULTILINE") > 0 then ret = ret || " MULTILINE"
       if style~wordpos("BS_NOTIFY") > 0 then ret = ret || " NOTIFY"
       if style~wordpos("BS_FLAT") > 0 then ret = ret || " FLAT"
       if style~wordpos("WS_BORDER") > 0 then ret = ret || " BORDER"
       if style~wordpos("NOT WS_TABSTOP") > 0 then ret = ret || " NOTAB"
       if spec_kind == "R" then do  /* radio buttons need explicit WS_TABSTOP to be tab stops */
           if style~wordpos("WS_TABSTOP") = 0 then ret = ret || " NOTAB"
       end
   end
   else if kind = "GB" then do
       if style~wordpos("BS_LEFT") > 0 then ret = ret || " LEFT"
       if style~wordpos("BS_RIGHT") > 0 then ret = ret || " RIGHT"
       if style~wordpos("WS_BORDER") > 0 then ret = ret || " BORDER"
       if style~wordpos("WS_TABSTOP") > 0 then ret = ret || " TAB"
   end
   else if kind = "EDIT" then do
      if style~wordpos("ES_AUTOHSCROLL") > 0 then ret = ret || " AUTOSCROLLH"
      if style~wordpos("ES_AUTOVSCROLL") > 0 then ret = ret || " AUTOSCROLLV"
      if style~wordpos("ES_PASSWORD") > 0 then ret = ret || " PASSWORD"
      if style~wordpos("ES_MULTILINE") > 0 then ret = ret || " MULTILINE"
      if style~wordpos("ES_READONLY") > 0 then ret = ret || " READONLY"
      if style~wordpos("ES_WANTRETURN") > 0 then ret = ret || " WANTRETURN"
      if style~wordpos("ES_NOHIDESEL") > 0 then ret = ret || " KEEPSELECTION"
      if style~wordpos("ES_CENTER") > 0 then ret = ret || " CENTER"
      if style~wordpos("ES_RIGHT") > 0 then ret = ret || " RIGHT"
      if style~wordpos("ES_UPPERCASE") > 0 then ret = ret || " UPPER"
      if style~wordpos("ES_LOWERCASE") > 0 then ret = ret || " LOWER"
      if style~wordpos("ES_NUMBER") > 0 then ret = ret || " NUMBER"
      if style~wordpos("ES_OEMCONVERT") > 0 then ret = ret || " OEM"
      if style~wordpos("NOT WS_TABSTOP") > 0 then ret = ret || " NOTAB"
   end
   else if kind = "CB" then do
      if style~wordpos("CBS_SIMPLE") > 0 then ret = ret || " SIMPLE"
      if style~wordpos("CBS_DROPDOWNLIST") > 0 then ret = ret || " LIST"
      if style~wordpos("CBS_SORT") > 0 then ret = ret || " SORT"
      if style~wordpos("CBS_AUTOHSCROLL") = 0 then ret = ret || " NOHSCROLL"
      if style~wordpos("CBS_NOINTEGRALHEIGHT") > 0 then ret = ret || " PARTIAL"
      if style~wordpos("WS_TABSTOP") = 0 then ret = ret || " NOTAB"
   end
   else if kind = "LB" then do
      if style~wordpos("LBS_USETABSTOPS") > 0 then ret = ret || " COLUMNS"
      if style~wordpos("LBS_STANDARD") > 0 then ret = ret|| " SORT"
      if style~wordpos("LBS_NOTIFY") > 0 then ret = ret || " NOTIFY"
      if style~wordpos("LBS_MULTIPLESEL") > 0 then ret = ret || " MULTI"
      if style~wordpos("LBS_MULTICOLUMN") > 0 then ret = ret || " MCOLUMN"
      if style~wordpos("LBS_NOINTEGRALHEIGHT") > 0 then ret = ret || " PARTIAL"
      if style~wordpos("LBS_DISABLENOSCROLL") > 0 then ret = ret || " SBALWAYS"
      if style~wordpos("LBS_WANTKEYBOARDINPUT") > 0 then ret = ret || " KEYINPUT"
      if style~wordpos("LBS_EXTENDEDSEL") > 0 then ret = ret || " EXTSEL"
      if style~wordpos("WS_TABSTOP") = 0 then ret = ret || " NOTAB"
   end
   else if kind = "ST" then do
       if style~wordpos("SS_CENTERIMAGE") > 0 then ret = ret || " CENTERIMAGE"
       if style~wordpos("SS_EDITCONTROL") > 0 then ret = ret || " EDITCONTROL"
       if style~wordpos("SS_ENDELLIPSIS") > 0 then ret = ret || " ENDELLIPSIS"
       if style~wordpos("SS_NOPREFIX") > 0 then ret = ret || " NOPREFIX"
       if style~wordpos("SS_NOTIFY") > 0 then ret = ret || " NOTIFY"
       if style~wordpos("SS_PATHELLIPSIS") > 0 then ret = ret || " PATHELLIPSIS"
       if style~wordpos("SS_RIGHTJUST") > 0 then ret = ret || " RIGHTJUST"
       if style~wordpos("SS_REALSIZECONTROL") > 0 then ret = ret || " SIZECONTROL"
       if style~wordpos("SS_REALSIZEIMAGE") > 0 then ret = ret || " SIZEIMAGE"
       if style~wordpos("SS_SUNKEN") > 0 then ret = ret || " SUNKEN"
       if style~wordpos("SS_WORDELLIPSIS") > 0 then ret = ret || " WORDELLIPSIS"

       if style~wordpos("WS_BORDER") > 0 then ret = ret || " BORDER"
       if style~wordpos("WS_TABSTOP") > 0 then ret = ret || " TAB"
   end
   else if kind = "SB" then do
      if style~wordpos("SBS_HORZ") > 0 then ret = ret || " HORIZONTAL"
      if style~wordpos("SBS_TOPALIGN") > 0 then ret = ret || " TOPLEFT"
      if style~wordpos("SBS_BOTTOMALIGN") > 0 then ret = ret || " BOTTOMRIGHT"
      if style~wordpos("WS_TABSTOP") > 0 then ret = ret || " TAB"
      if style~wordpos("WS_BORDER") > 0 then ret = ret || " BORDER"
   end

   if kind = "EDIT" | kind = "LB" | kind = "CB" then
      if style~wordpos("NOT WS_BORDER") > 0 then ret = ret || " NOBORDER"
   if kind = "EDIT" | kind = "CB" | kind = "LB" | kind = "DLG" then do
      if style~wordpos("WS_VSCROLL") > 0 then ret = ret || " VSCROLL"
      if style~wordpos("WS_HSCROLL") > 0 then ret = ret || " HSCROLL"
   end

   return ret


::method InternAddRCButton
   use arg id, x, y, w, h, name, style, lOpts
   id = self~CheckId(id)
   if id < 0 then return
   opts = self~InternGetRCStyle(style, "BTN")
   self~DialogItemCount = self~DialogItemCount + 1
   self~activePtr = UsrAddControl("BUT", self~activePtr, id, x, y, w, h, name, opts)
   if lOpts~wordpos("CONNECTBUTTONS") > 0 then
      self~ConnectButton(id, name~space(0)~changestr('&','')~changestr(':',''))
   else self~ConnectButton(id)


::method InternAddRC_RBCB
   use arg id, x, y, w, h, name, style, lOpts, kind
   opts = self~InternGetRCStyle(style, "BTN", kind)
   if (kind = "R" & lOpts~wordpos("CONNECTRADIOS") > 0) | lOpts~wordpos("CONNECTCHECKS") > 0 then
   do
      self~ConnectControl(id, "ID"||name~space(0)~changestr('&','')~changestr(':',''))
      if kind = "R" then self~AddRadioButton(id,, x, y, w, h, name, opts)
      else self~AddCheckBox(id,, x, y, w, h, name, opts)
   end
   else do
      if kind = "R" then self~AddRadioButton(id,name,x, y, w, h, name, opts)
      else self~AddCheckBox(id, name, x, y, w, h, name, opts)
   end


   /* This method creates the dialog main window using the data of the */
   /* given resource file. It is called by the 'Load' method. */

::method LoadFrame protected
   use arg resfile, dialogid, options, expected
   if Arg(4,"o") = 1 | expected = 0 then expected = 200
   if options~translate~wordpos("CENTER") > 0 then internopts = "CENTER "
   else internopts = ""

   file = SysSearchPath("PATH",resfile)
   f = .stream~new(file)
   op = f~open(read)
   if op \= "READY:" then do
      say "Resource file not found -->" resfile "("op")"
      return 0
   end

   found = 0; n = 0
   fl = f~lines
   do while found = 0
      do while n = 0 & fl > 0
         s = f~linein; fl = fl -1
         select
            when s~wordpos("DIALOG ") > 0 then n = s~wordpos("DIALOG ")
            when s~wordpos("DIALOGEX ") > 0 then n = s~wordpos("DIALOGEX ")
            when s~wordpos("ICON") = 2 then do
               -- ICON RESOURCE statement as opposed to an ICON CONTROL statement.
               -- filename could have spaces and *should* be in quotes.
               parse var s nameID ic fileName
               self~AddIcon(nameID, fileName)
            end
            /* premission: #define only at the top of the RC and separated by a tab or a blank */
            when s~wordpos("#define") > 0 then do
               s = s~translate(" ", "9"x)
               parse var s "#define " symb iid
               self~ConstDir[symb~space(0)~translate] = iid
            end
            otherwise
               nop
         end
         -- End select
      end
      if (self~checkfile(f) = 1) then return 1
      if Arg(2,'o') = 1 | dialogid = "DIALOGID" then found = 1
      else do
         foundID = s~word(n-1)~translate
         if foundID = dialogid~translate then found = 1
         else if self~CheckId(foundID) = dialogid then found = 1
         else if foundID = self~CheckId(dialogid~translate) then found = 1
         else n = 0
      end
   end

   n = s~Pos(",")
   parse var s prev "," dlg.y "," dlg.w "," dlg.h
   dlg.x = s~substr(n-3,3)
   if dlg.x~datatype('N') = 0 then dlg.x = s~substr(n-2,2)

   s = f~linein; fl = fl -1
   do while s~wordpos("STYLE ") = 0 & fl > 0
      s = f~linein; fl = fl -1
   end
   if (self~checkfile(f) = 1) then return 1
   parse var s prev dlg.style
   dlg.style = dlg.style~translate(" ", "|")

   internopts = internopts || self~InternGetRCStyle(dlg.style, "DLG")

   s = f~linein; fl = fl -1
   do while s~wordpos("CAPTION ") = 0 & s~wordpos("FONT ") = 0 & fl > 0
      s = f~linein; fl = fl -1
   end
   if (self~checkfile(f) = 1) then return 1
   if s~wordpos("CAPTION ") > 0 then do
      parse var s prev '"'dlg.title'"'
      s = f~linein; fl = fl -1
      do while s~wordpos("FONT ") = 0 & fl > 0
         s = f~linein; fl = fl -1
      end
      if fl > 0 then
         parse var s prev dlg.fsize "," '"'dlg.font'"'
      else do
         dlg.fsize = ""
         dlg.font = ""
      end
   end
   else do
      parse var s prev dlg.fsize "," '"'dlg.font'"'
      dlg.title = ""
   end
   f~close

   return self~create(dlg.x, dlg.y, dlg.w, dlg.h, dlg.title, internopts,, dlg.font, dlg.fsize, expected)




   /* This method creates the dialog's items using the data of the */
   /* given resource file. It is called by the 'Load' method. */
   /* */

::method LoadItems protected
   use arg resfile, dialogid, loadOptions
   if self~activePtr = 0 then return -2

   file = SysSearchPath("PATH",resfile)
   f = .stream~new(file)
   op = f~open(read)
   if op \= "READY:" then do
      say "Resource file not found --> " op
      return 1
   end
   if Arg(3, "o") = 1 then loadOptions = ""; else loadOptions = loadOptions~translate
   found = 0; n = 0
   fl = f~lines
   do while found = 0 & fl > 0
      do while n = 0 & fl > 0
         s = f~linein; fl = fl - 1
         /* premission: #define only at the top of the RC and separated by a tab or a blank */
         if s~wordpos("#define") > 0 then do
             s = s~translate(" ", "9"x)
             parse var s "#define " symb iid
             self~ConstDir[symb~space(0)~translate] = iid
         end
         n = s~wordpos("DIALOG")
         if n = 0 then n = s~wordpos("DIALOGEX")
      end
      if self~checkfile(f)=1 then return 1
      if Arg(2,'o') = 1 | dialogid = "DIALOGID" then found = 1
      else do
         foundID = s~word(n-1)~translate
         if foundID = dialogid~translate then found = 1
         else if self~CheckId(foundID) = dialogid then found = 1
         else if foundID = self~CheckId(dialogid~translate) then found = 1
         else n = 0
      end
   end

   s = f~linein; fl = fl - 1
   do while s~wordpos("BEGIN") = 0 & s~pos("{") = 0 & fl > 0
       s = f~linein; fl = fl - 1
   end
   if self~checkfile(f)=1 then return 1

   self~ProcessingLoad = 1
   do while fl > 0 & s~wordpos("END") = 0 & s~pos("}") = 0
      s = f~linein; fl = fl - 1
      if s~wordpos("END") > 0 | s~pos("}") > 0 then leave;

      if s~wordpos("CONTROL") > 0 then
      do
         parse var s type '"'name'"' "," id "," class "," style "," x "," y "," w "," h "," extStyle
         opts = ""
         class = class~strip~translate
         style = style~translate(" ", "|")
         extStyle = extStyle~translate(" ", "|")
         select
            when class = '"BUTTON"' then do
               if style~wordpos("BS_AUTORADIOBUTTON") > 0 | style~wordpos("BS_RADIOBUTTON") > 0 then
                  self~InternAddRC_RBCB(id, x, y, w, h, name, style, loadOptions, "R")
               else if style~wordpos("BS_AUTOCHECKBOX") > 0 | style~wordpos("BS_CHECKBOX") > 0 then
                  self~InternAddRC_RBCB(id, x, y, w, h, name, style, loadOptions, "C")
               else if style~wordpos("BS_GROUPBOX") > 0 then
                  self~AddGroupBox(x, y, w, h, name, self~InternGetRCStyle(style, "GB"), id)
               else if style~pos("BS_OWNERDRAW") = 0 & style~wordpos("BS_AUTO3STATE") > 0 then  /* if 3state is not ownerdrawn */
                  self~InternAddRC_RBCB(id, x, y, w, h, name, style, loadOptions, "C")
               else self~InternAddRCButton(id, x, y, w, h, name, style, loadOptions)
            end

            when class = '"STATIC"' | class = WC_STATIC then do
               opts = self~InternGetRCStyle(style, "ST")
               select
                  when style~wordpos("SS_LEFT") > 0 then
                     self~addText(x, y, w, h, name, opts, id)
                  when style~wordpos("SS_RIGHT") > 0 then
                     self~addText(x, y, w, h, name, opts || " RIGHT", id)
                  when style~wordpos("SS_CENTER") > 0 then
                     self~addText(x, y, w, h, name, opts || " CENTER", id)
                  when style~wordpos("SS_SIMPLE") > 0 then
                     self~addText(x, y, w, h, name, opts || " SIMPLE", id)
                  when style~wordpos("SS_LEFTNOWORDWRAP") > 0 then
                     self~addText(x, y, w, h, name, opts || " LEFTNOWRAP", id)
                  when style~wordpos("SS_WHITERECT") > 0 then
                     self~addWhiteRect(x, y, w, h, opts, id)
                  when style~wordpos("SS_GRAYRECT") > 0 then
                     self~addGrayRect(x, y, w, h, opts, id)
                  when style~wordpos("SS_BLACKRECT") > 0 then
                     self~addBlackRect(x, y, w, h, opts, id)
                  when style~wordpos("SS_WHITEFRAME") > 0 then
                     self~addWhiteFrame(x, y, w, h, opts, id)
                  when style~wordpos("SS_GRAYFRAME") > 0 then
                     self~addGrayFrame(x, y, w, h, opts, id)
                  when style~wordpos("SS_BLACKFRAME") > 0 then
                     self~addBlackFrame(x, y, w, h, opts, id)
                  when style~wordpos("SS_ETCHEDFRAME") > 0 then
                     self~addEtchedFrame(x, y, w, h, opts, id)
                  when style~wordpos("SS_ETCHEDHORZ") > 0 then
                     self~addEtchedHorizontal(x, y, w, h, opts, id)
                  when style~wordpos("SS_ETCHEDVERT") > 0 then
                     self~addEtchedVertical(x, y, w, h, opts, id)
                  when style~wordpos("SS_BITMAP") > 0 then
                     self~addImage(id, x, y, w, h, "BITMAP " || opts)
                  when style~wordpos("SS_ENHMETAFILE") > 0 then
                     self~addImage(id, x, y, w, h, "METAFILE " || opts)
                  when style~wordpos("SS_ICON") > 0 then
                     self~addImage(id, x, y, w, h, "ICON " || opts)
                  otherwise
                      ret = errorDialog("Static item ignored due to unknown static style option in:" || -
                                         '0d0a'x || f "("s")")
               end
            end
            when class = '"EDIT"' then
               self~AddEntryLine(id, name, x, y, w, h, self~InternGetRCStyle(style, "EDIT"))

            when class = '"COMBOBOX"' then
               self~AddComboBox(id, name, x, y, w, h, self~InternGetRCStyle(style, "CB"))

            when class = '"LISTBOX"' then
               self~AddListBox(id, name, x, y, w, h, self~InternGetRCStyle(style, "LB"))

            when class = '"SCROLLBAR"' then
               self~AddScrollBar(id, x, y, w, h, self~InternGetRCStyle(style, "SB"))

            when class = 'WC_TREEVIEW' | class = '"SYSTREEVIEW32"' then
               self~AddTreeControl(id, name, x, y, w, h, self~InternGetNewCtrlStyle(style, "TREE"))

            when class = 'WC_LISTVIEW' | class = '"SYSLISTVIEW32"' then
               self~AddListControl(id, name, x, y, w, h, self~InternGetNewCtrlStyle(style, "LIST"))

            when class = 'PROGRESS_CLASS' | class = '"MSCTLS_PROGRESS32"' then
               self~AddProgressBar(id, x, y, w, h, self~InternGetNewCtrlStyle(style, "PROGRESS"))

            when class = 'TRACKBAR_CLASS' | class = '"MSCTLS_TRACKBAR32"' then
               self~AddSliderControl(id, name, x, y, w, h, self~InternGetNewCtrlStyle(style, "SLIDER"))

            when class = 'WC_TABCONTROL' | class = '"SYSTABCONTROL32"' then
               self~AddTabControl(id, name, x, y, w, h, self~InternGetNewCtrlStyle(style, "TAB"))

            when class = 'DATETIMEPICK_CLASS' then
               self~AddDateTimePicker(id, name, x, y, w, h, self~InternGetNewCtrlStyle(style, "DTP"))

            when class = 'MONTHCAL_CLASS' then
               self~AddMonthCalendar(id, name, x, y, w, h, self~InternGetNewCtrlStyle(style, "MONTH"))

            otherwise do
               self~errorFile(f, s)
               return 1
            end
         end /* select 2*/
      end
      else do
         parse var s type '"'name'"' "," id "," x "," y "," w "," h "," style "," extStyle
         if id~space(0) = "" then parse var s type id "," x "," y "," w "," h "," style "," extStyle
         type = type~strip~translate
         style = style~translate(" ", "|")
         extStyle = extStyle~translate(" ", "|")

         select
            when type = "DEFPUSHBUTTON" | type = "PUSHBUTTON" then
            do
               if type~strip~left(3) = ("DEF") then style = style || " | BS_DEFPUSHBUTTON"
               self~InternAddRCButton(id, x, y, w, h, name, style, loadOptions)
            end

            when type = "EDITTEXT" then
               self~AddEntryLine(id, name, x, y, w, h, self~InternGetRCStyle(style, "EDIT"))

            when type = "LTEXT" | type = "CTEXT" | type = "RTEXT" then
            do
               opts = self~InternGetRCStyle(style, "ST")
               if type = "CTEXT" then opts = opts || " CENTER";
               else if type = "RTEXT" then opts = opts || " RIGHT";
               else if style~wordpos("SS_LEFTNOWORDWRAP") > 0 then opts = opts || " LEFTNOWRAP";
               self~addText(x, y, w, h, name, opts, id)
            end

            when type = "ICON" then do
               opts = self~InternGetRCStyle(style, "ST")
               self~addImage(id, x, y, w, h, "ICON " || opts)
            end

            when type = "GROUPBOX" then
               self~AddGroupBox(x, y, w, h, name, self~InternGetRCStyle(style, "GB"), id)

            when type = "COMBOBOX" then
               self~AddComboBox(id, name, x, y, w, h, self~InternGetRCStyle(style, "CB"))

            when type = "LISTBOX" then
               self~AddListBox(id, name, x, y, w, h, self~InternGetRCStyle(style, "LB"))

            when type = "SCROLLBAR" then
               self~AddScrollBar(id, x, y, w, h, self~InternGetRCStyle(style, "SB"))

            when type = "AUTORADIOBUTTON" | type = "RADIOBUTTON" then
               self~InternAddRC_RBCB(id, x, y, w, h, name, style, loadOptions, "R")

            when type = "AUTOCHECKBOX" | type = "CHECKBOX" then
               self~InternAddRC_RBCB(id, x, y, w, h, name, style, loadOptions, "C")

            when type = "AUTO3STATE" then
               if style~pos("BS_OWNERDRAW") > 0 then   /* sometimes AUTO3STATE is used for an owner draw button */
                   self~InternAddRCButton(id, x, y, w, h, name, style, loadOptions, "C")  /* if so, treat as normal button */
               else  /* otherwise (not owner drawn) treat as 3state */
                   self~InternAddRC_RBCB(id, x, y, w, h, name, style || " | BS_AUTO3STATE", loadOptions, "C")

            otherwise do
               self~errorFile(f, s)
               return 1
            end
         end /*select 1*/
      end
   end /* do while */
   f~close
   self~ProcessingLoad = 0
   return 0

::method ItemAdd
   use arg id, staticId
   if self~activePtr = 0 then return -2
   if Arg(2,'o') = 1 then do
       id = self~CheckId(id)
       if id = -1 then return -1
   end
   else do
       if staticId = 1 then id = self~CheckId(id)
       else id = self~GetStaticID
   end
   self~DialogItemCount = self~DialogItemCount + 1
   return id


   /* This method adds a push button to the dialog and connect it with */
   /* a message that will be rised if the butten is pushed. */
   /* argument 'options' can be one of: */

::method addButton
   use arg id, x, y, cx, cy, text = "", msgToRise, options
   id = self~itemAdd(id)
   if id < 0 then return id
   options = self~getDefaultOpts(options)
   self~activePtr = UsrAddControl("BUT", self~activePtr, id, x, y, cx, cy, text, options)
   if Arg(7,"o") = 0 then return self~connectButton(id, msgToRise~space(0))
   else return 0


::method addGroupBox
   use arg x, y, cx, cy, text = "", opts, id
   id = self~itemAdd(id, \(arg(7,"o") | id = "ID"))
   if id = -2 then return id
   opts = self~getDefaultOpts(opts)
   self~activePtr = UsrAddControl("GB",self~activePtr, x, y, cx, cy, opts, text, id)
   return 0


::method addStatic
   use strict arg id = (-1), x, y, cx = 0, cy = 0, opts = "TEXT", text = ""
   argA = .array~new
   opts = self~getDefaultOpts(opts)
   argA[1] = x
   argA[2] = y
   argA[3] = cx
   argA[4] = cy
   argA[5] = opts
   argA[6] = id

   select
     when opts~wordpos("TEXT") > 0 then return self~addText(x, y, cx, cy, text, opts, id)

     when opts~wordpos("BITMAP") > 0 | opts~wordpos("METAFILE") > 0 | opts~wordpos("ICON") > 0 then do
        forward message "addImage"
     end

     when opts~wordpos("WHITERECT") > 0 then return self~InternAddStatic(argA, 0)
     when opts~wordpos("GRAYRECT") > 0 then return self~InternAddStatic(argA, 1)
     when opts~wordpos("BLACKRECT") > 0 then return self~InternAddStatic(argA, 2)
     when opts~wordpos("WHITEFRAME") > 0 then return self~InternAddStatic(argA, 3)
     when opts~wordpos("GRAYFRAME") > 0 then return self~InternAddStatic(argA, 4)
     when opts~wordpos("BLACKFRAME") > 0 then return self~InternAddStatic(argA, 5)
     when opts~wordpos("ETCHED") > 0 then return self~InternAddStatic(argA, 6)
     when opts~wordpos("HORZ") > 0 then return self~InternAddStatic(argA, 7)
     when opts~wordpos("VERT") > 0 then return self~InternAddStatic(argA, 8)

     otherwise return self~addText(x, y, cx, cy, text, opts, id)
   end
   -- End select

::method addImage
   use strict arg id, x, y, cx, cy, options = ""
   id = self~ItemAdd(id)
   if id < 0 then return id
   options = self~GetDefaultOpts(options)
   self~activePtr = UsrAddControl("IMG", self~activePtr, id, x, y, cx, cy, options)
   return 0

::method addText
   use arg x, y, cx = 0, cy = 0, text = "", opts = "", id = (-1)

   if cx = 0 | cy = 0 then do
      size = self~getTextSizeDlg(text)
      if cx = 0 then cx = size~width + 2
      if cy = 0 then cy = size~height
   end
   id = self~ItemAdd(id, 1)
   if id = -2 then return id
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("TXT", self~activePtr, x, y, cx, cy, opts, text, id)
   return 0

::method addWhiteRect
   return self~InternAddStatic(Arg(1,"A"), 0)

::method addGrayRect
   return self~InternAddStatic(Arg(1,"A"), 1)

::method addBlackRect
   return self~InternAddStatic(Arg(1,"A"), 2)

::method addWhiteFrame
   return self~InternAddStatic(Arg(1,"A"), 3)

::method addGrayFrame
   return self~InternAddStatic(Arg(1,"A"), 4)

::method addBlackFrame
   return self~InternAddStatic(Arg(1,"A"), 5)

::method addEtchedFrame
   return self~InternAddStatic(Arg(1,"A"), 6)

::method addEtchedHorizontal
   return self~InternAddStatic(Arg(1,"A"), 7)

::method addEtchedVertical
   return self~InternAddStatic(Arg(1,"A"), 8)

::method InternAddStatic private
   use arg arga, type
   if arga~hasindex(6) = 0 then id = self~ItemAdd(-1, 0); else id = self~ItemAdd(arga[6],1)
   if id = -2 then return id
   opts = self~GetDefaultOpts(arga[5])
   self~activePtr = UsrAddControl("FRM", self~activePtr, arga[1], arga[2], arga[3], arga[4], type, opts, id)
   return 0

::method AddEntryLine
   use arg id, fname, x, y, cx, cy, opts
   symbid = id
   id = self~ItemAdd(id)
   if id < 0 then return id
   if Arg(2,'o') = 1 then fname = "DATA"id
   if Arg(6,'o') = 1 then cy = 0
   if cy = 0 then do
      size = self~getTextSizeDlg("Tg")
      cy = size~height + 4
   end
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("EL", self~activePtr, id, x, y, cx, cy, opts)
   if self~AutoDetect = 1 then return self~ConnectEntryLine(symbid, fname)
   else return 0


::method AddPasswordLine
   newarg = Arg(1, 'A')
   if Arg(7,'o') = 1 then newarg[7] = ""
   newarg[7] = newarg[7] || " PASSWORD"
   forward message "AddEntryLine" Arguments (newarg)


::method AddListBox
   use arg id, fname, x, y, cx, cy, opts
   symbid = id
   id = self~ItemAdd(id)
   if id < 0 then return id
   if Arg(2,'o') = 1 then fname = "DATA"id
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("LB", self~activePtr, id, x, y, cx, cy, opts)
   if self~AutoDetect = 1 then do
      if opts~wordpos("MULTI") > 0 then ret = self~ConnectMultiListBox(symbid, fname)
      else ret = self~ConnectListBox(symbid, fname)
   end
   return ret


::method AddComboBox
   use arg id, fname, x, y, cx, cy, opts
   symbid = id
   id = self~ItemAdd(id)
   if id < 0 then return id
   if Arg(2,'o') = 1 then fname = "DATA"id
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("CB", self~activePtr, id, x, y, cx, cy, opts)
   if opts~wordpos("CAT") = 0 & self~AutoDetect = 1 then return self~ConnectComboBox(symbid, fname, opts)
   else return 0


::method AddCheckBox
   use arg id, fname, x, y, cx, cy, text, opts
   symbid = id
   id = self~ItemAdd(id)
   if id < 0 then return id
   if Arg(2,'o') = 1 then fname = text
   if Arg(5, 'o') = 1 then cx = 0
   if Arg(6, 'o') = 1 then cy = 0
   if cx = 0 | cy = 0 then do
      size = self~getTextSizeDlg(text)
      if cx = 0 then cx = size~width + 12
      if cy = 0 then cy = size~height
   end
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("CH", self~activePtr, id, x, y, cx, cy, text, opts)
   if self~AutoDetect = 1 then return self~ConnectCheckBox(symbid, fname)
   else return 0


::method AddRadioButton
   use arg id, fname, x, y, cx, cy, text, opts
   symbid = id
   id = self~ItemAdd(id)
   if id < 0 then return id
   if Arg(2,'o') = 1 then fname = text
   if Arg(5, 'o') = 1 then cx = 0
   if Arg(6, 'o') = 1 then cy = 0
   if cx = 0 | cy = 0 then do
      size =self~getTextSizeDlg(text)
      if cx=0 then cx=size~width + 12
      if cy=0 then cy=size~height
   end
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("RB", self~activePtr, id, x, y, cx, cy, text, opts)
   if opts~wordpos("CAT") = 0 & self~AutoDetect = 1 then return self~ConnectRadioButton(symbid, fname)
   else return 0


::method AddScrollBar
   use arg id, x, y, cx, cy, opts
   id = self~ItemAdd(id)
   if id < 0 then return id
   opts = self~GetDefaultOpts(opts)
   self~activePtr = UsrAddControl("SB", self~activePtr, id, x, y, cx, cy, opts)
   return 0


::method AddRadioGroup
   arr = Arg(1,"A")
   arr[8] = "R"
   forward message "InternAddRBCBGroup" Arguments (arr)

::method AddCheckGroup
   arr = Arg(1,"A")
   arr[8] = "C"
   forward message "InternAddRBCBGroup" Arguments (arr)

::method InternAddRBCBGroup
   use arg startid, x, y, cx, inp, opts, idstat, kind

   if self~activePtr = 0 then return -2
   if self~CheckID(startid) = -1 then return
   if Arg(4, 'o') = 1 then cx = 0
   parse var inp rb inp
   size = self~getTextSizeDlg(rb)
   len = size~width
   stepy = size~height
   lastlen = len; oldy = stepy; stepy = trunc(stepy * 1.5)
   opts = self~GetDefaultOpts(opts)
   nob = opts~wordpos("NOBORDER")
   if nob = 0 then my = y + oldy; else my = y
   newopts = opts~changestr("NOBORDER","")
   if cx \= 0 then len = cx-20
   if Arg(7,'o') = 1 then idstat = self~GetStaticID(idstat)

   i = 0
   do while rb \= ""
      if kind = "R" then do
          if i = 0 then self~AddRadioButton(startid+i,, x+4, my, len+12, stepy, rb, newopts || " GROUP")
          else self~AddRadioButton(startid + i,, x+4, my, len+12, stepy, rb, newopts)
      end
      else do
          self~AddCheckBox(startid + i,, x+4, my, len+12, stepy, rb, newopts)
      end
      my = my + stepy; i = i + 1
      parse var inp rb inp
      if cx = 0 then do
         s = self~getTextSizeDlg(rb)
         len = s~width
         if len > lastlen then lastlen = len
      end
   end
   if cx \= 0 then LastLen = cx-20
   size = .Size~new(lastlen+20, my-y+trunc(oldy / 2))
   if nob = 0 then self~AddGroupBox(x,y,size~width,size~height,"", newopts, idstat)
   return size


::method AddInput
   use arg id, fname, x, y, cx1, cx2, cy, text, opts, idstat
   if self~activePtr = 0 then return -2
   if self~CheckID(id) = -1 then return
   if Arg(2,'o') = 1 then fname = text
   if Arg(5, 'o') = 1 then cx1 = 0
   if Arg(7, 'o') = 1 then cy = 0
   if cy = 0 | cx1 = 0 then do
      size = self~getTextSizeDlg(text)
      if cy = 0 then cy = size~height + 4
      if cx1 = 0 then cx1 = size~width + 4
   end
   if Arg(10,'o') = 1 then idstat = self~GetStaticID(idstat)
   opts = self~GetDefaultOpts(opts)
   ret = self~addText(x, y+2, cx1, cy, text, opts, idstat)
   if ret == 0 then ret = self~AddEntryLine(id, fname, x+cx1+1, y, cx2, cy, opts)
   return ret


::method AddInputGroup
   use arg startid, x, y, cx1, cx2, inp, opts, idstat
   parse var inp rb inp

   if self~activePtr = 0 then return -2
   if self~CheckID(startid) = -1 then return
   if Arg(4, 'o') = 1 then cx1 = 0
   count = 0; ll = 0
   do while rb \= ""
      count = count + 1
      data.count = rb
      if cx1 = 0 then do
         size = self~getTextSizeDlg(rb)
         if size~width > ll then ll = size~width
      end
      parse var inp rb inp
   end
   if cx1 = 0 then cx1 = ll+4
   if Arg(8,'o') = 1 then idstat = self~GetStaticID(idstat)
   is = idstat

   s = self~getTextSizeDlg("Tg")
   stepy = s~height
   opts = self~GetDefaultOpts(opts)
   nob = opts~wordpos("NOBORDER")
   if nob = 0 then my = y + trunc(stepy * 1.25); else my = y
   newopts = opts~changestr("NOBORDER", "")
   do i=1 to count
      self~AddInput(startid + i -1,, x+4, my, cx1-4, cx2-6, stepy+4, data.i, newopts, is)
      if is \= -1 then is = is + 1
      my = my + trunc(stepy * 2.25)
   end
   if nob = 0 then return self~AddGroupBox(x,y,cx1+cx2,my-y,"", newopts, is)
   else return 0


::method AddComboInput
   use arg id, attname, x, y, cx1, cx2, clines, text, opts, idstat

   if self~activePtr = 0 then return -2
   if self~CheckID(id) = -1 then return
   if Arg(2, 'o') = 1 then attname = text
   if Arg(5, 'o') = 1 then cx1 = 0
   if Arg(7, 'o') = 1 then clines = 5

   s = self~getTextSizeDlg(text)
   cy = s~height + 4
   if cx1 = 0 then cx1 = s~width + 4
   if Arg(10,'o') = 1 then idstat = self~GetStaticID(idstat)
   opts = self~GetDefaultOpts(opts)
   ret = self~addText(x, y, cx1, cy, text, opts, idstat)
   if ret == 0 then return self~AddComboBox(id, attname, x+cx1+2, y, cx2, cy*clines, opts)
   else return 0


::method AddInputStem
   use arg startid, x, y, cx1, cx2, inp., opts, idstat

   if self~activePtr = 0 then return -2
   if self~CheckID(startid)= -1 then return
   if Arg(4, 'o') = 1 then cx1 = 0
   count = 0; ll = 0
   do while var("inp."count+1) = 1
      count = count + 1
      if cx1 = 0 then do
         s = self~getTextSizeDlg(inp.count)
         if s~width > ll then ll = s~width
      end
   end
   if cx1 = 0 then cx1 = ll+4

   if idstat = "IDSTAT" then idstat = self~GetStaticID(idstat)
   is = idstat
   s = self~getTextSizeDlg("Tg")
   stepy = s~height

   opts = self~GetDefaultOpts(opts)
   nob = opts~wordpos("NOBORDER")
   if nob = 0 then my = y + trunc(stepy * 1.25); else my = y
   newopts = opts~changestr("NOBORDER", "")
   do i=1 to count
      self~AddInput(startid + i -1,, x+4, my, cx1-4, cx2-6, stepy+4, inp.i, newopts, is)
      if is \= -1 then is = is + 2
      my = my + trunc(stepy * 2.25)
   end
   if nob = 0 then return self~AddGroupBox(x,y,cx1+cx2,my-y,"", newopts, is)
   else return 0


::method AddCheckBoxStem
   arr = Arg(1,"A")
   arr[11] = "C"
   forward message "InternAddRBCBStem" Arguments (arr)

::method AddRadioStem
   arr = Arg(1,"A")
   arr[11] = "R"
   forward message "InternAddRBCBStem" Arguments (arr)

::method InternAddRBCBStem private
   use arg startid, x, y, cxx, inp., max, opts, idstat, fn, fs, kind
   -- Note that font name and font size (fn, fs) are no longer used.  They are
   -- here for backwards compatibility.

   if self~activePtr = 0 then return -2
   if self~CheckID(startid) = -1 then return
   if Arg(4, 'o') = 1 then cxx = 0
   count = 0; ll = 0
   do while var("inp."count+1) = 1
      count = count + 1
      if cxx = 0 then do
         s = self~getTextSizeDlg(inp.count)
         if s~width > ll then ll = s~width
      end
   end
   if var("max") = 0 | max = 0 then max = count
   if cxx = 0 & max \= 1 then cx = ll+20; else cx = cxx
   if Arg(8, 'o') = 1 then idstat = self~GetStaticID(idstat)
   s = self~getTextSizeDlg("Tg")
   stepx = s~width
   oldy = s~height
   mx = 0; highy = 0
   opts = self~GetDefaultOpts(opts)
   nob = opts~wordpos("NOBORDER")
   if nob = 0 then do
      stepy = trunc(oldy * 1.5)
      my = y + oldy
      newopts = opts
   end
   else do
      stepy = oldy
      my = y
      newopts = delword(opts, nob, 1)
   end
   do i=1 to count
      if (i>1) & ((i-1)//max = 0) then do
         highy = my
         if nob = 0 then my = y + oldy; else my = y
         mx = mx + cx
      end
      if max=1 & cxx=0 then do
         s = self~getTextSizeDlg(inp.i)
         cx = s~width + 24
      end
      if kind = "R" then
      do
         if i = 1 then tmpopts = newopts || " GROUP"; else tmpopts = newopts
         self~AddRadioButton(startid + i -1,, x+mx+4, my, cx-8, stepy, inp.i, tmpopts)
      end; else self~AddCheckBox(startid + i -1,, x+mx+4, my, cx-8, stepy, inp.i, newopts)
      my = my + stepy
   end
   if highy = 0 then highy = my
   if nob = 0 then return self~AddGroupBox(x,y,mx+cx, highy-y+(stepy % 2),"", newopts, idstat)
   else return 0


::method AddButtonGroup
   use arg x, y, cx, cy, inp, bottom, opts
   parse var inp bt bid bfnk newinp

   if self~activePtr = 0 then return -2
   if Arg(6,"o") = 1 then bottom = 0
   if cx = 0 | Arg(3,"o") = 1 then cx = 40
   if cy = 0 | Arg(4,"o") = 1 then cy = 12

   opts = self~GetDefaultOpts(opts)
   do while bt \= ""
      if bt~pos("'") > 0 then parse var inp "'"bt"'" bid bfnk newinp
      else if bt~pos('"') > 0 then parse var inp '"'bt'"' bid bfnk newinp
      if bfnk = 0 then bfnk = ""
      self~AddButton(bid, x, y, cx, cy, bt, bfnk, opts)
      n = opts~wordpos("DEFAULT")
      if n \= 0 then opts = opts~delword(n,1)
      if bottom = 0 then y = y + trunc(cy * 1.5); else x = x + trunc(cx * 1.25)
      inp = newinp
      parse var inp bt bid bfnk newinp
   end
   return 0


::method AddOkCancelRightBottom
   return self~AddButtonGroup(self~SizeX-95, self~SizeY - 15,,, "&Ok 1 OK &Cancel 2 CANCEL", 1, "FIX DEFAULT")

::method AddOkCancelLeftBottom
   return self~AddButtonGroup(5, self~SizeY - 15,,, "&Ok 1 OK &Cancel 2 CANCEL", 1, "FIX DEFAULT")

::method AddOkCancelRightTop
   return self~AddButtonGroup(self~SizeX-45, 5,,, "&Ok 1 OK &Cancel 2 CANCEL", 0, "FIX DEFAULT")

::method AddOkCancelLeftTop
   return self~AddButtonGroup(5, 5,,, "&Ok 1 OK &Cancel 2 CANCEL", 0, "FIX DEFAULT")

::method AddIcon
   use arg id, fileName

   id = self~CheckId(id~strip~translate)
   if id < 0 then return id

   fileName = fileName~strip~strip('B', '"')
   return UsrAddResource(self~Adm, "ICO", id, fileName)

::method GetDefaultOpts protected
   use arg opts
   if opts = .NIL | opts = "OPTS" | opts = "OPTIONS" then return ""
   else return opts~translate


::method GetStaticID protected
   return -1


::method StartIt
   use arg icon = 0, modal = 0
   if self~BasePtr = 0 then return 0

   icon = self~resolveIconID(icon)
   if modal <> 0 & modal <> 1 then do
     if modal~translate = "NOTMODAL" then modal = 1
     else modal = 0
   end
   self~DlgHandle = UsrCreateDialog(self~Adm, "PARENT", self~DialogItemCount, 0, self~BasePtr, 0 /* no autodetection */, 1, icon, modal)
   self~activePtr = 0
   if self~DlgHandle = 0 then return 0
   else self~InitDialog
   return self~DlgHandle

::method StopDynamic protected
   self~DialogItemCount = 0
   self~BasePtr = 0
   return ret

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
/* Plain User Dialog Class                                                    */
/*                                                                            */
/******************************************************************************/
/* include the plain base dialog class */

/* This class extends the 'PlainBaseDialog' class */
::class 'PlainUserDialog' subclass PlainBaseDialog public inherit DynamicDialog

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
   forward message "StopDynamic"
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
/* Standard Dialog Classes                                                    */
/*                                                                            */
/******************************************************************************/


/* This class shows a message window for a defined duration */

::class 'TimedMessage' subclass PlainUserDialog public

   /* The class' constructor */

::method init
   expose message title sleeping earlyReply
   use arg message, title, sleeping, earlyReply = .false
   if \ earlyReply~datatype('O') then earlyReply = .false
   if \ sleeping~datatype('W') then sleeping = 0
   self~init:super("")

::method DefineDialog
   expose message sx sy
   self~addText(10,10,sx+2,sy, message)

::method InitAutoDetection
   self~NoAutoDetection

::method execute
   expose message title sleeping earlyReply sx sy

   s = self~getTextSizeDlg(message)
   sx = s~width
   sy = s~height
   self~createCenter(sx+20, sy + 20, title,"NOMENU", , , , 1)
   self~startit
   self~show("SHOWTOP")
   if sleeping >= 0 then
   do
      if earlyReply then reply
      ret = msSleep(sleeping)
      self~stopit
   end


/* This class provides a simple dialog with a titel, a message, one entry line, */
/*   an OK and a Cancel push button */

::class 'InputBox' subclass PlainUserDialog public

::method sx attribute
::method sy attribute

::method init
   expose message title datas. size
   use arg message, title, default, size
   if Arg(3,"o") = 1 | default = "DEFAULT" then datas.101 = ""; else datas.101 = default
   if Arg(4,"o") = 1 | size = "SIZE" then size = 0
   self~init:super(datas.)

::method DefineDialog
   expose message size dlgy
   if size = 0 then size = self~sx
   self~addText(10,10, self~sx+2, self~sy, message)
   self~AddLine(10, self~sy+4+10, size)
   self~AddButtonGroup(self~sx-80, dlgy - 18, 0, 0, "&Ok 1 OK &Cancel 2 CANCEL", 1, "DEFAULT")

::method AddLine
   use arg x, y, l
   self~AddEntryLine(101,"DATA", x, y, l)


::method execute
   expose message title datas. dlgy size

   s = self~getTextSizeDlg(message)
   self~sx = max(s~width,100,size)
   self~sy = s~height

   dlgy = (self~sy+4)*2+36
   dlgx = self~sx + 20
   if dlgx < 110 then dlgx = 110

   self~createCenter(dlgx, dlgy, title,,,,,4)
   self~execute:super("SHOWTOP")
   if self~InitCode = 1 then return datas.101
   else return ""


/* This class is an InputBox dialog which entry line have the not visible flag */

::class 'PasswordBox' subclass InputBox public

::method AddLine
   use arg x, y, l
   self~AddPasswordLine(101, "DATA", x, y, l)


/* This class is an InputBox dialog which entry line allows only numerical data */

::class 'IntegerBox' subclass InputBox public

::method validate
   self~GetDataStem(datas.)
   if datas.101~DataType('W') = 0 then
   do
      call infoDialog "You did not enter numerical data"
      return 0
   end
   else return 1


/* This class is a dialog that provides a title a message and one or more */
/* entry lines. After execution of this dialog you can query the values of the */
/* entry lines. */

::class 'MultiInputBox' subclass PlainUserDialog public

::method init
   expose message title datas. entries. len
   use arg message, title, entries., datas., len
   if Arg(5,"o") = 1 then len = 0
   self~init:super(datas.)

::method DefineDialog
   expose message sx sy len dlgx dlgy entries. maxlen
   self~addText(10,10, sx+2, sy, message)
   self~AddInputStem(101, 10, sy+4+10, maxlen+4, len, entries.)
   self~AddButtonGroup(dlgx-100, dlgy - 18, 0, 0, "&Ok 1 OK &Cancel 2 CANCEL", 1, "DEFAULT")

::method execute
   expose message title sx sy datas. dlgx dlgy len entries. maxlen

   s = self~getTextSizeDlg(message)
   sx = s~width
   sy = s~height
   ll = 0; count = 0

   do while var("entries."count+1) = 1
      count = count + 1
      s = self~getTextSizeDlg(entries.count)
      if s~width > ll then ll = s~width
   end

   if len = 0 then len = max(sx - ll, 50)
   maxlen = ll
   dlgy = count * (sy * 2) + trunc((sy*count)/4) + 56

   if sx >= len + ll + 4 then dlgx = (sx+24)
   else dlgx = (len + ll + 24)
   if dlgx < 110 then dlgx = 110

   self~createCenter(dlgx, dlgy, title,,,,, count*2+5)
   self~execute:super("SHOWTOP")
   return self~InitCode


/* This class provides a dialog with a list box, an OK and a Cancel button. */
/* You the selected item will be returned when the OK pushbutton terminates the dialog */

::class 'ListChoice' subclass PlainUserDialog public

::method sx attribute
::method sy attribute
::method dlgx attribute
::method dlgy attribute
::method lenx attribute
::method leny attribute
::method message attribute

::method init
   expose message title datas. input. lenx leny useVScroll
   use arg message, title, input., lenx, leny, predev
   if Arg(4,"o") = 1 then lenx = 0
   if Arg(5,"o") = 1 then leny = 0
   if Arg(6,"o") = 0 then datas.101 = predev; else datas.101 = ""
   self~init:super(datas.)
   s = self~getTextSizeDlg(message)
   self~sx = s~width
   self~sy = s~height
   self~message = message
   useVScroll = .false

::method DefineDialog
   expose message sx sy lenx leny dlgx dlgy useVScroll
   self~addText(10,10, sx+2, sy, message)
   if useVScroll then style = "VSCROLL"
   else style = "PARTIAL"
   self~AddListBox(101, "DATA", 10, sy+4+10, lenx, leny+4, style)
   self~AddButtonGroup(dlgx-100, dlgy - 18, 0, 0, "&Ok 1 OK &Cancel 2 CANCEL", 1, "DEFAULT")

::method InitDialog
   expose count input.
   do i = 1 to count
      self~AddListEntry(101, input.i)
   end
   self~SetListLine(101, "")


   /* after execution it will return the value of the selected item */

::method execute
   expose message title sx sy datas. input. count lenx leny dlgx dlgy useVScroll

   s = self~getTextSizeDlg(message)
   sx = s~width
   sy = s~height

   ll = 0; count = 0
   do while var("input."count+1) = 1
      count = count + 1
      s = self~getTextSizeDlg(input.count)
      if s~width > ll then ll = s~width
   end

   goodHeight = (count * self~sy) + 1

   if self~lenx = 0 then self~lenx = max(self~sx,ll+5)
   if self~leny = 0 then do
      if count > 15 then do
         self~leny = self~sy * 15
         useVScroll = .true
         self~lenx += self~getScrollBarWidth
      end
      else do
         self~leny = goodHeight
      end
   end
   else do
      if self~leny < goodHeight then do
         self~leny = self~sy * 15
         useVScroll = .true
         self~lenx += self~getScrollBarWidth
      end
   end

   self~dlgy = self~leny+(self~sy+4)+34

   if self~sx >= self~lenx then self~dlgx = self~sx + 20; else self~dlgx = self~lenx + 20
   if self~dlgx < 110 then self~dlgx = 110

   self~createCenter(self~dlgx, self~dlgy, title,,,,, 4)
   self~execute:super("SHOWTOP")

   if self~InitCode = 1 then return datas.101
   else return ""

::method getScrollBarWidth private
   SM_CXVSCROLL = 20
   size = .Size~new
   size~width = .DlgUtil~getSystemMetrics(SM_CXVSCROLL)
   size~width = trunc(size~width * self~factorX)
   if size~width > 15 then return size~width + 2
   else return size~width


::class 'MultiListChoice' subclass ListChoice public


::method DefineDialog
   self~addText(10,10, self~sx+2, self~sy, self~message)
   self~AddListBox(101, "DATA", 10, self~sy+4+10, self~lenx, self~leny+4, "MULTI VSCROLL TABULATORS")
   self~AddButtonGroup(self~dlgx-100, self~dlgy - 18, 0, 0, "&Ok 1 OK &Cancel 2 CANCEL", 1, "DEFAULT")


/* This class is a dialog with a group of one or more check boxes. */

::class 'CheckList' subclass PlainUserDialog public

  /* Entries is a stem variable with all the titles for the check boxes */
  /* Datas is a stem variable you can use to preselect the check boxes */
  /* 'Datas.i=1' means there will be a check mark on the i-th box */
::method Init
   expose message title datas. entries. len max
   use arg message, title, entries., datas., len, max
   if Arg(5,"o") = 1 then len = 0
   if Arg(6,"o") = 1 then max = 0
   self~init:super(datas.)

::method DefineDialog
   expose message sx sy len dlgx dlgy entries. placex max
   self~addText(10,10, sx+2, sy, message)
   self~AddCheckBoxStem(101, placex, sy+4+10, len, entries., max)
   self~AddButtonGroup(dlgx-100, dlgy - 18, 0, 0, "&Ok 1 OK &Cancel 2 CANCEL", 1, "DEFAULT")

::method Execute
   expose message title sx sy datas. dlgx dlgy len entries. placex max

   s = self~getTextSizeDlg(message)
   sx = s~width
   sy = s~height
   ll = 0; count = 0
   do while var("entries."count+1) = 1
      count = count + 1
      s = self~getTextSizeDlg(entries.count)
      if s~width > ll then ll = s~width
   end

   if max = 0 then max = count
   dlgy = max * trunc(sy * 1.5) + sy + 60
   placex = 10
   columns = (count % max)
   if  (count // max) > 0 then columns = columns +1

   if sx >= (ll + 20) * columns then
   do
      dlgx = (sx+24)
      if columns = 1 then placex = trunc((dlgx - ll - 20) /2)
   end
   else dlgx = (ll + 20) * columns + 24
   if dlgx < 110 then dlgx = 110

   self~createCenter(dlgx, dlgy, title,,,,, count+5)
   self~execute:super("SHOWTOP")
   return self~InitCode


/* The dialog this class shows has a group of radio buttons. That is you only can */
/* select one item at the same time. */

::class 'SingleSelection' subclass PlainUserDialog public

  /*  */
::method Init
   expose message title entries. len datas. prev max
   use arg message, title, entries., prev, len, max
   if Arg(5,"o") = 1 then len = 0
   if Arg(6,"o") = 1 then max = 0
   if Arg(4,"o") = 1 then prev = 1
   self~init:super(datas.)

::method DefineDialog
   expose message sx sy len dlgx dlgy entries. placex max
   self~addText(10,10, sx+2, sy, message)
   self~AddRadioStem(101, placex, sy+4+10, len, entries., max)
   self~AddButtonGroup(dlgx-100, dlgy - 18, 0, 0, "&Ok 1 OK &Cancel 2 CANCEL", 1, "DEFAULT")

::method Execute
   expose message title sx sy datas. dlgx dlgy len entries. placex prev max

   s = self~getTextSizeDlg(message)
   sx = s~width
   sy = s~height
   ll = 0; count = 0
   do while var("entries."count+1) = 1
      count = count + 1
      s = self~getTextSizeDlg(entries.count)
      if s~width > ll then ll = s~width
   end

   if max = 0 then max = count
   dlgy = max * trunc(sy * 1.5) + sy + 60
   placex = 10
   columns = (count % max)
   if  (count // max) > 0 then columns = columns +1

   if sx >= (ll + 20) * columns then
   do
      dlgx = (sx+24)
      if columns = 1 then placex = trunc((dlgx - ll - 20) /2)
   end
   else dlgx = (ll + 20) * columns + 24
   if dlgx < 110 then dlgx = 110

   do i = 101 to 100+count
      datas.i = 0
   end
   pr = (100+prev)
   datas.pr = 1

   self~createCenter(dlgx, dlgy, title,,,,, count+5)
   self~execute:super("SHOWTOP")
   if self~InitCode = 1 then
   do
     do i = 101 to 100+count
      if datas.i = 1 then return i-100
     end
     return 0
   end
   else
    return ""






