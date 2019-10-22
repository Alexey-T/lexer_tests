/*----------------------------------------------------------------------------*/
/*                                                                            */
/* Copyright (c) 1995, 2004 IBM Corporation. All rights reserved.             */
/* Copyright (c) 2005-2006 Rexx Language Association. All rights reserved.    */
/*                                                                            */
/* This program and the accompanying materials are made available under       */
/* the terms of the Common Public License v1.0 which accompanies this         */
/* distribution. A copy is also available at the following address:           */
/* http://www.oorexx.org/license.html                          */
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
/****************************************************************************/
/* Name: EMPVALID.REX                                                       */
/* Type: Object REXX Script                                                 */
/*                                                                          */
/****************************************************************************/


dlg = .MyDialogClass~new
if dlg~InitCode <> 0 then exit
if dlg~Load("EMPLOYE2.RC", 100) \= 0 then exit
dlg~Execute("SHOWTOP")
dlg~deinstall
::requires "OODPLAIN.CLS"

::class MyDialogClass subclass PlainUserDialog
::method InitDialog
    self~City = "New York"
    self~Male = 1
    self~Female = 0
    self~AddComboEntry(22, "Munich")
    self~AddComboEntry(22, "New York")
    self~AddComboEntry(22, "San Francisco")
    self~AddComboEntry(22, "Stuttgart")
    self~AddListEntry(23, "Business Manager")
    self~AddListEntry(23, "Software Developer")
    self~AddListEntry(23, "Broker")
    self~AddListEntry(23, "Police Man")
    self~AddListEntry(23, "Lawyer")
    self~ConnectButton(10, "Print")   /* connect button 10 with a method */

::method Print
    self~GetData
    if self~Male = 1 then title = "Mr."; else title = "Ms."
    if self~Married = 1 then addition = " (married) "; else addition = ""
    call infoDialog title self~Name addition || "A"x || "City:" self~City || "A"x ||,
                     "Profession:" self~Profession

::method Validate
    if self~GetValue(21)~strip = "" then
    do
        call infoDialog "An unnamed employee is not accepted!"
        return 0       /* dialog annot be closed */
    end; else
        return 1       /* dialog can be closed */
