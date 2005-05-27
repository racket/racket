/*								-*- C++ -*-
 *
 * Purpose: wxWindows Xt-port main include file
 *
 * Authors: Markus Holzem and Julian Smart
 *
 * Copyright: (C) 2004-2005 PLT Scheme, Inc.
 * Copyright: (C) 1995, AIAI, University of Edinburgh (Julian)
 * Copyright: (C) 1995, GNU (Markus)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

//=============================================================================
// basic includes for wxWindows
//=============================================================================

#ifndef wx_h
#define wx_h

#if   defined (Uses_XtIntrinsicP)
#	include <X11/IntrinsicP.h>
#	define  Have_X_Types
#	define  Have_Xt_Types
#elif defined (Uses_XtIntrinsic)
#	include <X11/Intrinsic.h>
#	define  Have_X_Types
#	define  Have_Xt_Types
#elif defined (Uses_XLib)
#	include <X11/Xlib.h>
#	include <X11/Xutil.h>
#	include <X11/Xresource.h>
#	define  Have_X_Types
#endif

// standard include files needed in addition to wx include files
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// standard wx include files
#include <wxSetup.h>
#include <wxVersion.h>

#include <wxDefines.h>

#include <Application/GlobalData.h>
#include <Dialogs/Dialogs.h>
#include <Utilities/Utilities.h>

#include "../../wxcommon/wxGC.h"

#endif // wx_h

//=============================================================================
// If the application doesn't use Uses_..., emulate the AIAI behaviour
//=============================================================================

#ifdef _NO_USES_DEFINES
#	define Uses_wxApp
#	define Uses_wxButton
#	define Uses_wxCanvas
#	define Uses_wxCanvasDC
#	define Uses_wxCheckBox
#	define Uses_wxChoice
#	define Uses_wxDC
#	define Uses_wxDialogBox
#	define Uses_wxEvent
#	define Uses_wxFrame
#	define Uses_wxGDI
#	define Uses_wxGauge
#	define Uses_wxGroupBox
#	define Uses_wxLayout
#	define Uses_wxList
#	define Uses_wxListBox
#	define Uses_wxMemoryDC
#	define Uses_wxMenu
#	define Uses_wxMenuBar
#	define Uses_wxMessage
#	define Uses_wxMultiText
#	define Uses_wxObject
#	define Uses_wxPanel
#	define Uses_wxPanelDC
#	define Uses_wxPathList
#	define Uses_wxPostScriptDC
#	define Uses_wxRadioBox
#	define Uses_wxSlider
#	define Uses_wxText
#	define Uses_wxTextWindow
#	define Uses_wxWindow
#endif

//=============================================================================
// class dependencies
//=============================================================================

//--- application -------------------------------------------------------------

#if defined(Uses_wxApp)
#	define Uses_wxObject
#endif

//--- PrologIO RPC ------------------------------------------------------------

#if defined(Uses_wxRPC)
#	define Uses_wxIPC
#	define Uses_wxClient
#	define Uses_wxServer
#	define Uses_wxPrologIO
#endif

#if defined(Uses_wxPrologIO)
#	define Uses_wxHashTable
#	define Uses_wxList
#endif

//--- data structures ---------------------------------------------------------

#if defined(Uses_wxTypeDef)
#	define Uses_wxTypeTree
#endif

#if defined(Uses_wxTypeTree)
#	define Uses_wxHashTable
#endif

/* MATTHEW */
#if defined(Uses_wxClipboard)
#	define Uses_wxObject
#       define Uses_wxStringList
#endif

#if defined(Uses_wxNode) || defined(Uses_wxStringList) \
 || defined(Uses_wxPathList) || defined(Uses_wxHashTable) \
 || defined(Uses_wxPrintSetup)
#	define Uses_wxList
#endif

#if defined(Uses_wxList) \
 || defined(Uses_wxDebugContext) || defined(Uses_wxDebugStreamBuf)
#	define Uses_wxObject
#endif

#if defined(Uses_wxString) && !USE_GNU_WXSTRING
#	define Uses_wxObject
#endif

//--- windows -----------------------------------------------------------------

//--- items ---
#if defined(Uses_wxMultiText) || defined(Uses_wxTextWindow)
#	define Uses_wxText
#endif

#if defined(Uses_wxButton) \
 || defined(Uses_wxCanvas) \
 || defined(Uses_wxCheckBox) \
 || defined(Uses_wxChoice) \
 || defined(Uses_wxGauge) \
 || defined(Uses_wxGroupBox) \
 || defined(Uses_wxListBox) \
 || defined(Uses_wxMenuBar) \
 || defined(Uses_wxMessage) \
 || defined(Uses_wxRadioBox) \
 || defined(Uses_wxScrollBar) \
 || defined(Uses_wxSlider) \
 || defined(Uses_wxText) \
 || defined(Uses_wxToolBar) \
 || defined(Uses_wxVirtListBox)
#	define Uses_wxItem
#endif

#if defined(Uses_wxStaticItems)
#	define Uses_wxObject
#endif

#if defined(Uses_wxMenuBar)
#	define Uses_wxMenu
#endif

#if defined(Uses_wxItem)
#	define Uses_wxPanel
#	define Uses_wxWindow
#endif

//--- panels ---
#if defined(Uses_wxDialogBase) || defined(Uses_wxEnhDialogBox)
#	define Uses_wxDialogBox
#endif

#if defined(Uses_wxDialogBox)
#	define Uses_wxFrame
#endif

#if defined(Uses_wxFrame)
#	define Uses_wxPanel
#endif

#if defined(Uses_wxPanel)
#	define Uses_wxWindow
#endif

//--- window ---
#if defined(Uses_wxWindow)
#	define Uses_wxEvtHandler
#	define Uses_wxWindowDC
#	define Uses_wxGDI
#endif

#if defined(Uses_wxLayout) || defined(Uses_wxMenu)
#	define Uses_wxEvent
#	define Uses_wxObject
#endif

//--- device contexts ---------------------------------------------------------

#if defined(Uses_wxCanvasDC) || defined(Uses_wxPanelDC) \
 || defined(Uses_wxMemoryDC)
#	define Uses_wxWindowDC
#endif

#if defined(Uses_wxWindowDC) || defined(Uses_wxPostScriptDC) || defined(Uses_MetafileDC)
#	define Uses_wxDC
#	define Uses_wxGDI
#endif

#if defined(Uses_wxDC)
#	define Uses_wxObject
#	define Uses_wxColour
#endif	

//--- event handling ----------------------------------------------------------

#if defined(Uses_wxCommandEvent) || defined(Uses_wxKeyEvent) \
 || defined(Uses_wxMouseEvent)   || defined(Uses_wxEvent) \
 || defined(Uses_wxEvtHandler)
#	define Uses_wxEvent
#	define Uses_wxObject
#endif

//--- GDI classes -------------------------------------------------------------

#if defined(Uses_wxGDI)
#	define Uses_wxBitmap
#	define Uses_wxColour
#	define Uses_wxFont
#	define Uses_wxPenBrush
#endif

#if defined(Uses_wxBitmap) || defined(Uses_wxCursor) || defined(Uses_wxIcon) \
 || defined(Uses_wxGDIList)
#	define Uses_wxList
#	define Uses_wxBitmap
#	define Uses_wxObject
#endif

#if defined(Uses_wxBrush) || defined(Uses_wxPen) || defined(Uses_wxPenBrush) \
 || defined(Uses_wxPenList) || defined(Uses_BrushList)
#	define Uses_wxPenBrush
#	define Uses_wxColour
#	define Uses_wxList
#	define Uses_wxObject
#endif

#if defined(Uses_wxFont) || defined(Uses_wxFontList)
#	define Uses_wxList
#	define Uses_wxObject
#       define Uses_wxFontNameDirectory
#endif

#if defined(Uses_wxFontNameDirectory)
#	define Uses_wxObject
#	define Uses_wxList
#       define Uses_wxHashTable
#endif

#if defined(Uses_wxColour) || defined(Uses_wxColourMap) \
 || defined(Uses_wxColourDatabase)
#	define Uses_wxList
#	define Uses_wxObject
#endif

//--- high level classes ------------------------------------------------------

#if defined(Uses_wxForm)
#	define Uses_wxObject
#endif

//--- IPC classes -------------------------------------------------------------

#if defined(Uses_wxHelp)
#	define Uses_wxClient
#endif

#if defined(Uses_wxClient) || defined(Uses_wxServer) || defined(Uses_wxIPC)
#	define Uses_wxIPC
#	define Uses_wxList
#	define Uses_wxObject
#endif

//--- miscellaneous classes ---------------------------------------------------

#if defined(Uses_wxTimer)
#	define Uses_wxObject
#endif

//=============================================================================
// include files depending on Uses defines
//=============================================================================

//--- data structures ---------------------------------------------------------

#if defined(Uses_wxObject)
#	include "DataStructures/Object.h"
#endif
#if defined(Uses_wxList)
#	include "../../wxcommon/wx_list.h"
#endif
#if defined(Uses_wxHashTable)
#	include "../../wxcommon/wx_hash.h"
#endif
#if defined(Uses_wxTypeTree)
#	include "DataStructures/TypeTree.h"
#endif
#if defined(Uses_wxPrintSetup)
#	include "DataStructures/PrintSetup.h"
#endif

//--- application -------------------------------------------------------------

#if defined(Uses_wxApp)
#	include "Application/AppMain.h"
#endif

//--- event handling ----------------------------------------------------------

#if defined(Uses_wxEvent)
#	include "EventHandling/wx_sysev.h"
#	include "EventHandling/wx_stdev.h"
#endif

#if defined(Uses_wxEvtHandler)
#	include "EventHandling/EvtHandler.h"
#endif

//--- GDI classes -------------------------------------------------------------

#if defined(Uses_wxBitmap)
#	include "GDI-Classes/Bitmap.h"
#endif

#if defined(Uses_wxColour)
#	include "GDI-Classes/Colour.h"
#endif

#if defined(Uses_wxFont)
#	include "GDI-Classes/Font.h"
#endif

#if defined(Uses_wxFontNameDirectory)
#	include "GDI-Classes/FontDirectory.h"
#endif

#if defined(Uses_wxPenBrush)
#	include "GDI-Classes/Pen+Brush.h"
#endif

#if defined(Uses_wxRegion)
#	include "../../wxcommon/Region.h"
#endif

//--- device contexts ---------------------------------------------------------

#if defined(Uses_wxDC)
#	include "DeviceContexts/DC.h"
#endif

#if defined(Uses_wxWindowDC)
#	include "DeviceContexts/WindowDC.h"
#endif

#if defined(Uses_wxMemoryDC)
#	include "DeviceContexts/MemoryDC.h"
#endif

#if USE_METAFILE && defined(Uses_wxMetaFileDC)
#	include "DeviceContexts/MetaFileDC.h"
#endif

#if USE_POSTSCRIPT && defined(Uses_wxPostScriptDC)
#	include "DeviceContexts/PSDC.h"
#endif

//--- high level classes ------------------------------------------------------

#if USE_DOC_VIEW_ARCHITECTURE && defined(Uses_wxDocView)
#	include "HighLevel/DocView.h"
#endif

#if USE_FORM && defined(Uses_wxForm)
#	include "HighLevel/wx_form.h"
#endif

#if USE_PRINTING_ARCHITECTURE && defined(Uses_wxPrintPreview)
#	include "HighLevel/PrintPreview.h"
#endif

//--- IPC classes -------------------------------------------------------------

#if USE_IPC && defined(Uses_wxIPC)
#	include "IPC-Classes/IPC.h"
#endif

#if USE_IPC && defined(Uses_wxClient)
#	include "IPC-Classes/Client.h"
#endif

#if USE_IPC && defined(Uses_wxServer)
#	include "IPC-Classes/Server.h"
#endif

#if USE_HELP && defined(Uses_wxHelp)
#	include "IPC-Classes/Help.h"
#endif

//--- PrologIO RPC ------------------------------------------------------------

#if USE_PROLOGIO && defined(Uses_wxPrologIO)
#	include "PrologIO/read.h"
#endif

#if USE_RPC && defined(Uses_wxRPC)
#	include "PrologIO/prorpc.h"
#endif

//--- miscellaneous classes ---------------------------------------------------

#if USE_CLIPBOARD && defined(Uses_wxClipboard)
#	include "Misc/Clipboard.h"
#endif

#if defined(Uses_wxTimer)
#	include "Misc/Timer.h"
#endif

//--- windows -----------------------------------------------------------------

#if defined(Uses_wxLayout)
#	include "Windows/Layout.h"
#endif

#if defined(Uses_wxWindow)
#	include "Windows/Window.h"
#endif

//--- panels ---
#if defined(Uses_wxPanel)
#	include "Windows/Panel.h"
#endif

#if defined(Uses_wxFrame)
#	include "Windows/Frame.h"
#endif

#if defined(Uses_wxDialogBox)
#	include "Windows/DialogBox.h"
#endif

#if USE_ENHANCED_DIALOG && defined(wx_UsesEnhDialogBox)
#	include "Windows/EnhDialogBox.h"
#endif

//--- items ---
#if defined(Uses_wxItem)
#	include "Windows/Item.h"
#endif

#if defined(Uses_wxButton)
#	include "Windows/Button.h"
#endif

#if defined(Uses_wxCanvas)
#	include "Windows/Canvas.h"
#endif

#if defined(Uses_wxCheckBox)
#	include "Windows/CheckBox.h"
#endif

#if defined(Uses_wxChoice)
#	include "Windows/Choice.h"
#endif

#if USE_GAUGE && defined(Uses_wxGauge)
#	include "Windows/Gauge.h"
#endif

#if defined(Uses_wxListBox)
#	include "Windows/ListBox.h"
#endif

#if defined(Uses_wxMenu)
#	include "Windows/Menu.h"
#endif

#if defined(Uses_wxMenuBar)
#	include "Windows/MenuBar.h"
#endif

#if defined(Uses_wxMessage)
#	include "Windows/Message.h"
#endif

#if defined(Uses_wxRadioBox)
#	include "Windows/RadioBox.h"
#endif

#if USE_SCROLLBAR && defined(Uses_wxScrollBar)
#	include "Windows/ScrollBar.h"
#endif

#if defined(Uses_wxSlider)
#	include "Windows/Slider.h"
#endif
