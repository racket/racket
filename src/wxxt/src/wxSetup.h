/*								-*- C++ -*-
 *
 * Purpose: Window library configuration file. Note: you may need to edit
 *          the main makefile after you have edited this.
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

#ifndef wxSetup_h
#define wxSetup_h

#ifdef __GNUG__
#pragma interface
#endif

/*
* Enables debugging: memory tracing, assert, etc.
*/
#define DEBUG 0
/*
 * Use PostScript device context
*/
#define USE_POSTSCRIPT 1
/*
 * Use font metric files in GetTextExtent for wxPostScriptDC
 */
#define USE_AFM_FOR_POSTSCRIPT 1
/*
 * Use Metafile and Metafile device context
*/
#define WX_NORMALIZED_PS_FONTS 1
/* MATTHEW
 * Use consistent PostScript fonts for AFM and printing (!)
*/
#define USE_METAFILE 0
/*
 * Use Form panel item placement
*/
#define USE_FORM 0
/*
 * Use Interprocess communication
 * Use wxWindows help facility (needs IPC)
*/
#define USE_IPC 0
#define USE_HELP 0
/*
 * Use enhanced dialog
*/
#define USE_ENHANCED_DIALOG 0
/*
 * Use wxGetResource & wxWriteResource (change .Xdefaults)
*/
#define USE_RESOURCES 1
/*
 * Use clipboard
*/
#define USE_CLIPBOARD 1
/*
 * Use splines
*/
#define USE_SPLINES 1
/*
 * Use toolbar
*/
#define USE_TOOLBAR 0
/*
 * Use gauge item
*/
#define USE_GAUGE 1
/*
 * Use static item classes
*/
#define USE_EXTENDED_STATICS 0
/*
 * Use virtual list box item
*/
#define USE_VLBOX 0
/*
 * Use scrollbar item
*/
#define USE_SCROLLBAR 0
/*
 * Support XPM package in wxBitmap and wxIcon.
 * XPM_INCLUDE specifies, where to find the include-file for XPM
 * If you want to use it, you have to link in the XPM library to your applications.
*/
#define USE_XPM 1
#define XPM_INCLUDE "../../contrib/xpm/include/xpm.h"
/*
 * Support GIF and BMP reading.
*/
#define USE_IMAGE_LOADING_IN_X 1
#define WXIMAGE_INCLUDE "../../utils/image/src/wx_image.h"
/*
 * Use wxWindows resource loading (.wxr-files)
*/
#define USE_WX_RESOURCES 0
/*
 * Use the GNU wxString class instead of the AIAI class
*/
#define USE_GNU_WXSTRING 0
/*
 * Use the document/view architecture
*/
#define USE_DOC_VIEW_ARCHITECTURE 0
/*
 * Use the print/preview architecture
*/
#define USE_PRINTING_ARCHITECTURE 0
/*
 * Disable this if your compiler can't cope
 * with omission of prototype parameters.
*/
#define REMOVE_UNUSED_ARG 1
/*
 * Normalize X drawing code to behave exactly as MSW.
*/
#define WX_STANDARD_GRAPHICS 1
/*
 * Use wxTree
 */
#define USE_WXTREE 0
/*
 * Use wxGraph
 */
#define USE_WXGRAPH 0
/*
 * Use Prolog IO
 */
#define USE_PROLOGIO 0
/*
 * Use Remote Procedure Call (Uses IPC and PROLOGIO)
 */
#define USE_RPC 0

/* MATTHEW */
#define WXGARBAGE_COLLECTION_ON 1
                                 
/*
 * Adjustions for Imakefile, allows skipping during compilation
 */
#ifdef InImakefile
#if USE_GNU_WXSTRING
	WXSTRING=DataStructures/wxstrgnu.o DataStructures/wxregex.o
#else
	WXSTRING=DataStructures/wxstring.o
#endif
#if USE_RPC
	RPC=PrologIO/prorpc.o
#	undef  USE_IPC
#	define USE_IPC 1
#	undef  USE_PROLOGIO
#	define USE_PROLOGIO 1
#endif
#if USE_PROLOGIO
	PROLOGIO=PrologIO/read.o PrologIO/parser.o
#endif
#if USE_POSTSCRIPT
	POSTSCRIPTDC=DeviceContexts/PSDC.o
	POSTSCRIPTDLG=Dialogs/PSDialog.o
#endif
#if USE_METAFILE
	METAFILEDC=DeviceContexts/MetafileDC.o
#endif
#if USE_FORM
	FORM=HighLevel/wb_form.o
#endif
#if USE_HELP
	HELP=IPC-Classes/Help.o
#	undef  USE_IPC
#	define USE_IPC 1
#endif
#if USE_IPC
	IPC=IPC-Classes/Client.o IPC-Classes/IPC.o IPC-Classes/Server.o IPC-Classes/Sock.o
#endif
#if USE_ENHANCED_DIALOG
	ENHDIALOGBOX=Windows/EnhDialogBox.o
#endif
#if USE_RESOURCES
	XRESOURCES=Utilities/Resources.o
#endif
#if USE_CLIPBOARD
	CLIPBOARD=Misc/Clipboard.o
#endif
#if USE_TOOLBAR
	TOOLBAR=Windows/ToolBar.o
#endif
#if USE_GAUGE
	GAUGE=Windows/Gauge.o
#endif
#if USE_EXTENDED_STATICS
	STATICITEMS=Windows/StaticItems.o
#endif
#if USE_VLBOX
	VIRTLISTBOX=Windows/VirtListBox.o
#endif
#if USE_SCROLLBAR
	SCROLLBAR=Windows/Scrollbar.o
#endif
#if USE_WX_RESOURCES
	WXRESOURCES=Windows/Resources.o
#endif
#if USE_DOC_VIEW_ARCHITECTURE
	DOCVIEW=HighLevel/DocView.o
#endif
#if USE_PRINTING_ARCHITECTURE
	PRINTPREVIEW=HighLevel/PrintPreview.o
#endif
#if USE_WXGRAPH
	WXGRAPH=DrawingClasses/wxgraph.o
#endif
#if USE_WXTREE
	WXTREE=DrawingClasses/wxtree.o
#endif

#endif /* InImakefile */

#endif /* wxSetup_h */
