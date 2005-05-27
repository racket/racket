/*
 * File:	wx_itemp.h
 * Purpose:	Panel item private declarations
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	April 1995
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1995, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wx_itemph
#define wx_itemph

#include "wx_item.h"
#include "wx_privt.h"
#include "wx_utils.h"

#include "fafa.h"
#include "fafapriv.h" //added by Chubraev 

#define STATIC_CLASS     "STATIC"
#define LSTATIC_CLASS    L"STATIC"
#define STATIC_FLAGS     (SS_LEFT|WS_CHILD)
#define CHECK_FLAGS      (BS_AUTOCHECKBOX|WS_TABSTOP|WS_CHILD)
#define RADIO_CLASS      "wxBUTTON"
#define RADIO_FLAGS      (BS_RADIOBUTTON|WS_CHILD)
#define RADIO_SIZE       20
#define PURE_WINDOWS

#define BITCHECK_FLAGS   (FB_BITMAP|FC_BUTTONDRAW|FC_DEFAULT)
#define BITRADIO_FLAGS   (FC_BUTTONDRAW|FB_BITMAP|FC_RADIO|WS_CHILD)

#define MEANING_CHARACTER '0'
#define EDIT_CONTROL_FACTOR (15.0/10.0)
                                        // Scale font to get edit control height
#if !defined(APIENTRY)	// NT defines APIENTRY, 3.x not
#define APIENTRY FAR PASCAL
#endif
 
#define _EXPORT /**/

// Generic subclass proc, for panel item moving/sizing and intercept
// EDIT control VK_RETURN messages
extern LONG APIENTRY _EXPORT
  wxSubclassedGenericControlProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

// Find maximum size of window/rectangle
extern void wxFindMaxSize(HWND hwnd, RECT *rect);

// List of controls
extern wxNonlockingHashTable *wxControlHandleList;
// List of scrollbar controls
extern wxList wxScrollBarList;
// The MakeProcInstance version of the function wxSubclassedGenericControlProc
extern FARPROC wxGenericControlSubClassProc;
extern char *wxBuffer;
extern HINSTANCE wxhInstance;

wxItem *wxFindControlFromHandle(HWND hWnd);
void wxAddControlHandle(HWND hWnd, wxItem *item);
void wxRemoveControlHandle(HWND hWnd);

#endif
