/*
 * File:	common.h
 * Purpose:	Declarations/definitions common to all wx source files
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2010 PLT Scheme Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 */

#ifndef wxb_commonh
#define wxb_commonh

/* We don't want all those "deprecated" messages: */
#ifndef WX_KEEP_DEPRECATED_WARNINGS
# include <AvailabilityMacros.h>
# undef DEPRECATED_ATTRIBUTE
# define DEPRECATED_ATTRIBUTE /**/
#endif

#ifdef OS_X
# include <Carbon/Carbon.h>
#else
# include <Carbon.h>
# include <string.h>
#endif

#ifndef macintosh
# define macintosh
#endif
#undef GUSI
#define DEBUG_NEW 0

#include "wx_setup.h"
#include "wx_ver.h"

#define WXUNUSED(x) /* empty */
#define CheckMemOK(v) (v ? 0 : wxOutOfMemory())
extern void *wxOutOfMemory();

#ifndef wx_mac
# define wx_mac
#endif

#ifdef wx_mac
typedef int Bool;
# define Bool_DEFINED
#endif

#ifndef TRUE
# define TRUE  1
# define FALSE 0
#endif

typedef short int WXTYPE;

// Styles for wxListBox - Yes, all in Multiple , and nothing in Style
#define wxMULTIPLE_MASK     0x03
#define wxSINGLE            0x00
#define wxMULTIPLE          0x01
#define wxEXTENDED          0x02

#define wxSB_MASK           0x08
#define wxNEEDED_SB         0x00
#define wxALWAYS_SB         0x08

// Frame/dialog/subwindow style flags
#define wxVSCROLL           0x00000001
#define wxHSCROLL           0x00000002
#define wxNO_CAPTION           0x00000004

// Frame/dialog style flags
#define wxSTAY_ON_TOP       0x00000008
#define wxICONIZE           0x00000010
#define wxMINIMIZE          wxICONIZE
#define wxMAXIMIZE          0x00000020
#define wxSDI               0x00000040
#define wxMDI_PARENT        0x00000080
#define wxMDI_CHILD         0x00000100
#define wxNO_THICK_FRAME       0x00000200
#define wxNO_SYSTEM_MENU       0x00000400
#define wxMINIMIZE_BOX      0x00000800
#define wxMAXIMIZE_BOX      0x00001000
#define wxTOOLBAR_BUTTON    0x00002000
#define wxNOT_AS_SHEET      0x00004000
#define wxRESIZE_BOX       wxMAXIMIZE_BOX
#define wxNO_RESIZE_BORDER	    0x00800000
#define wxHIDE_MENUBAR	    0x01000000
#define wxMETAL             0x02000000
#define wxFLOAT_FRAME       0x04000000

#define wxDEFAULT_FRAME    (wxRESIZE_BORDER | wxMINIMIZE_BOX | wxMAXIMIZE_BOX | wxTHICK_FRAME | wxSYSTEM_MENU | wxCAPTION)

// Subwindow style flags
#define wxBORDER           0x00000040
#define wxVERTICAL_LABEL   0x00000200
#define wxHORIZONTAL_LABEL 0x00000400
#define wxINVISIBLE        0x00000800
#define wxGL_CONTEXT       0x00010000
#define wxNO_AUTOCLEAR     0x00040000
#define wxCONTROL_BORDER   0x00080000
#define wxTRANSPARENT_WIN  0x00100000
#define wxRESIZE_CORNER    0x00200000
#define wxCOMBO_SIDE       0x00400000
#define wxAS_CONTROL       0x00800000
#define wxNEVER_FOCUS      0x01000000

# define wxFLAT            0x00000100

#define wxDEFAULT_DIALOG_STYLE	(wxSYSTEM_MENU|wxCAPTION|wxTHICK_FRAME)

// GDI descriptions

enum {
// Text font families
  wxDEFAULT    = 70,
  wxDECORATIVE,
  wxROMAN,
  wxSCRIPT,
  wxSWISS,
  wxMODERN,
  wxTELETYPE,  /* @@@@ */
  wxSYSTEM,
  wxSYMBOL,

// Proportional or Fixed width fonts (not yet used)
  wxVARIABLE   = 80,
  wxFIXED,

  wxNORMAL     = 90,
  wxLIGHT,
  wxBOLD,
// Also wxNORMAL for normal (non-italic text)
  wxITALIC,
  wxSLANT,

// Pen styles
  wxSOLID      =   97,
  wxDOT,
  wxLONG_DASH,
  wxSHORT_DASH,
  wxDOT_DASH,
  wxUSER_DASH,
  wxXOR_DOT,
  wxXOR_LONG_DASH,
  wxXOR_SHORT_DASH,
  wxXOR_DOT_DASH,

  wxTRANSPARENT,

// Brush & Pen Stippling. Note that a stippled pen cannot be dashed!!
// Note also that stippling a Pen IS meaningfull, because a Line is
// drawn with a Pen, and without any Brush -- and it can be stippled.
  wxSTIPPLE =          110,
  wxBDIAGONAL_HATCH,
  wxCROSSDIAG_HATCH,
  wxFDIAGONAL_HATCH,
  wxCROSS_HATCH,
  wxHORIZONTAL_HATCH,
  wxVERTICAL_HATCH,
#define IS_HATCH(s)	((s)>=wxBDIAGONAL_HATCH && (s)<=wxVERTICAL_HATCH)
  wxOPAQUE_STIPPLE,
  wxPANEL_PATTERN,

  wxJOIN_BEVEL =     120,
  wxJOIN_MITER,
  wxJOIN_ROUND,

  wxCAP_ROUND =      130,
  wxCAP_PROJECTING,
  wxCAP_BUTT,

  wxSMOOTHING_DEFAULT = 140,
  wxSMOOTHING_PARTIAL,
  wxSMOOTHING_ON,
  wxSMOOTHING_OFF,

  wxDIM_OVER = 150,
  wxFADE_OVER,
};


// Logical ops
typedef enum {
  wxCLEAR,      // 0
  wxXOR,        // src XOR dst
  wxINVERT,     // NOT dst
  wxOR_REVERSE, // src OR (NOT dst)
  wxAND_REVERSE,// src AND (NOT dst)
  wxCOPY,       // src
  wxAND,        // src AND dst
  wxAND_INVERT, // (NOT src) AND dst
  wxNO_OP,      // dst
  wxNOR,        // (NOT src) AND (NOT dst)
  wxEQUIV,      // (NOT src) XOR dst
  wxSRC_INVERT, // (NOT src)
  wxOR_INVERT,  // (NOT src) OR dst
  wxNAND,       // (NOT src) OR (NOT dst)
  wxOR,         // src OR dst
  wxSET,        // 1
  wxSRC_OR,     // source _bitmap_ OR destination
  wxSRC_AND,     // source _bitmap_ AND destination
  wxCOLOR
} form_ops_t;

// Flood styles
#define  wxFLOOD_SURFACE   1
#define  wxFLOOD_BORDER    2

// Polygon filling mode
#define  wxODDEVEN_RULE    1
#define  wxWINDING_RULE    2

// Directions
#define wxHORIZONTAL     0x01
#define wxVERTICAL       0x02
#define wxBOTH           (wxVERTICAL|wxHORIZONTAL)
#define wxCENTER_FRAME   0x04  /* centering into frame rather than screen */

#define wxOK                0x0001
#define wxYES_NO            0x0002
#define wxCANCEL            0x0004
#define wxYES               0x0008
#define wxNO                0x0010

#define wxCENTRE            0x0200
#define wxCENTER wxCENTRE

// Use internally-calculated width if -1
#define wxSIZE_AUTO_WIDTH       1
// Use internally-calculated height if -1
#define wxSIZE_AUTO_HEIGHT      2
// Use internally-calculated width and height if each is -1
#define wxSIZE_AUTO             3
// Use internally-calculated x if -1

// -1 as x/y position really means -1; don't use default
#define wxPOS_USE_MINUS_ONE 4

#define wxDEFAULT_POSITION -11111
#define wxNEGPOS_IS_DEFAULT(x) ((x < 0) ? wxDEFAULT_POSITION : x)

// Ignore missing (-1) dimensions (use existing).
// For readability only: test for wxSIZE_AUTO_WIDTH/HEIGHT in code.
#define wxSIZE_USE_EXISTING     0

// Clipboard formats
#define wxCF_TEXT               1
#define wxCF_BITMAP             2
#define wxCF_METAFILE           3
#define wxCF_DIB                4
#define wxCF_OEMTEXT            5

// Virtual keycodes
enum _Virtual_keycodes {
 WXK_BACK    =   8,
 WXK_TAB     =   9,
 WXK_RETURN  =	13,
 WXK_ESCAPE  =	27,
 WXK_SPACE   =	32,
 WXK_DELETE  = 127,

 /* Use Unicode surrogate area for these: */
 WXK_START   = 0xD800,
 WXK_LBUTTON,
 WXK_RBUTTON,
 WXK_CANCEL,
 WXK_MBUTTON,
 WXK_CLEAR,
 WXK_SHIFT,
 WXK_CONTROL,
 WXK_MENU,
 WXK_PAUSE,
 WXK_CAPITAL,
 WXK_PRIOR,
 WXK_NEXT,
 WXK_END,
 WXK_HOME,
 WXK_LEFT,
 WXK_UP,
 WXK_RIGHT,
 WXK_DOWN,
 WXK_SELECT,
 WXK_PRINT,
 WXK_EXECUTE,
 WXK_SNAPSHOT,
 WXK_INSERT,
 WXK_HELP,
 WXK_NUMPAD0,
 WXK_NUMPAD1,
 WXK_NUMPAD2,
 WXK_NUMPAD3,
 WXK_NUMPAD4,
 WXK_NUMPAD5,
 WXK_NUMPAD6,
 WXK_NUMPAD7,
 WXK_NUMPAD8,
 WXK_NUMPAD9,
 WXK_MULTIPLY,
 WXK_ADD,
 WXK_SEPARATOR,
 WXK_SUBTRACT,
 WXK_DECIMAL,
 WXK_DIVIDE,
 WXK_F1,
 WXK_F2,
 WXK_F3,
 WXK_F4,
 WXK_F5,
 WXK_F6,
 WXK_F7,
 WXK_F8,
 WXK_F9,
 WXK_F10,
 WXK_F11,
 WXK_F12,
 WXK_F13,
 WXK_F14,
 WXK_F15,
 WXK_F16,
 WXK_F17,
 WXK_F18,
 WXK_F19,
 WXK_F20,
 WXK_F21,
 WXK_F22,
 WXK_F23,
 WXK_F24,
 WXK_NUMLOCK,
 WXK_SCROLL,
 WXK_PAGEUP,
 WXK_PAGEDOWN,
 // included for windows compatibility:
 WXK_WHEEL_UP,
 WXK_WHEEL_DOWN,
 WXK_PRESS,
 WXK_RELEASE
};

// Colours - see wx_gdi.cc for database

// OS mnemonics -- Identify the running OS (useful for Windows)
// [Not all platforms are currently available or supported]
enum {
  wxCURSES,
  wxXVIEW_X,	// Sun's XView OpenLOOK toolkit
  wxMOTIF_X,	// OSF Motif 1.x.x
  wxCOSE_X,	// OSF Common Desktop Environment
  wxNEXTSTEP,	// NeXTStep
  wxMACINTOSH,	// Apple System 7
  wxGEOS,	// GEOS
  wxOS2_PM,	// OS/2 Workplace
  wxWINDOWS,	// Windows or WfW
  wxPENWINDOWS,	// Windows for Pen Computing
  wxWINDOWS_NT,	// Windows NT
  wxWIN32S,	// Windows 32S API
  wxWIN386	// Watcom 32-bit supervisor modus
};

const int kActiveControl = 0;
const int kInactiveControl = 255;

#define IMPLEMENT_DYNAMIC_CLASS(x, y) /* empty */

extern GDHandle wxGetGDHandle(void);
extern CGrafPtr wxGetGrafPtr(void);

/* Extra "events" to suppliment the old Mac OS constants: */
#define leaveEvt 42
#define wheelEvt 43
#define mouseMenuDown 44
#define unicodeEvt 45

#include "wx_obj.h"

#endif // wxb_commonh
