/*
 * File:	common.h
 * Purpose:	Declarations/definitions common to all wx source files
 * Author:	Julian Smart
 * Created:	1993
 * Updated:	
 * Copyright:	(c) 2004-2005 PLT Scheme, Inc.
 * Copyright:	(c) 1993, AIAI, University of Edinburgh
 *
 * Renovated by Matthew for MrEd, 1995-2000
 */

#ifndef wxb_commonh
#define wxb_commonh

#define wx_msw

#include <stddef.h>
#include <string.h>
#include "wx_setup.h"
#include "wx_ver.h"

#include <windows.h>
#ifndef Bool
  typedef int Bool;
# define Bool_DEFINED
#endif

#ifndef TRUE
# define TRUE  1
# define FALSE 0
#endif

// wxWindows checks for WIN32, not __WIN32__
#if (defined(__WIN32__) && !defined(WIN32))
# define WIN32
#endif

#ifdef IS_MRMAIN
# define MRED_EXTERN extern __declspec(dllimport)
#else
# define MRED_EXTERN extern __declspec(dllexport)
# define MRED_EXPORT __declspec(dllexport)
#endif

typedef short int WXTYPE;

// Macro to cut down on compiler warnings.
#if REMOVE_UNUSED_ARG
# define WXUNUSED(identifier) /* identifier */
#else  // stupid, broken compiler
# define WXUNUSED(identifier) identifier
#endif

/*
 * Panel item styles
 */
 
// Styles for wxListBox - Yes, all in Multiple , and nothing in Style
#define wxMULTIPLE_MASK     0x00000003
#define wxSINGLE            0x00000000
#define wxMULTIPLE          0x00000001
#define wxEXTENDED          0x00000002

// In style flag.
#define wxSB_MASK           0x00000008
#define wxNEEDED_SB         0x00000000
#define wxALWAYS_SB         0x00000008

// wxText style flags
#define wxPROCESS_ENTER     0x00001000

#define wxPASSWORD          0x00000008

/*
 * Frame/dialog/subwindow/panel item style flags
 */
#define wxVSCROLL           0x00000001
#define wxHSCROLL           0x00000002
#define wxCAPTION           0x00000004

#define wxINVISIBLE         0x00010000

#define wxABSOLUTE_POSITIONING 8
                           // Hint to Windowing system not to try anything clever: ***OBSOLETE***
/*
 * Frame/dialog style flags
 */
 
#define wxSTAY_ON_TOP        0x00000008
#define wxICONIZE            0x00000010
#define wxMINIMIZE           wxICONIZE
#define wxMAXIMIZE           0x00000020
#define wxSDI                0x00000040
#define wxMDI_PARENT         0x00000080
#define wxMDI_CHILD          0x00000100
#define wxNO_THICK_FRAME     0x00000200
#define wxNO_SYSTEM_MENU     0x00000400
#define wxRESIZE_BORDER      0x00001000
#define wxTINY_CAPTION_HORIZ 0x00002000
#define wxTINY_CAPTION_VERT  0x00004000
#define wxRESIZE_BOX         wxMAXIMIZE_BOX
#define wxNO_RESIZE_BORDER   0x00800000
#define wxNO_CAPTION         0x00000004
#define wxFLOAT_FRAME        0x01000000

/*
 * Subwindow style flags
 */
 
#define wxBORDER             0x00000040
#define wxCONTROL_BORDER     0x00020000
#define wxGL_CONTEXT         0x00000800
#define wxNO_AUTOCLEAR       0x00100000
#define wxVERTICAL_LABEL     0x00200000
#define wxHORIZONTAL_LABEL   0x00400000
#define wxTRANSPARENT_WIN    0x00800000
#define wxCOMBO_SIDE         0x04000000

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
  wxOPAQUE_STIPPLE,
#define IS_HATCH(s)	((s)>=wxBDIAGONAL_HATCH && (s)<=wxVERTICAL_HATCH)
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

// Dialog specifiers/return values
#define wxOK                0x0001
#define wxYES_NO            0x0002
#define wxCANCEL            0x0004
#define wxYES               0x0008
#define wxNO                0x0010

#define wxCENTRE            0x0200
#define wxCENTER wxCENTRE

// Possible SetSize flags

// Use internally-calculated width if -1
#define wxSIZE_AUTO_WIDTH       1
// Use internally-calculated height if -1
#define wxSIZE_AUTO_HEIGHT      2
// Use internally-calculated width and height if each is -1
#define wxSIZE_AUTO             3
// Ignore missing (-1) dimensions (use existing).
// For readability only: test for wxSIZE_AUTO_WIDTH/HEIGHT in code.
#define wxSIZE_USE_EXISTING     0

#define wxPOS_USE_MINUS_ONE 4

#define wxDEFAULT_POSITION -11111
#define wxNEG_POS_IS_DEFAULT(x) ((x < 0) ? wxDEFAULT_POSITION : x)

// Clipboard formats
#define wxCF_TEXT               CF_TEXT
#define wxCF_BITMAP             CF_BITMAP
#define wxCF_METAFILE           CF_METAFILEPICT
#define wxCF_DIB                CF_DIB
#define wxCF_OEMTEXT            CF_OEMTEXT

// Virtual keycodes
enum _Virtual_keycodes {
 WXK_BACK    =   8,
 WXK_TAB     =   9,
 WXK_RETURN  =	13,
 WXK_ESCAPE  =	27,
 WXK_SPACE   =	32,
 WXK_DELETE  = 127,

 /* Use Unicode surrogate area to avoid conflicts with real chars */
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
 WXK_SCROLL ,

 WXK_WHEEL_UP,
 WXK_WHEEL_DOWN,

 WXK_PRESS,
 WXK_RELEASE
};

#endif // wxb_commonh
