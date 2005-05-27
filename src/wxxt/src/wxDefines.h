/*								-*- C++ -*-
 *
 * Purpose: Declarations/definitions common to all wx source files
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

#ifndef wxDefines_h
#define wxDefines_h

#ifdef __GNUG__
#pragma interface
#endif

// typedef of Bool when Xt include files are NOT included
#ifndef Bool
typedef int Bool;
#endif
#undef True
#undef False
#undef TRUE
#undef FALSE
#define True  1
#define False 0
#define TRUE  1
#define FALSE 0
// typedef for wxWindows callback functions
class wxObject;
class wxCommandEvent;
typedef void (*wxFunction)(wxObject*, wxCommandEvent*);

// defines for port
#define wx_x

typedef short int WXTYPE;

// Types of objects
#define wxTYPE_ANY             0
#define wxTYPE_OBJECT          wxTYPE_ANY
#define wxTYPE_WINDOW          1
#define wxTYPE_DIALOG_BOX      2
#define wxTYPE_ITEM            3
#define wxTYPE_PANEL           4
#define wxTYPE_CANVAS          5
#define wxTYPE_TEXT_WINDOW     6
#define wxTYPE_FRAME           7

#define wxTYPE_BUTTON          8
#define wxTYPE_POPUP_EVENT     9
#define wxTYPE_MESSAGE         10
#define wxTYPE_CHOICE          11
#define wxTYPE_LIST_BOX        12
#define wxTYPE_SLIDER          13
#define wxTYPE_CHECK_BOX       14
#define wxTYPE_MENU            15
#define wxTYPE_MENU_BAR        16
#define wxTYPE_MULTI_TEXT      17
#define wxTYPE_RADIO_BOX       18
#define wxTYPE_GROUP_BOX       19
#define wxTYPE_GAUGE           20
#define wxTYPE_SCROLL_BAR      21
#define wxTYPE_VIRT_LIST_BOX   22

#define wxTYPE_EVENT           25
#define wxTYPE_DC              26
#define wxTYPE_DC_WINDOW       27
#define wxTYPE_DC_POSTSCRIPT   28
#define wxTYPE_DC_PRINTER      29
#define wxTYPE_DC_METAFILE     30
#define wxTYPE_DC_MEMORY       31
#define wxTYPE_MOUSE_EVENT     32
#define wxTYPE_KEY_EVENT       33
#define wxTYPE_COMMAND_EVENT   34
#define wxTYPE_SCROLL_EVENT    36
#define wxTYPE_DC_OBJECT       37

#define wxTYPE_DC_CANVAS       wxTYPE_DC_WINDOW
#define wxTYPE_DC_PANEL        wxTYPE_DC_WINDOW

#define wxTYPE_PEN             40
#define wxTYPE_BRUSH           41
#define wxTYPE_FONT            42
#define wxTYPE_ICON            42
#define wxTYPE_BITMAP          43
#define wxTYPE_METAFILE        44
#define wxTYPE_TIMER           45
#define wxTYPE_COLOUR          46
#define wxTYPE_COLOURMAP       47
#define wxTYPE_CURSOR          48

#define wxTYPE_DDE_CLIENT      60
#define wxTYPE_DDE_SERVER      61
#define wxTYPE_DDE_CONNECTION  62

#define wxTYPE_HELP_INSTANCE   63

#define wxTYPE_LIST            70
#define wxTYPE_STRING_LIST     71
#define wxTYPE_HASH_TABLE      72
#define wxTYPE_NODE            73
#define wxTYPE_APP             74

#define wxTYPE_ENHANCED_DIALOG 80
#define wxTYPE_TOOLBAR         81
#define wxTYPE_BUTTONBAR       82

#define wxTYPE_DATABASE        90
#define wxTYPE_QUERY_FIELD     91
#define wxTYPE_QUERY_COL       92
#define wxTYPE_RECORDSET       93

#define wxTYPE_CONSTRAINTS     94
#define wxTYPE_TYPEDEF         95

#define wxTYPE_USER            500

// defines for wxBitmap
#define wxBITMAP_DISCARD_COLOURMAP	0x00001
#define wxBITMAP_TYPE_BMP		0x00002
#define wxBITMAP_TYPE_BMP_RESOURCE	0x00004
#define wxBITMAP_TYPE_ICO		0x00008
#define wxBITMAP_TYPE_ICO_RESOURCE	0x00010
#define wxBITMAP_TYPE_CUR		0x00020
#define wxBITMAP_TYPE_CUR_RESOURCE	0x00040
#define wxBITMAP_TYPE_XBM		0x00080
#define wxBITMAP_TYPE_XBM_DATA		0x00100
#define wxBITMAP_TYPE_XPM		0x00200
#define wxBITMAP_TYPE_XPM_DATA		0x00400
#define wxBITMAP_TYPE_TIF		0x00800
#define wxBITMAP_TYPE_GIF		0x01000
#define wxBITMAP_TYPE_ANY		0x02000
#define wxBITMAP_TYPE_JPEG		0x04000
#define wxBITMAP_TYPE_PNG		0x08000
#define wxBITMAP_TYPE_RESOURCE		wxBITMAP_TYPE_BMP_RESOURCE
#define wxBITMAP_TYPE_MASK		0x10000
#define wxBITMAP_DEFAULT		(wxBITMAP_DISCARD_COLOURMAP | wxBITMAP_TYPE_XBM)

// defines for wxBrush / wxPen
// line/brush styles
#define wxSOLID            0
#define wxTRANSPARENT      1
// user defined dash
#define wxUSER_DASH        100
// dashs
#define wxDOT              101
#define wxLONG_DASH        102
#define wxSHORT_DASH       103
#define wxDOT_DASH         104
#define wxXOR_DOT          105
#define wxXOR_LONG_DASH    106
#define wxXOR_SHORT_DASH   107
#define wxXOR_DOT_DASH     108
#define wxFIRST_DASH       wxDOT
#define wxIS_DASH(d)	   (wxDOT <= d && d <= wxDOT_DASH)
#define wxNUM_DASH         (wxDOT_DASH - wxDOT + 1)
// user defined stipple
#define wxSTIPPLE          200
// hatches
#define wxBDIAGONAL_HATCH  201
#define wxCROSSDIAG_HATCH  202
#define wxFDIAGONAL_HATCH  203
#define wxCROSS_HATCH      204
#define wxHORIZONTAL_HATCH 205
#define wxVERTICAL_HATCH   206
#define wxFIRST_HATCH      wxBDIAGONAL_HATCH
#define wxIS_HATCH(h)	   (wxBDIAGONAL_HATCH <= h && h <= wxVERTICAL_HATCH)
#define wxNUM_HATCH        (wxVERTICAL - wxBDIAGONAL + 1)
#define wxOPAQUE_STIPPLE   207
#define wxPANEL_PATTERN    208

#define wxJOIN_BEVEL       0
#define wxJOIN_MITER       1
#define wxJOIN_ROUND       2

#define wxCAP_ROUND        0
#define wxCAP_PROJECTING   1
#define wxCAP_BUTT         2
#define wxCAP_NOTLAST      3

// defines for wxClipboard
# define wxCF_TEXT               1
# define wxCF_BITMAP             2
# define wxCF_METAFILE           3
# define wxCF_DIB                4
# define wxCF_OEMTEXT            5

// defines for wxCursor
enum {
    // cursors with X11 equvalents
    wxFIRST_X11_CURSOR          = 0,
    wxCURSOR_ARROW		= wxFIRST_X11_CURSOR,
    wxCURSOR_BASED_ARROW_DOWN,
    wxCURSOR_BASED_ARROW_UP,
    wxCURSOR_BULLSEYE,
    wxCURSOR_CROSS,
    wxCURSOR_CROSS_REVERSE,
    wxCURSOR_DOUBLE_ARROW,
    wxCURSOR_HAND,
    wxCURSOR_IBEAM,
    wxCURSOR_LEFT_BUTTON,
    wxCURSOR_MAGNIFIER,
    wxCURSOR_MIDDLE_BUTTON,
    wxCURSOR_NO_ENTRY,
    wxCURSOR_PAINT_BRUSH,
    wxCURSOR_PENCIL,
    wxCURSOR_POINT_LEFT,
    wxCURSOR_POINT_RIGHT,
    wxCURSOR_QUESTION_ARROW,
    wxCURSOR_RIGHT_BUTTON,
    wxCURSOR_SIZENESW,
    wxCURSOR_SIZENS,
    wxCURSOR_SIZENWSE,
    wxCURSOR_SIZEWE,
    wxCURSOR_SIZING,
    wxCURSOR_SPRAYCAN,
    wxCURSOR_WAIT,
    wxCURSOR_WATCH,
    wxLAST_X11_CURSOR		= wxCURSOR_WATCH,
    // privately defined cursors
    wxCURSOR_START_PRIVATE	= wxLAST_X11_CURSOR+1,
    wxCURSOR_CHAR,
    wxCURSOR_BLANK
};

// defines for wxDC
#define wxDEVICE_NONE		0
#define wxDEVICE_WINDOW		1
#define wxDEVICE_EPS		2
#define wxDEVICE_MEMORY		3
#define wxDEVICE_CANVAS		wxDEVICE_WINDOW
#define wxDEVICE_PANEL		wxDEVICE_WINDOW

#define MM_TEXT			0
#define MM_ISOTROPIC		1
#define MM_ANISOTROPIC		2
#define MM_LOMETRIC		3
#define MM_HIMETRIC		4
#define MM_TWIPS		5
#define MM_POINTS		6
#define MM_METRIC		7

#define PS_PORTRAIT		0
#define PS_LANDSCAPE		1

#define PS_PRINTER		0
#define PS_FILE			1
#define PS_PREVIEW		2

#define mm2inches		0.0393700787402
#define inches2mm		25.4
#define mm2twips		56.6929133859
#define twips2mm		0.0176388888889
#define mm2pt			2.83464566929
#define pt2mm			0.352777777778

#define wxODDEVEN_RULE		0
#define wxWINDING_RULE		1
#define wxFLOOD_SURFACE		0
#define wxFLOOD_BORDER		1

#define wxAND			0
#define wxAND_INVERT		1
#define wxAND_REVERSE		2
#define wxCLEAR			3
#define wxCOPY			4
#define wxEQUIV			5
#define wxINVERT		6
#define wxNAND			7
#define wxNOR			8
#define wxNO_OP			9
#define wxOR			10
#define wxOR_INVERT		11
#define wxOR_REVERSE		12
#define wxSET			13
#define wxSRC_INVERT		14
#define wxXOR			15
#define wxCOLOR			16

#define wxCOPY_INVERT		wxSRC_INVERT

// defines for wxEvent
enum {
    // Command event types
    wxEVENT_TYPE_BUTTON_COMMAND         = 0x0000,
    wxEVENT_TYPE_CHECKBOX_COMMAND       = 0x0001,
    wxEVENT_TYPE_RESERVED1              = 0x0002,
    wxEVENT_TYPE_CHOICE_COMMAND         = 0x0003,
    wxEVENT_TYPE_LISTBOX_COMMAND        = 0x0004,
    wxEVENT_TYPE_RESERVED2              = 0x0005,
    wxEVENT_TYPE_TEXT_COMMAND           = 0x0006,
    wxEVENT_TYPE_MENU_POPDOWN_NONE      = 0x0007,
    wxEVENT_TYPE_MENU_COMMAND           = 0x0008,
    wxEVENT_TYPE_SLIDER_COMMAND         = 0x0009,
    wxEVENT_TYPE_RADIOBOX_COMMAND       = 0x000a,
    wxEVENT_TYPE_TEXT_ENTER_COMMAND     = 0x000b,
    wxEVENT_TYPE_SET_FOCUS              = 0x000c,
    wxEVENT_TYPE_KILL_FOCUS             = 0x000d,
    wxEVENT_TYPE_SCROLLBAR_COMMAND      = 0x000e,
    wxEVENT_TYPE_MENU_POPDOWN           = 0x000f,
    wxEVENT_TYPE_LISTBOX_DCLICK_COMMAND = 0x0010,
    wxEVENT_TYPE_MENU_SELECT            = 0x0011,
    wxEVENT_TYPE_TAB_CHOICE_COMMAND     = 0x0012,
    // Scrollbar event identifiers
    wxEVENT_TYPE_SCROLL_TOP           = 0x1000,
    wxEVENT_TYPE_SCROLL_BOTTOM        = 0x1001,
    wxEVENT_TYPE_SCROLL_LINEUP        = 0x1002,
    wxEVENT_TYPE_SCROLL_LINEDOWN      = 0x1003,
    wxEVENT_TYPE_SCROLL_PAGEUP        = 0x1004,
    wxEVENT_TYPE_SCROLL_PAGEDOWN      = 0x1005, 
    wxEVENT_TYPE_SCROLL_THUMBTRACK    = 0x1006,
    // Character input event type
    wxEVENT_TYPE_CHAR                 = 0x2000,
    // Mouse event types
    wxEVENT_TYPE_MOUSE                = 0x3000,
    wxEVENT_TYPE_LEFT                 = 0x3000,
    wxEVENT_TYPE_MIDDLE               = 0x3001,
    wxEVENT_TYPE_RIGHT                = 0x3002,
    wxEVENT_TYPE_DOWN                 = 0x3010,
    wxEVENT_TYPE_DOUBLE               = 0x3100,
    wxEVENT_TYPE_LEFT_DCLICK          = 0x3110, // = LEFT | DOWN | DOUBLE
    wxEVENT_TYPE_LEFT_DOWN            = 0x3010, // = LEFT | DOWN
    wxEVENT_TYPE_LEFT_UP              = 0x3000, // = LEFT
    wxEVENT_TYPE_MIDDLE_DCLICK        = 0x3111, // = MIDDLE | DOWN | DOUBLE
    wxEVENT_TYPE_MIDDLE_DOWN          = 0x3011, // = MIDDLE | DOWN
    wxEVENT_TYPE_MIDDLE_UP            = 0x3001, // = MIDDLE
    wxEVENT_TYPE_RIGHT_DCLICK         = 0x3112, // = RIGHT | DOWN | DOUBLE
    wxEVENT_TYPE_RIGHT_DOWN           = 0x3012, // = RIGHT | DOWN
    wxEVENT_TYPE_RIGHT_UP             = 0x3002, // = RIGHT
    wxEVENT_TYPE_MOTION               = 0x3003,
    wxEVENT_TYPE_ENTER_WINDOW         = 0x3004,
    wxEVENT_TYPE_LEAVE_WINDOW         = 0x3005
};

#define WXSCROLLPOS(event)	event.moveType
#define WXSCROLLORIENT(event)	event.direction

// defines for wxFont
//--- default for all others
#define wxDEFAULT       0
//--- families
#define wxDECORATIVE    1
#define wxMODERN        2
#define wxROMAN         3
#define wxSCRIPT        4
#define wxSWISS         5
#define wxTELETYPE      6
#define wxSYSTEM        13
#define wxSYMBOL        14
//--- styles
#define wxNORMAL        7
#define wxSLANT         8
#define wxITALIC        9
//--- weight
#define wxNORMAL_WEIGHT 10
#define wxBOLD          11
#define wxLIGHT         12
//--- smoothing
#define wxSMOOTHING_DEFAULT   13
#define wxSMOOTHING_PARTIAL   14
#define wxSMOOTHING_OFF       15
#define wxSMOOTHING_ON        16

// defines for wxFrame, wxDialogBox, and wxEnhDialogBox
#define wxBORDER		0x00000001
#define wxNO_CAPTION		0x00000002
#define wxICONIZE		0x00000004
#define wxMDI_CHILD		0x00000008
#define wxMDI_PARENT		0x00000010
#define wxMINIMIZE		wxICONIZE
#define wxMINIMIZE_BOX		0x00000020
#define wxMAXIMIZE		0x00000040
#define wxMAXIMIZE_BOX		0x00000080
#define wxNO_SYSTEM_MENU       	0x00000400
#define wxNO_THICK_FRAME       	0x00000800
#define wxTRANSIENT		0x00001000
#define wxNO_RESIZE_BORDER     	0x00002000
#define wxVSCROLL		0x00020000
#define wxNO_DC			0x00040000
#define wxINVISIBLE             0x00080000
#define wxTRANSPARENT_WIN       0x00100000
#define wxFLOAT_FRAME           0x00200000

#define wxDEFAULT_FRAME		0
#define wxDEFAULT_DIALOG_STYLE	0
#define wxMAX_STATUS		4

// defines for wxItem and descendants
#define wxSINGLE			0x00000000
//--- defined for wxFrame too --------------------
//#define wxBORDER			0x00000001
//------------------------------------------------
//--- defined for centering too (wxWindow) -------
//#define wxHORIZONTAL			0x00000002
//#define wxVERTICAL			0x00000004
//------------------------------------------------
#define wxMULTIPLE			0x00000008
#define wxEXTENDED			0x00000010
#define wxNEEDED_SB			0x00000020
#define wxALWAYS_SB			0x00000040
#define wxHSCROLL			0x00000080
#define wxVERTICAL_LABEL		0x00000100
#define wxHORIZONTAL_LABEL		0x00000200
#define wxBACKINGSTORE			0x00001000
#define wxAT_MOST_ONE			0x00004000
#define wxFLAT				0x00008000
#define wxGL_CONTEXT                    0x02000000
#define wxNO_AUTOCLEAR                  0x04000000
#define wxCOMBO_SIDE                    0x08000000

#define wxALIGN_CENTRE		0
#define wxALIGN_LEFT		1
#define wxALIGN_RIGHT		2
#define wxALIGN_TOP		4
#define wxALIGN_BOTTOM		8
#define wxALIGN_TOP_LEFT	(wxALIGN_TOP    + wxALIGN_LEFT  )
#define wxALIGN_TOP_RIGHT	(wxALIGN_TOP    + wxALIGN_RIGHT )
#define wxALIGN_BOTTOM_LEFT	(wxALIGN_BOTTOM + wxALIGN_LEFT  )
#define wxALIGN_BOTTOM_RIGHT	(wxALIGN_BOTTOM + wxALIGN_RIGHT )

#define wxCANVAS_WIDTH		200
#define wxCANVAS_HEIGHT		200
#define wxGAUGE_WIDTH		150
#define wxGAUGE_HEIGHT		 40
#define wxGROUP_BOX_WIDTH	150
#define wxGROUP_BOX_HEIGHT	100
#define wxMULTI_TEXT_WIDTH	150
#define wxMULTI_TEXT_HEIGHT	100
#define wxTEXT_WIDTH		150
#define wxTEXT_WINDOW_WIDTH	400
#define wxTEXT_WINDOW_HEIGHT	200

// defines for wxKeyEvent
// Virtual keycodes
enum _Virtual_keycodes {
    WXK_BACK    =   8,
    WXK_TAB     =   9,
    WXK_RETURN  =  13,
    WXK_ESCAPE  =  27,
    WXK_SPACE   =  32,
    WXK_DELETE  = 127,
    
    /* Use Unicode surrogate region to avoid char conflicts */
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

    WXK_WHEEL_UP,
    WXK_WHEEL_DOWN,

    WXK_PRESS,
    WXK_RELEASE
};

// defines for wxLayout
#define wxLAYOUT_DEFAULT_MARGIN	0
#define wxLAYOUT_MAX_ITERATIONS	500

// usually used for Window stacking method (X.h)
#undef Above
#undef Below

enum wxEdge {
    wxLeft, wxTop, wxRight, wxBottom, wxWidth, wxHeight,
    wxCentreX, wxCentreY
};
enum wxRelationship {
    wxUnconstrained = 0,
    wxAsIs, wxPercentOf, wxAbove, wxBelow,
    wxLeftOf, wxRightOf, wxSameAs, wxAbsolute
};

// defines for wxPanel
#define PANEL_HMARGIN		2
#define PANEL_VMARGIN		2
#define PANEL_HSPACING		10
#define PANEL_VSPACING		8  

#define wxABSOLUTE_POSITIONING	0x000000 /* obsolete */

// defines for wxPen (see under wxBrush)

// defines for wxUtils
#define wxDIR			0x0001

#define wxOK			0x0001
#define wxYES_NO		0x0002
#define wxCANCEL		0x0004
#define wxYES			0x0008
#define wxNO			0x0010
#define wxICON_EXCLAMATION	0x0020
#define wxICON_HAND		0x0040
#define wxICON_QUESTION		0x0080
#define wxICON_INFORMATION	0x0100
#define wxCENTRE		0x0200
#define wxICON_STOP		wxICON_HAND
#define wxICON_ASTERISK		wxICON_INFORMATION
#define wxICON_MASK		(0x0020|0x0040|0x0080|0x0100)

#define wxOPEN			0x0001
#define wxSAVE			0x0002
#define wxOVERWRITE_PROMPT	0x0004
#define wxHIDE_READONLY		0x0008
#define wxSHOW_HIDDEN		0x0010
#define wxMULTIOPEN             0x0020

#define wxCHOICE_HEIGHT		150
#define wxCHOICE_WIDTH		200
#define wxFSB_WIDTH		500
#define wxFSB_HEIGHT		350

// defines for wxWindow
#define wxHORIZONTAL		0x0002
#define wxVERTICAL		0x0004
#define wxBOTH			(wxHORIZONTAL | wxVERTICAL)
#define wxCENTRE_FRAME		0x0008
#define wxCENTRE_TOPLEFT	0x0010

#define wxSIZE_USE_EXISTING	0x0000
#define wxSIZE_AUTO_WIDTH	0x0001
#define wxSIZE_AUTO_HEIGHT	0x0002
#define wxSIZE_AUTO		wxSIZE_AUTO_WIDTH | wxSIZE_AUTO_HEIGHT
#define wxPOS_USE_MINUS_ONE	0x0004

#define wxDEFAULT_POSITION -11111

// defines to translate british/american english
#define Center			Centre
#define wxALIGN_CENTER		wxALIGN_CENTRE
#define wxCENTER		wxCENTRE
#define wxCENTER_FRAME		wxCENTRE_FRAME
#define wxCenter		wxCentre
#define wxCenterX		wxCentreX
#define wxCenterY		wxCentreY
#define wxColor			wxColour
#define wxColorMap		wxColourMap
#define wxMainColormap		wxMainColourmap
 
// Macro to cut down on compiler warnings.
#if REMOVE_UNUSED_ARG
# define WXUNUSED(identifier) /* identifier */
#else  // stupid, broken compiler
# define WXUNUSED(identifier) identifier
#endif

#endif // wxDefines_h
