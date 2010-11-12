/* $Id: plevent.h,v 1.1 2004/03/01 20:54:51 cozmic Exp $

    Input event (especially keyboard) definitions for use from plplot
    event handlers.

    Key definitions are taken from the X11/keysymdef.h include file, with
    some changes:
        - only the control keys are retained
	- the XK prefix has been changed to PLK
	- control keys with ASCII equivalents use the ASCII code

    By using the ASCII equivalent (if it exists) for all control keys, it
    is easier to handle keyboard input from any device which is ASCII based.
    Devices which use some other kind of key encoding must translate the raw
    keycodes to those used here.
*/

#ifndef __PLEVENT_H__
#define __PLEVENT_H__

/* Key definitions */

/***********************************************************
Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* Miscellaneous control keys, those with ASCII equivalents */

#define PLK_BackSpace		0x08	/* back space, back char */
#define PLK_Tab			0x09
#define PLK_Linefeed		0x0A	/* Linefeed, LF */
#define PLK_Return		0x0D	/* Return, enter */
#define PLK_Escape		0x1B
#define PLK_Delete		0xFF	/* Delete, rubout */

/* Those without ASCII equivalents */

#define PLK_Clear		0xFF0B
#define PLK_Pause		0xFF13	/* Pause, hold */
#define PLK_Scroll_Lock		0xFF14

/* Cursor control & motion */

#define PLK_Home		0xFF50
#define PLK_Left		0xFF51	/* Move left, left arrow */
#define PLK_Up			0xFF52	/* Move up, up arrow */
#define PLK_Right		0xFF53	/* Move right, right arrow */
#define PLK_Down		0xFF54	/* Move down, down arrow */
#define PLK_Prior		0xFF55	/* Prior, previous (Page Up) */
#define PLK_Next		0xFF56	/* Next (Page Down) */
#define PLK_End			0xFF57	/* EOL */
#define PLK_Begin		0xFF58	/* BOL */

/* Misc Functions */

#define PLK_Select		0xFF60	/* Select, mark */
#define PLK_Print		0xFF61
#define PLK_Execute		0xFF62	/* Execute, run, do */
#define PLK_Insert		0xFF63	/* Insert, insert here */
#define PLK_Undo		0xFF65	/* Undo, oops */
#define PLK_Redo		0xFF66	/* redo, again */
#define PLK_Menu		0xFF67
#define PLK_Find		0xFF68	/* Find, search */
#define PLK_Cancel		0xFF69	/* Cancel, stop, abort, exit */
#define PLK_Help		0xFF6A	/* Help, ? */
#define PLK_Break		0xFF6B
#define PLK_Mode_switch		0xFF7E	/* Character set switch */
#define PLK_script_switch	0xFF7E  /* Alias for mode_switch */
#define PLK_Num_Lock		0xFF7F

/* Keypad Functions, keypad numbers cleverly chosen to map to ascii */

#define PLK_KP_Space		0xFF80	/* space */
#define PLK_KP_Tab		0xFF89
#define PLK_KP_Enter		0xFF8D	/* enter */
#define PLK_KP_F1		0xFF91	/* PF1, KP_A, ... */
#define PLK_KP_F2		0xFF92
#define PLK_KP_F3		0xFF93
#define PLK_KP_F4		0xFF94
#define PLK_KP_Equal		0xFFBD	/* equals */
#define PLK_KP_Multiply		0xFFAA
#define PLK_KP_Add		0xFFAB
#define PLK_KP_Separator	0xFFAC	/* separator, often comma */
#define PLK_KP_Subtract		0xFFAD
#define PLK_KP_Decimal		0xFFAE
#define PLK_KP_Divide		0xFFAF

#define PLK_KP_0		0xFFB0
#define PLK_KP_1		0xFFB1
#define PLK_KP_2		0xFFB2
#define PLK_KP_3		0xFFB3
#define PLK_KP_4		0xFFB4
#define PLK_KP_5		0xFFB5
#define PLK_KP_6		0xFFB6
#define PLK_KP_7		0xFFB7
#define PLK_KP_8		0xFFB8
#define PLK_KP_9		0xFFB9

/*
 * Auxilliary Functions; note the duplicate definitions for left and right
 * function keys;  Sun keyboards and a few other manufactures have such
 * function key groups on the left and/or right sides of the keyboard.
 * We've not found a keyboard with more than 35 function keys total.
 */

#define PLK_F1			0xFFBE
#define PLK_F2			0xFFBF
#define PLK_F3			0xFFC0
#define PLK_F4			0xFFC1
#define PLK_F5			0xFFC2
#define PLK_F6			0xFFC3
#define PLK_F7			0xFFC4
#define PLK_F8			0xFFC5
#define PLK_F9			0xFFC6
#define PLK_F10			0xFFC7
#define PLK_F11			0xFFC8
#define PLK_L1			0xFFC8
#define PLK_F12			0xFFC9
#define PLK_L2			0xFFC9
#define PLK_F13			0xFFCA
#define PLK_L3			0xFFCA
#define PLK_F14			0xFFCB
#define PLK_L4			0xFFCB
#define PLK_F15			0xFFCC
#define PLK_L5			0xFFCC
#define PLK_F16			0xFFCD
#define PLK_L6			0xFFCD
#define PLK_F17			0xFFCE
#define PLK_L7			0xFFCE
#define PLK_F18			0xFFCF
#define PLK_L8			0xFFCF
#define PLK_F19			0xFFD0
#define PLK_L9			0xFFD0
#define PLK_F20			0xFFD1
#define PLK_L10			0xFFD1
#define PLK_F21			0xFFD2
#define PLK_R1			0xFFD2
#define PLK_F22			0xFFD3
#define PLK_R2			0xFFD3
#define PLK_F23			0xFFD4
#define PLK_R3			0xFFD4
#define PLK_F24			0xFFD5
#define PLK_R4			0xFFD5
#define PLK_F25			0xFFD6
#define PLK_R5			0xFFD6
#define PLK_F26			0xFFD7
#define PLK_R6			0xFFD7
#define PLK_F27			0xFFD8
#define PLK_R7			0xFFD8
#define PLK_F28			0xFFD9
#define PLK_R8			0xFFD9
#define PLK_F29			0xFFDA
#define PLK_R9			0xFFDA
#define PLK_F30			0xFFDB
#define PLK_R10			0xFFDB
#define PLK_F31			0xFFDC
#define PLK_R11			0xFFDC
#define PLK_F32			0xFFDD
#define PLK_R12			0xFFDD
#define PLK_R13			0xFFDE
#define PLK_F33			0xFFDE
#define PLK_F34			0xFFDF
#define PLK_R14			0xFFDF
#define PLK_F35			0xFFE0
#define PLK_R15			0xFFE0

/* Modifiers */

#define PLK_Shift_L		0xFFE1	/* Left shift */
#define PLK_Shift_R		0xFFE2	/* Right shift */
#define PLK_Control_L		0xFFE3	/* Left control */
#define PLK_Control_R		0xFFE4	/* Right control */
#define PLK_Caps_Lock		0xFFE5	/* Caps lock */
#define PLK_Shift_Lock		0xFFE6	/* Shift lock */

#define PLK_Meta_L		0xFFE7	/* Left meta */
#define PLK_Meta_R		0xFFE8	/* Right meta */
#define PLK_Alt_L		0xFFE9	/* Left alt */
#define PLK_Alt_R		0xFFEA	/* Right alt */
#define PLK_Super_L		0xFFEB	/* Left super */
#define PLK_Super_R		0xFFEC	/* Right super */
#define PLK_Hyper_L		0xFFED	/* Left hyper */
#define PLK_Hyper_R		0xFFEE	/* Right hyper */

#endif	/* __PLEVENT_H__ */
