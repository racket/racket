/*
 *	QDDrawingState.h
 *
 *	Part of The A List
 *  Three routines to help in saving and restoring all parts of the QuickDraw drawing state EXCEPT THE PORT.
 *  You could save the state from one port, switch ports, then set the drawing state and all foreground/background
 * and optionally text state would be set so the new port draws identically to the old port.
 *
 *  Copyright (c) 2000 Kyle Hammond
 *	All Rights Reserved
 *
 */

#if !defined( __QDDrawingState_h_ )
#define __QDDrawingState_h_		0x0100

#if !defined( __ALISTOPTIMIZATIONS_H_ )
# include "AListOptimizations.h"
#endif

#ifdef WX_CARBON
# ifdef OS_X
#  include <Carbon/Carbon.h>
# else
#  include <Carbon.h>
# endif
#endif

typedef struct {
#if ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM
	ThemeDrawingState	themeState;		/* This is only needed if we're running under CFM. */
#endif
	PenState			penState;
	RGBColor			backColor, foreColor;
	TextStyle			textStyle;
	short			textMode;
	Boolean			haveTextState;
#if ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM
	Boolean			haveThemeState;	/* This is only needed if we're running under CFM. */
#endif
} QDDrawingState;

#ifdef __cplusplus
extern "C" {
#endif

/* This saves the drawing state into the ioDrawingState parameter.
	You should allocate space for the QDDrawingState record and pass a pointer to it in ioDrawingState. */
void SaveQDDrawingState( QDDrawingState *ioDrawingState, const Boolean inSaveTextState );

/* This sets the drawing state to the inDrawingState.  You MUST call SaveQDDrawingState before calling this function.
	The inDrawingState information can optionally be disposed of so you don't need to call DisposeQDDrawingState. */
void RestoreQDDrawingState( QDDrawingState *inDrawingState, const Boolean inDisposeNow );

/* This disposes of the drawing state information. */
void DisposeQDDrawingState( QDDrawingState *inDrawingState );

#ifdef __cplusplus
}
#endif

#endif
