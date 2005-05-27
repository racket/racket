/*
 *	LongControls.h
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#ifndef __CONTROLS__
	#include <Controls.h>
#endif

/* creation and destruction */
OSErr LCAttach (ControlHandle control);
void LCDetach (ControlHandle control);

/* setting variables */
void LCSetValue (ControlHandle control, long value);
void LCSetMin (ControlHandle control, long min);
void LCSetMax (ControlHandle control, long max);
void LCSetRefCon (ControlHandle control, long refCon);

#define LCSetMinimum(x, y)	LCSetMin(x, y)
#define LCSetMaximum(x, y)	LCSetMax(x, y)

/* getting variables */
long LCGetValue (ControlHandle control);
long LCGetMin (ControlHandle control);
long LCGetMax (ControlHandle control);
long LCGetRefCon (ControlHandle control);

#define LCGetMinimum(x)		LCGetMin(x)
#define LCGetMaximum(x)	LCGetMax(x)

/* synchronizing long settings with control (short) settings */
void LCSynch (ControlHandle control);
