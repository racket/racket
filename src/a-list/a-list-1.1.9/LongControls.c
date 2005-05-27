/*
 *	LongControls.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#ifndef __FIXMATH__
	#include <FixMath.h>
#endif
#ifndef __TOOLUTILS__
	#include <ToolUtils.h>
#endif

#include "LongControls.h"

/* LongControls private constants and data types */

#define	kMaxShort	0x7FFF			/* maximum signed short integer */
#define	kMinShort		0x8000			/* minimum signed short integer */

/* Long control auxiliary record used for keeping long settings. A handle
to this record is stored in the reference field of the control record. */

typedef struct LCAuxRec {
	long		value;			// long value
	long		min;				// long min
	long		max;				// long max
	SInt32	refCon;			// long refCon field to be used for anything.
} LCAuxRec, *LCAuxPtr, **LCAuxHandle;


OSErr LCAttach (ControlHandle control)
{	Handle		aux;
	LCAuxPtr		pAux;

// Allocate the auxiliary record that will hold long settings.
	aux = NewHandleClear(sizeof(LCAuxRec));
	if (aux == nil)
		return MemError();

// copy current control settings into the auxiliary record
	HLock( aux );
	pAux = (*(LCAuxHandle)aux);
	pAux->value = GetControlValue( control );
	pAux->min = GetControlMinimum( control );
	pAux->max = GetControlMaximum( control );
	pAux->refCon = GetControlReference( control );
	HUnlock( aux );

// store a handle to the auxiliary record in the reference field
	SetControlReference(control, (long)aux);

	return noErr;
}	// LCAttach

void LCDetach (ControlHandle control)
{	Handle	aux;

	aux = (Handle)GetControlReference(control);
	if (aux != nil) {
		// Restore the reference constant.
		SetControlReference(control, (*(LCAuxHandle)aux)->refCon);
		DisposeHandle(aux);
	}
}	// LCDetach

#pragma mark -

void LCSetValue(ControlHandle control, long value)
{	LCAuxPtr		pAux;
	long			controlMin, controlMax, newControlValue;

	pAux = *(LCAuxHandle)GetControlReference(control);

// Make sure value is in the range minämax.
	if (value < pAux->min)
		value = pAux->min;
	if (value > pAux->max)
		value = pAux->max;

// Save value in auxiliary record.
	pAux->value = value;

// Calculate new thumb position.
	controlMin = GetControlMinimum(control);
	controlMax = GetControlMaximum(control);
	newControlValue = controlMin + FixRound(FixMul(FixDiv(value - pAux->min, pAux->max - pAux->min), (controlMax - controlMin << 16)));

	SetControlValue(control, newControlValue);

	return;
}	// LCSetValue

void LCSetMin (ControlHandle control, long min)
{	LCAuxPtr	pAux;

	pAux = *(LCAuxHandle)GetControlReference(control);

// Make sure min is less than or equal to max.
	if (min > pAux->max)
		min = pAux->max;

// Save min in auxiliary record.
	pAux->min = min;

// Set contrlMin field to min or kMinShort, whichever is greater.
	if (min < kMinShort)
		min = kMinShort;
	SetControlMinimum(control, min);

// Reset value.
	LCSetValue(control, pAux->value);

}	// LCSetMin


void LCSetMax (ControlHandle control, long max)
{	LCAuxPtr	pAux;

	pAux = *(LCAuxHandle)GetControlReference(control);

// Make sure max is greater than or equal to min.
	if (max < pAux->min)
		max = pAux->min;

// Save max in auxiliary record.
	pAux->max = max;

// Set contrlMax field to max or kMaxShort, whichever is less.
	if (max > kMaxShort)
		max = kMaxShort;
	SetControlMaximum(control, max);

// Reset value.
	LCSetValue(control, pAux->value);

}	// LCSetMax


void LCSetRefCon (ControlHandle control, long refCon)
{	LCAuxPtr	pAux;

	pAux = *(LCAuxHandle)GetControlReference(control);

	pAux->refCon = refCon;
}	// LCSetRefCon

#pragma mark -

long LCGetValue (ControlHandle control)
{
	return (**(LCAuxHandle)GetControlReference(control)).value;
}	// LCGetValue

long LCGetMin (ControlHandle control)
{
	return (**(LCAuxHandle)GetControlReference(control)).min;
}	// LCGetMin

long LCGetMax (ControlHandle control)
{
	return (**(LCAuxHandle)GetControlReference(control)).max;
}	// LCGetMax

long LCGetRefCon (ControlHandle control)
{
	return (**(LCAuxHandle)GetControlReference(control)).refCon;
}	// LCGetRefCon

#pragma mark -

void LCSynch(ControlHandle control)
{	long	controlMin, controlMax, controlValue;
	LCAuxPtr	pAux;

	controlMin = GetControlMinimum(control);
	controlMax = GetControlMaximum(control);
	controlValue = GetControlValue(control);
	pAux = *(LCAuxHandle)GetControlReference(control);

// Calculate new long value.
	pAux->value = pAux->min + FixMul(FixRatio(controlValue - controlMin, controlMax - controlMin), pAux->max - pAux->min);

}	// LCSynch