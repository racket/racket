/*
 *	ALSelectors.c
 *
 *	Part of The A List.
 *	Borrowed most of this code from the WASTE library.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#ifndef OS_X
# include <Carbon.h>
# define __APPEARANCE__
#endif

#include "AListInternal.h"
#ifndef __APPEARANCE__
# include <Appearance.h>
#endif
#if defined( TARGET_API_MAC_CARBON )
# if !defined( __CONTROLDEFINITIONS__ )
#  include <ControlDefinitions.h>
# endif
#endif

static void	local_ALLookupSelector(const ALLookupTable *table, ALSelector selector, ALFieldDescriptor *desc);
static OSErr	local_ALGetField(const ALLookupTable *table, ALSelector selector, long *info, void *structure);
static OSErr	local_ALSetField(const ALLookupTable *table, ALSelector selector, long *info, void *structure);


#define FIELD_OFFSET(FIELD, STRUCT)	(short) &((STRUCT *) 0L)->FIELD
#define FIELD_SIZE(FIELD, STRUCT)		(short) sizeof(((STRUCT *) 0L)->FIELD)
#define FIELD_DESC(FIELD, STRUCT)		{ FIELD_OFFSET(FIELD, STRUCT), FIELD_SIZE(FIELD, STRUCT) }

static ALLookupTable _alMainSelectorTable[] = {
{ alRefCon,  	   		FIELD_DESC(refCon,				ALRec) },
{ alWindow,			FIELD_DESC(winRef,				ALRec) },
{ alClickLoop,			FIELD_DESC(clickLoop,			ALRec) },
{ alClickCellHook,		FIELD_DESC(clickCellHook,		ALRec) },
{ alDrawCellHook,		FIELD_DESC(drawCellHook,		ALRec) },
{ alDrawBackgroundHook,	FIELD_DESC(drawBackgroundHook,	ALRec) },
{ alHiliteCellHook,		FIELD_DESC(hiliteCellHook,		ALRec) },
{ alUserHandle,			FIELD_DESC(userHandle,			ALRec) },
{ alVertScrollControl,	FIELD_DESC(vScroll,				ALRec) },
{ alVertScrollProc,		FIELD_DESC(scrollVertProc,		ALRec) },
{ alHorzScrollControl,	FIELD_DESC(hScroll,				ALRec) },
{ alHorzScrollProc,		FIELD_DESC(scrollHorzProc,		ALRec) },
{ alCurrentDrag,		FIELD_DESC(currentDrag,			ALRec) },
{ alDisposeCellDataHook,	FIELD_DESC(disposeCellDataHook,	ALRec) },
{ alInputFlavorsHook,	FIELD_DESC(inputFlavorsHook,		ALRec) },
{ alOutputFlavorsHook,	FIELD_DESC(outputFlavorsHook,	ALRec) },
{ alSendDataDragHook,	FIELD_DESC(sendDataDragHook,		ALRec) },
{ alStringSearchHook,	FIELD_DESC(stringSearchHook,		ALRec) },
{ 0, 					0, 0 }};

static void local_ALLookupSelector(const ALLookupTable *table, ALSelector selector, ALFieldDescriptor *desc)
{
	for ( ; table->selector != selector ; table++ )
		if ( * (long *) &(table->desc) == 0L )
			break;
	
	*desc = table->desc;
} // local_ALLookupSelector

OSErr local_ALGetField(const ALLookupTable *table, ALSelector selector, long *info, void *structure)
{	ALFieldDescriptor desc;
	
	local_ALLookupSelector(table, selector, &desc);
	
	if (desc.fLength == 0)
		return alUndefinedSelectorErr;
	
	*info = * (long *) ((long) structure + desc.fOffset);
	return noErr;
} // _ALGetField

OSErr local_ALSetField(const ALLookupTable *table, ALSelector selector, long *info, void *structure)
{	ALFieldDescriptor desc;
	
	local_ALLookupSelector(table, selector, &desc);
	
	if (desc.fLength == 0)
		return alUndefinedSelectorErr;
	
	* (long *) ((long) structure + desc.fOffset) = *info;
	return noErr;
} // _ALSetField

#pragma mark -

ALIST_API OSErr ALGetInfo(ALSelector selector, void *info, ALHandle hAL)
{	if (hAL == nil || *hAL == nil || info == nil)
		return paramErr;

	return local_ALGetField(_alMainSelectorTable, selector, (long *)info, (void *)*hAL);
} // ALGetInfo

ALIST_API OSErr ALSetInfo(ALSelector selector, const void *info, ALHandle hAL)
{	OSErr err;

	if (hAL == nil || *hAL == nil)
		return paramErr;

	err = local_ALSetField(_alMainSelectorTable, selector, (long *)info, (void *)*hAL);
	
	// the hook fields can never be nil, so replace any nil field with the default address
	_ALSetStandardHooks( hAL );

	// We don't want to increment the HookUser count, so remove ourselves again here.
	_ALRemoveStandardHookUser( );

	return err;
} // ALSetInfo

ALIST_API short ALFeatureFlag(unsigned long feature, short action, ALHandle hAL)
{	ALPtr	pAL;
	short	status;

	if (hAL == nil || *hAL == nil)
		return alBitClear;

	pAL = *hAL;

	// get current status of the specified feature
	status = BTST(pAL->features, feature) ? alBitSet : alBitClear;

	// if action is alBitToggle, invert flag
	if (action == alBitToggle)
		action = 1 - status;

	// reset flag according to action
	if (action == alBitClear) {
		BCLR(pAL->features, feature);
		if ( feature == alFInhibitRedraw )
			// If we're enabling drawing, recalculate what's visible.
			_ALCalcVisibleCells( hAL );
	} else if ( action == alBitSet )
		BSET(pAL->features, feature);

	// return old status
	return status;
} // ALFeatureFlag

ALIST_API void ALActivate(Boolean isActive, ALHandle hAL)
{	short	status;
	short	hilite;
	ALPtr	pAL;

	// Sanity check!
	if  (hAL == nil || *hAL == nil)
		return;

	// Get current status of the active flag
	status = BTST((*hAL)->flags, alFActive) ? alBitSet : alBitClear;

	if ((isActive && status == alBitSet) || (!isActive && status == alBitClear))
		return; // Do nothing, we're already the way the user wants us to be.
	else {
		// Hide the selection hiliting.
		_ALHiliteSelected(hAL);

		pAL = *hAL;

		if ( isActive ) {
			BSET(pAL->flags, alFComingActive);
			BSET(pAL->flags, alFActive);
			hilite = kControlNoPart;
 		 } else {
			BCLR(pAL->flags, alFActive);
			hilite = kControlInactivePart;
			// Dispose of the offscreen port if we're inactive.
			if (pAL->offscreenPort != nil) {
				DisposeGWorld(pAL->offscreenPort);
				(*hAL)->offscreenPort = nil;
			}
		}

		// Show the selection hiliting.
		_ALHiliteSelected(hAL);

#if ALIST_USEAPPEARANCEMGR
		if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0) {
			if ((*hAL)->vScroll != nil) {
				if (hilite == kControlInactivePart)
					DeactivateControl((*hAL)->vScroll);
				else
					ActivateControl((*hAL)->vScroll);
			}
			if ((*hAL)->hScroll != nil) {
				if (hilite == kControlInactivePart)
					DeactivateControl((*hAL)->hScroll);
				else
					ActivateControl((*hAL)->hScroll);
			}
		} else
#endif
			{
			if ((*hAL)->vScroll != nil)
				HiliteControl((*hAL)->vScroll, hilite);
			if ((*hAL)->hScroll != nil)
				HiliteControl((*hAL)->hScroll, hilite);
		}

		ALUpdate( nil, hAL );
	}
} // ALActivate

ALIST_API int ALIsActive(ALHandle hAL)
{
  return BTST((*hAL)->flags, alFActive) ? 1 : 0;
}

ALIST_API ControlPartCode ALSetFocus(ControlPartCode focusPart, ALHandle hAL)
{	ControlPartCode	result;
	Boolean			oldFocus;
	CGrafPtr			savePort;
	WindowPtr		winRef;

	if (hAL == nil || *hAL == nil)
		return kControlFocusNoPart;

	// Keep track of the old state of things.
	oldFocus = (BTST((*hAL)->flags, alFFocused) != 0);

	switch (focusPart) {
	case kControlFocusNoPart:		// Turn off all focusing.
		BCLR((*hAL)->flags, alFFocused);
		result = kControlFocusNoPart;
		break;
	case kControlFocusNextPart:		// Switch the state from off/on to on/off.
	case kControlFocusPrevPart:
		if (BTST((*hAL)->flags, alFFocused)) {
			// Turn off the focus.
			BCLR((*hAL)->flags, alFFocused);
			result = kControlFocusNoPart;
		} else {
			// Turn on the focus.
			BSET((*hAL)->flags, alFFocused);
			result = kControlListBoxPart;
		}
		break;
	case kControlListBoxPart:			// Turn on focusing.
		BSET((*hAL)->flags, alFFocused);
		result = kControlListBoxPart;
		break;
	default:	// simply return the state of focus
			result = (BTST((*hAL)->flags, alFFocused)) ? kControlListBoxPart : kControlFocusNoPart;
			break;
	}

	// If the focus changed, redraw if the alFDrawFocus feature is turned on.
	if ( BTST((*hAL)->features, alFDrawFocus) && oldFocus != (BTST((*hAL)->flags, alFFocused) != 0)) {
	  GDHandle        saveDevice;
	  
	  GetGWorld(&savePort, &saveDevice);
	  ALGetInfo(alWindow, &winRef, hAL);
	  SetPortWindowPort(winRef);
	  _ALDrawListBorder(hAL);
	  SetGWorld(savePort, saveDevice);
	}

	return result;
} // ALSetFocus

ALIST_API ControlPartCode ALPartFocused(ALHandle hAL)
{	// Sanity check!
	if  (hAL == nil || *hAL == nil)
		return kControlFocusNoPart;

	// Get current status of the focus flag
	if (BTST((*hAL)->flags, alFFocused))
		return kControlListBoxPart;
	else
		return kControlFocusNoPart;
} // ALPartFocused
