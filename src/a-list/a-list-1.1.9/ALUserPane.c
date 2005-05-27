/*
 *	ALUserPane.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#include "AListInternal.h"

#if ALIST_USEAPPEARANCEMGR

#if defined( TARGET_API_MAC_CARBON )
	#if !defined( __CONTROLDEFINITIONS__ )
		#include <ControlDefinitions.h>
	#endif
#endif

static pascal void			local_ALUserPaneDrawProc(ControlHandle control, SInt16 part);
static pascal ControlPartCode	local_ALUserPaneHitTestProc(ControlHandle control, Point where);
static pascal ControlPartCode	local_ALUserPaneTrackProc(ControlHandle control, Point startPt, ControlActionUPP actionProc);
static pascal ControlPartCode	local_ALUserPaneKeyDownProc(ControlHandle control, SInt16 keyCode, SInt16 charCode,
									SInt16 modifiers);
static pascal void			local_ALUserPaneActivateProc(ControlHandle control, Boolean activating);
static pascal ControlPartCode	local_ALUserPaneFocusProc(ControlHandle control, ControlFocusPart part);

ALIST_API OSErr ALMakeUserPaneControl(ALHandle hAL, Boolean canFocus, ControlHandle *newCntl)
{	OSErr		err, err2;
	WindowPtr	windowPtr;
	Rect			box;
	ControlHandle	tempCntl;
	SInt16		features;

	static short					numberOfUsers = 0;
	static ControlUserPaneDrawUPP	drawUPP = nil;
	static ControlUserPaneHitTestUPP	testUPP;
	static ControlUserPaneTrackingUPP	trackingUPP;
	static ControlUserPaneKeyDownUPP	downUPP;
	static ControlUserPaneActivateUPP	activateUPP;
	static ControlUserPaneFocusUPP	focusUPP;

	if (hAL != nil && newCntl != nil) {
		// If the Appearance Manager is not installed, can't create a user pane control.
		if (BTST((*hAL)->flags, alFHasAppearanceMgr) == 0)
			return alAppearanceMgrNotInstalled;

		if (numberOfUsers == 0 && drawUPP == nil) {
			// If nobody is using these yet, create  the UserPane Routine Descriptors.
			drawUPP = NewControlUserPaneDrawUPP( local_ALUserPaneDrawProc );
			testUPP = NewControlUserPaneHitTestUPP( local_ALUserPaneHitTestProc );
			trackingUPP = NewControlUserPaneTrackingUPP( local_ALUserPaneTrackProc );
			downUPP = NewControlUserPaneKeyDownUPP( local_ALUserPaneKeyDownProc );
			activateUPP = NewControlUserPaneActivateUPP( local_ALUserPaneActivateProc );
			focusUPP = NewControlUserPaneFocusUPP( local_ALUserPaneFocusProc );
		}

		numberOfUsers++;

		// Get the window and the size of the control.
		ALGetInfo(alWindow, &windowPtr, hAL);
		// Note: this covers the scroll bar also.  Which is why we set the scroll bar supervisor to here later on.
		ALGetViewRect(&box, hAL);

		// The A List can always handle tracking and activation events.
		features = kControlHandlesTracking + kControlWantsActivate + kControlSupportsEmbedding;
		// The A List can optionally handle keyboard focus as well.
		if (canFocus)
			features += kControlSupportsFocus + kControlGetsFocusOnClick;

		// Create the new control.
		tempCntl = NewControl( windowPtr, &box, "\p", true, features, 0, 0, kControlUserPaneProc, (long)hAL );

		// Try to embed the control in the window wherever it fits.
		// Don't worry about the error code though, because we may be in a window without
		// an embedding heirarchy.
		err2 = AutoEmbedControl( tempCntl, windowPtr );

		err = SetControlData(tempCntl, kControlNoPart, kControlUserPaneDrawProcTag,
								sizeof(drawUPP), (Ptr)&drawUPP);
		if (err == noErr)
			err = SetControlData(tempCntl, kControlNoPart, kControlUserPaneHitTestProcTag,
								sizeof(testUPP), (Ptr)&testUPP);
		if (err == noErr)
			err = SetControlData(tempCntl, kControlNoPart, kControlUserPaneTrackingProcTag,
								sizeof(trackingUPP), (Ptr)&trackingUPP);
		if (err == noErr)
			err = SetControlData(tempCntl, kControlNoPart, kControlUserPaneKeyDownProcTag,
								sizeof(downUPP), (Ptr)&downUPP);
		if (err == noErr)
			err = SetControlData(tempCntl, kControlNoPart, kControlUserPaneActivateProcTag,
								sizeof(activateUPP), (Ptr)&activateUPP);
		if (err == noErr)
			err = SetControlData(tempCntl, kControlNoPart, kControlUserPaneFocusProcTag,
								sizeof(focusUPP), (Ptr)&focusUPP);

		// Also need to set the scroll bar's supervisor so all calls are routed through this.
		if (err == noErr && (*hAL)->vScroll != nil) {
			err = SetControlSupervisor((*hAL)->vScroll, tempCntl);
			if ( err == noErr )
				err2 = EmbedControl( (*hAL)->vScroll, tempCntl );
		}
		if (err == noErr && (*hAL)->hScroll != nil) {
			err = SetControlSupervisor((*hAL)->hScroll, tempCntl);
			if ( err == noErr )
				err2 = EmbedControl( (*hAL)->hScroll, tempCntl );
		}

		*newCntl = tempCntl;
	} else {
		// If either the list or control pointer are nil, somebody isn't using it anymore.
		if ( numberOfUsers != 0 )
			numberOfUsers--;

		if (numberOfUsers == 0 && drawUPP != nil) {
			// If nobody is using it, dispose of the routine descriptors.
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
			DisposeRoutineDescriptor(drawUPP);
			drawUPP = nil;
			DisposeRoutineDescriptor(testUPP);
			DisposeRoutineDescriptor(trackingUPP);
			DisposeRoutineDescriptor(downUPP);
			DisposeRoutineDescriptor(activateUPP);
			DisposeRoutineDescriptor(focusUPP);
#else
			DisposeControlUserPaneDrawUPP(drawUPP);
			drawUPP = nil;
			DisposeControlUserPaneHitTestUPP(testUPP);
			DisposeControlUserPaneTrackingUPP(trackingUPP);
			DisposeControlUserPaneKeyDownUPP(downUPP);
			DisposeControlUserPaneActivateUPP(activateUPP);
			DisposeControlUserPaneFocusUPP(focusUPP);
#endif
		}
		err = noErr;
	}

	return err;
}

#pragma mark -

static pascal void local_ALUserPaneDrawProc(ControlHandle control, SInt16 part)
{
#pragma unused( part )
	ALUpdate(nil, (ALHandle)GetControlReference(control));
}

static pascal ControlPartCode local_ALUserPaneHitTestProc(ControlHandle control, Point where)
{	Rect		box;

	// Initially, get the view rectangle which includes any scroll bars.
	ALGetViewRect(&box, (ALHandle)GetControlReference(control));

	if (PtInRect(where, &box))
		return kControlListBoxPart;
	else
		return kControlNoPart;
}

static pascal ControlPartCode local_ALUserPaneTrackProc(ControlHandle control, Point startPt, ControlActionUPP actionProc)
{	Boolean			doubleClick;
	ControlPartCode	result;
	ALHandle			hAL;
	ControlHandle		hScroll, vScroll;
	EventRecord		tempEvent;

	result = kControlNoPart;

	if ( actionProc == (ControlActionUPP)-1L || actionProc == nil ) {
		hAL = (ALHandle)GetControlReference(control);
		vScroll = (*hAL)->vScroll;
		hScroll = (*hAL)->hScroll;

		// Reset the supervisors so the scroll bars can handle themselves.
		if ( vScroll != nil )
			/*err = */SetControlSupervisor( vScroll, nil );
		if ( hScroll != nil )
			/*err = */SetControlSupervisor( hScroll, nil );

#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
		// Call OSEventAvail to get the modifier keys.
		OSEventAvail( everyEvent, &tempEvent );
#else
		tempEvent.modifiers = GetCurrentKeyModifiers( );
#endif

		doubleClick = ALClick( startPt, startPt, tempEvent.modifiers, TickCount( ), hAL );

		// Reset the supervisors so later clicks will be handled through here.
		if ( vScroll != nil )
			/*err = */SetControlSupervisor( vScroll, control );
		if ( hScroll != nil )
			/*err = */SetControlSupervisor( hScroll, control );


		if ( doubleClick )
			result = kControlListBoxDoubleClickPart;
		else
			result = kControlListBoxPart;
	}

	return result;
}

static pascal ControlPartCode local_ALUserPaneKeyDownProc(ControlHandle control, SInt16 keyCode, SInt16 charCode,
			SInt16 modifiers)
{
#pragma unused( keyCode )
	ALKey( charCode, modifiers, TickCount( ), (ALHandle)GetControlReference(control) );

	return kControlListBoxPart;
}

static pascal void local_ALUserPaneActivateProc(ControlHandle control, Boolean activating)
{	ALActivate(activating, (ALHandle)GetControlReference(control));
}

static pascal ControlPartCode local_ALUserPaneFocusProc(ControlHandle control, ControlFocusPart part)
{	return ALSetFocus(part, (ALHandle)GetControlReference(control));
}

#endif
