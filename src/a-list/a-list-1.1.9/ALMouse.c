/*
 *	ALMouse.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#ifndef OS_X
# include <Carbon.h>
# define __APPEARANCE__
#endif
#if ALIST_USEAPPEARANCEMGR
#ifndef __APPEARANCE__
	#include <Appearance.h>
#endif
#endif
#if defined( TARGET_API_MAC_CARBON )
	#if !defined( __CONTROLDEFINITIONS__ )
		#include <ControlDefinitions.h>
	#endif
#endif
#ifndef __FIXMATH__
	#include <FixMath.h>
#endif
#ifndef __TOOLUTILS__
	#include <ToolUtils.h>
#endif

// Include the DragManagerAdditions only if we're using an old version of the Universal Interfaces.
#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0300)
	#include "DragManagerAdditions.h"
#endif

#include "AListInternal.h"
#include "LongControls.h"
#include "ScrollBars.h"
#include "QDDrawingState.h"

#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	// Should be in Apple's headers, but they're not.
	#define	GetControlBounds(ctlH, theRect)	{ *theRect = (**(ctlH)).contrlRect; }
	#define	GetControlHilite(ctlH)		((**(ctlH)).contrlHilite)
	#define	GetNextControl(ctlH)			((**(ctlH)).nextControl)
#endif

// static variables
static ControlHandle		dynamicCntl;
static SInt32			dynamicOriginalValue;
static RgnHandle		_alSaveClip;
static PenState			_alSavePen;


// Local function prototypes.
#if ALIST_HEIRARCHICAL
	static Boolean local_ALTrackDisclosureTriangle( Point mouseLoc, EventModifiers modifiers, const ALCellPtr theCell, ALHandle hAL );
#endif
static SInt32	local_ALCalcValueFromPoint(ControlHandle theControl, Point thePoint);
static void	local_ALEnableDrawing(void);
static void	local_ALDisableDrawing(void);
static void	local_ALCommandClick(ALHandle hAL);
static void	local_ALShiftClick(ALHandle hAL);
static Boolean	local_ALStdClickLoop(ALHandle hAL);
static pascal void	local_ALHorzThumbActionProc(void);
static pascal void	local_ALVertThumbActionProc(void);


Boolean _ALIsOptionDrag(DragReference drag)
{	EventModifiers downModifiers, upModifiers;

	// get drag modifiers
	GetDragModifiers(drag, nil, (SInt16 *) &downModifiers, (SInt16 *) &upModifiers);

	// return true if the option key was held down at the beginning and/or at the end
	return (((downModifiers | upModifiers) & optionKey) != 0);
}

OSErr _ALMakeDragImage(DragReference theDrag, const ALCellPtr imageCell, GWorldPtr *imageGWorld,
						RgnHandle *imageRgn, ALHandle hAL)
{	ALPtr			pAL = *hAL;		//	assume AL record is already locked
	PixMapHandle		pixels = nil;
	Point				offset;
	Rect				localBounds;
	GDHandle			saveDevice;
	CGrafPtr			savePort;
	OSErr			err;
	ALCell			theCell;
	ItemReference		itemRef;
	ALData			cellData;
	RgnHandle			tempRgn, tempRgn2;
	Rect				cellRect;
	QDDrawingState	saveDrawingState;

	// Init return values.
	*imageGWorld = nil;
	
	// Save current graphics world.
	GetGWorld(&savePort, &saveDevice);

	// Calculate delta between global coords and window local coords.
	offset.v = 0;
	offset.h = 0;
	LocalToGlobal(&offset);

	*imageRgn = NewRgn();
	// Need to add all the cells selected!
	// Go down the rows.
	for (theCell.v = pAL->dataBounds.top; theCell.v < pAL->dataBounds.bottom; theCell.v++) {
		// Go across the columns.
		for (theCell.h = pAL->dataBounds.left; theCell.h < pAL->dataBounds.right; theCell.h++) {
			if (_ALCellIsSelected(&theCell, hAL)) {
				err = ALGetCell(&cellData, &theCell, hAL);

				itemRef = _ALGetItemRefFromCell(&theCell);

				// Add the flavors, if any.
				if (pAL->outputFlavorsHook != nil) {
					ALDataDescriptor	flavorDesc = {typeNull, nil};
					Boolean			more, saveLock;
					short			index;

					for (more = true, index = 0; more; index++) {
						err = CallALOutputFlavorsProc(index, &more, true, &flavorDesc, cellData, &theCell, hAL, (*hAL)->outputFlavorsHook);
						if (err == noErr) {
							if (flavorDesc.dataHandle != nil) {
								saveLock = _ALSetHandleLock(flavorDesc.dataHandle, true);
								err = AddDragItemFlavor(theDrag, itemRef, flavorDesc.descriptorType,
												*flavorDesc.dataHandle, GetHandleSize(flavorDesc.dataHandle), 0);
								_ALSetHandleLock(flavorDesc.dataHandle, saveLock);
								_ALForgetHandle(&flavorDesc.dataHandle);
							} else
								err = AddDragItemFlavor(theDrag, itemRef, flavorDesc.descriptorType, nil, 0, 0);
						}

						if (err != noErr)
							goto cleanup;
					}
				}

				if (theCell.v >= pAL->visCells.top && theCell.v < pAL->visCells.bottom && theCell.h >= pAL->visCells.left && theCell.h < pAL->visCells.right) {
					// Get hilite region
					tempRgn2 = ALGetCellHiliteRgn(&theCell, hAL);

					// We need just the outline of this region
					tempRgn = NewRgn();
					CopyRgn(tempRgn2, tempRgn);
					InsetRgn(tempRgn, 1, 1);
					DiffRgn(tempRgn2, tempRgn, tempRgn2);
					DisposeRgn(tempRgn);
					UnionRgn(tempRgn2, *imageRgn, *imageRgn);
					DisposeRgn(tempRgn2);

					// set the bounds of the drag
					GetRegionBounds( *imageRgn, &localBounds );
				} else
					// set the bounds of the drag
					localBounds.top = localBounds.right = localBounds.bottom = localBounds.left = 0;

				if ((err = SetDragItemBounds(theDrag, itemRef, &localBounds)) != noErr)
					goto cleanup;
			}
		}
	}

	// Convert the image region to global coordinates.
	OffsetRgn(*imageRgn, offset.h, offset.v);

	// If translucent drags are available, prepare a drag image.
	if (BTST( pAL->flags, alFHasTranslucentDrags )) {
		// Get the data on the cell to image (translucent drag).
		err = ALGetCell(&cellData, imageCell, hAL);

		// Get the cell rectangle.
		_ALCalcCellRect(&cellRect, imageCell, false, hAL);

		// Put that into global coordinates.
		OffsetRect(&cellRect, offset.h, offset.v);

		//	create an 8-bit deep gworld for drawing the drag image
		if ( ( err = NewGWorld(imageGWorld, 8, &cellRect, nil, saveDevice, noNewDevice + useTempMem) ) == noErr ) {
			Point		imageOffset = {0, 0};

			// Get the Quickdraw environment of the window the list is in.
			// set up the port
			SetPortWindowPort( pAL->winRef );
			SaveQDDrawingState( &saveDrawingState, true );

			// Get the pixmap from the gworld.
			pixels = GetGWorldPixMap(*imageGWorld);

			// Set up the gworld.
			SetGWorld(*imageGWorld, saveDevice);
			if ( LockPixels( pixels ) ) {
				// Synchronize text attributes.
				TextFont( saveDrawingState.textStyle.tsFont );
				TextFace( saveDrawingState.textStyle.tsFace );
				TextSize( saveDrawingState.textStyle.tsSize );
				if (BTST(pAL->flags, alFHasColorQD))
					RGBForeColor( &saveDrawingState.textStyle.tsColor );

				// Draw the selection.  Note: don't use the notepad background.
				EraseRect( &cellRect );
				if (pAL->drawCellHook != nil)
					CallALDrawCellProc( cellData, imageCell, &cellRect, hAL, pAL->drawCellHook ) ;

// Don't draw the hiliting normally!  It looks better with a black background.
//				if ((*hAL)->hiliteCellHook != nil)
//					CallALHiliteCellProc(cellData, imageCell, BTST(pAL->flags, alFActive) ? true : false,
//									BTST(pAL->features, alFOutlineHilite) ? true : false,
//									&cellRect, hAL, pAL->hiliteCellHook);

				// Invert the cell (using black).  This looks MUCH better when the color highlighting is not black.
				InvertRect( &cellRect );

				// End drawing
				UnlockPixels(pixels);
			}

			DisposeQDDrawingState( &saveDrawingState );

			// Set the drag image.
			SetDragImage( theDrag, pixels, nil, imageOffset, kDragRegionAndImage );
		}
	}

	// clear result code
	err = noErr ;

cleanup :
	// Restore original graphics world.
	SetGWorld(savePort, saveDevice);
	
	// Return result code.
	return err;
} // _ALMakeDragImage

OSErr _ALDrag(Point mouseLoc, EventModifiers modifiers, unsigned long clickTime, ALHandle hAL)
{	ALPtr		pAL = *hAL;
	DragReference	theDrag = nil;
	GWorldPtr		dragGWorld = nil;
	GDHandle			saveDevice;
	RgnHandle		dragRgn = nil;
	EventRecord	theEvent;
	CGrafPtr		savePort;
	OSErr		err;
	ALCell		theCell;

	HLock((Handle)hAL);

	// set up the port
	GetGWorld(&savePort, &saveDevice);
	SetPortWindowPort(pAL->winRef);

	// turn the cursor into an arrow
#if defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 )
	{	Cursor	tempCursor;
		SetCursor( GetQDGlobalsArrow( &tempCursor ) );
	}
#else
	SetCursor(&qd.arrow);
#endif

	// fabricate an EventRecord for TrackDrag
	theEvent.what = mouseDown;
	theEvent.message = 0;
	theEvent.when = clickTime;
	theEvent.where = mouseLoc;
	LocalToGlobal(&theEvent.where);
	theEvent.modifiers = modifiers;

	// before seeing the dotted outline, the user must move the mouse a certain
	// distance away from the initial mouse location; this allows a short click in the selection
	// area to set the insertion point instead of starting a drag-and-drop sequence
	err = alNoDragErr;
	if (!WaitMouseMoved(theEvent.where))
		goto cleanup;

	// create a drag object
	if ((err = NewDrag(&theDrag)) != noErr)
		goto cleanup;

	// We need to supply a data send callback possibly.  Put the initiating ALHandle into the refCon field.
	if (pAL->sendDataDragHook != nil)
		if ((err = SetDragSendProc(theDrag, pAL->sendDataDragHook, hAL)) != noErr)
			goto cleanup;

	// Which cell should did the user click in (translucent drag that cell only).
	(void)ALGetCellAndEdge(mouseLoc, &theCell, hAL);

	// Get drag image & region, add all the items and flavors also.
	if ((err = _ALMakeDragImage(theDrag, &theCell, &dragGWorld, &dragRgn, hAL)) != noErr )
		goto cleanup;

	// Stash drag reference in currentDrag so ALTrackDrag and ALReceiveDrag
	// can tell whether a given drag originated from this AL instance
	pAL->currentDrag = theDrag;

	// track the drag
	err = TrackDrag(theDrag, &theEvent, dragRgn);
	pAL->currentDrag = nil;
	if (err != noErr)
		goto cleanup;

	// clear result code
	err = noErr;

cleanup:
	pAL->currentDrag = nil;
	HUnlock((Handle)hAL);

	// dispose of the drag
	if (theDrag != nil)
		DisposeDrag(theDrag);

	// dispose of the drag gworld
	if (dragGWorld != nil)
		DisposeGWorld(dragGWorld);

	// dispose of the drag region
	if (dragRgn != nil)
		DisposeRgn(dragRgn);

	// restore the port
	SetGWorld(savePort, saveDevice);

	// return result code
	return err;
} // _ALDrag

OSErr _ALExtractFlavor(DragReference theDrag, ItemReference theItem, ALData *cellDataPtr, ALHandle hAL)
{	OSErr			err;
	ALDataDescriptor	flavorDesc = {typeNull, nil};
	FlavorFlags		theFlags;
	Boolean			more;
	short			index;
	Size				theSize;

	// put the requested flavor into the cellDataPtr

	if ((*hAL)->inputFlavorsHook == nil)
		return badDragFlavorErr;

	for (more = true, index = 0; more; index++) {
		// Find out what flavor to look for.
		err = CallALInputFlavorsProc(index, &more, &flavorDesc,  nil, nil, hAL, (*hAL)->inputFlavorsHook);
		// Just in case...
		_ALForgetHandle(&flavorDesc.dataHandle);
		if (err)	goto cleanup;

		// Check for that flavor.
		err = GetFlavorFlags(theDrag, theItem, flavorDesc.descriptorType, &theFlags);
		if (err == noErr) {
			// Need to extract the data and put it into the cellData.

			// get size of flavor data
			if ((err = GetFlavorDataSize(theDrag, theItem, flavorDesc.descriptorType, &theSize)) != noErr)
				goto cleanup;

			// Allocate some memory to put it into.
			if ((err = _ALAllocate(theSize, kAllocTemp, &flavorDesc.dataHandle)) != noErr)
				goto cleanup;

			// get flavor data
			_ALSetHandleLock(flavorDesc.dataHandle, true);
			err = GetFlavorData(theDrag, theItem, flavorDesc.descriptorType, *flavorDesc.dataHandle, &theSize, 0);
			_ALSetHandleLock(flavorDesc.dataHandle, false);

			if (err)	goto cleanup;

			// Need to set cellData now.
			err = CallALInputFlavorsProc(index, &more, &flavorDesc, cellDataPtr, nil, hAL, (*hAL)->inputFlavorsHook);
		}
	}

cleanup:
	_ALForgetHandle(&flavorDesc.dataHandle);

	return err;
} // _ALExtractFlavor

static Boolean local_ALStdClickLoop(ALHandle hAL)
{	ALPtr	pAL = *hAL;		// assume AL record is already locked
	Point		mouseLoc;
	long		currentOffset;
	long		maxOffset;
	long		temp;
	long		vDelta = 0;
	long		hDelta = 0;
	
	// do nothing if all auto-scrolling is disabled or if we're inactive
	if (!(BTST(pAL->features, alFHAutoScroll) || BTST(pAL->features, alFVAutoScroll)) || !BTST(pAL->flags, alFActive))
		return true;

	// Get current mouse location, in local coords.
	// We can safely assume the graphics port is set up correctly.
	GetMouse(&mouseLoc);

	if (BTST(pAL->features, alFVAutoScroll) && pAL->vScroll != nil) {
		// HANDLE VERTICAL AUTOSCROLL
		currentOffset = LCGetValue(pAL->vScroll);
		maxOffset = LCGetMaximum(pAL->vScroll);
	
		// is the mouse below the display rect?
		if (mouseLoc.v > pAL->dispRect.bottom) {
			// is there anything hidden below the view rect?
			if (currentOffset < maxOffset) {
				// then scroll down: calculate the scroll delta => number of cells to scroll
				temp = pAL->dispRect.bottom - mouseLoc.v;
				vDelta = temp / pAL->cellSize.v;
				if (temp % pAL->cellSize.v != 0)
					vDelta--;

				// Pin the new vertical offset to the bottom of the list.
				if (vDelta < (currentOffset - maxOffset))
					vDelta = currentOffset - maxOffset;
			
				// Never scroll by more than kAL_MaxScrollDelta cells
				if (vDelta < -kAL_MaxScrollDelta)
					vDelta = -kAL_MaxScrollDelta;
			}
		}

		// is the mouse above the display rect?
		else if (mouseLoc.v < pAL->dispRect.top) {
			// is there anything hidden above the view rect?
			if (currentOffset > 0) {
				// then scroll up: calculate the scroll delta
				temp = pAL->dispRect.top - mouseLoc.v;
				vDelta = temp / pAL->cellSize.v;
				if (temp % pAL->cellSize.v != 0)
					vDelta++;

				// Pin the new vertical offset to the top of the dest rectangle.
				if (vDelta > currentOffset)
					vDelta = currentOffset;

				// Never scroll by more than kAL_MaxScrollDelta cells.
				if (vDelta > kAL_MaxScrollDelta)
					vDelta = kAL_MaxScrollDelta;
			}
		}
	}
	
	if (BTST(pAL->features, alFHAutoScroll) && pAL->hScroll != nil) {
		// HANDLE HORIZONTAL AUTOSCROLL
		currentOffset = LCGetValue(pAL->hScroll);
		maxOffset = LCGetMaximum(pAL->hScroll);

		// is the mouse to the right of the view rect?
		if (mouseLoc.h > pAL->dispRect.right) {
			// is there anything hidden to the right of the view rect?
			if (currentOffset < maxOffset) {
				// then scroll right: calculate the scroll delta
				hDelta = pAL->dispRect.right - mouseLoc.h;

				// pin the new vertical offset to the rightmost edge
				// of the dest rectangle
				if (hDelta < (currentOffset - maxOffset))
					hDelta = currentOffset - maxOffset;
			
				// never scroll by more than kAL_MaxScrollDelta pixels
				if (hDelta < -kAL_MaxScrollDelta)
					hDelta = -kAL_MaxScrollDelta;
			}
		}

		// is the mouse to the left of the view rect?
		else if (mouseLoc.h < pAL->dispRect.left) {
			// is there anything hidden to the left of the view rect?
			if (currentOffset > 0) {
				// then scroll up: calculate the scroll delta
				hDelta = pAL->dispRect.left - mouseLoc.h;

				// pin the new horizontal offset to the leftmost edge
				// of the dest rectangle
				if (hDelta > currentOffset)
					hDelta = currentOffset;

				// never scroll by more than kAL_MaxScrollDelta pixels
				if (hDelta > kAL_MaxScrollDelta)
					hDelta = kAL_MaxScrollDelta;
			}
		}
	}

	if ( vDelta != 0 || hDelta != 0 )
		// do the scroll
		ALScrollCells(hDelta, vDelta, hAL);

	return true;
} // local_ALStdClickLoop

pascal void _ALHorzScrollActionProc(ControlHandle ctlRef, ControlPartCode partCode)
{	long		value, step, newValue, width;
	ALHandle	scrollAL = (ALHandle)LCGetRefCon(ctlRef);

	if (partCode == 0)
		return;

	width = (*scrollAL)->visCells.right - (*scrollAL)->visCells.left - 1;
	value = LCGetValue(ctlRef);

	// Figure out how many pixels to scroll the list.
	switch (partCode) {
		case kControlUpButtonPart :
			step = - 1;
			break;
		case kControlDownButtonPart :
			step = 1;
			break;
		case kControlPageUpPart :
			step = - width;
			break;
		case kControlPageDownPart :
			step = width;
			break;
		case kControlIndicatorPart :
			LCSynch( ctlRef );			// Synchronize the LongControl to the new ControlHandle value.
			step = LCGetValue( ctlRef ) - value;
			break;
		default :
			step = 0;
			break;
	}

	newValue = value + step;

	// Pin the newValue to the max and min.
	if (newValue > LCGetMaximum(ctlRef))
		newValue = LCGetMaximum(ctlRef);
	else if (newValue < LCGetMinimum(ctlRef))
		newValue = LCGetMinimum(ctlRef);

	// Set the control value and redraw, if necessary.
	if ( newValue != value )
		ALScrollCells( value - newValue, 0, scrollAL );

} // _ALHorzScrollActionProc

pascal void _ALVertScrollActionProc(ControlHandle ctlRef, ControlPartCode partCode)
{	long		value, step, newValue, height;
	ALHandle	scrollAL = (ALHandle)LCGetRefCon(ctlRef);

	if (partCode == 0)
		return;

	height = ( (*scrollAL)->dispRect.bottom - (*scrollAL)->dispRect.top ) / (*scrollAL)->cellSize.v;
	value = LCGetValue( ctlRef );

	// Figure out how many pixels to scroll the list.
	switch (partCode) {
		case kControlUpButtonPart :
			step = - 1;
			break;
		case kControlDownButtonPart :
			step = 1;
			break;
		case kControlPageUpPart :
			step = - height;
			break;
		case kControlPageDownPart :
			step = height;
			break;
		case kControlIndicatorPart :
			LCSynch( ctlRef );			// Synchronize the LongControl to the new ControlHandle value.
			step = LCGetValue( ctlRef ) - value;	// Step to the new value.
			break;
		default :
			step = 0;
			break;
	}

	newValue = value + step;

	// Pin the newValue to the max and min.
	if (newValue > LCGetMaximum(ctlRef))
		newValue = LCGetMaximum(ctlRef);
	else if (newValue < LCGetMinimum(ctlRef))
		newValue = LCGetMinimum(ctlRef);

	// Do the scroll if necessary.
	if ( newValue != value )
		ALScrollCells( 0, value - newValue, scrollAL );
} // _ALVertScrollActionProc

static void local_ALEnableDrawing(void)
{	SetClip(_alSaveClip);
	DisposeRgn(_alSaveClip);
	SetPenState(&_alSavePen);
	return;
}

static void local_ALDisableDrawing(void)
{ 	Rect	nullRect = { 0, 0, 0, 0 };

	_alSaveClip = NewRgn();
	GetClip(_alSaveClip);
	ClipRect(&nullRect);
	GetPenState(&_alSavePen);
	return;
}

static SInt32 local_ALCalcValueFromPoint(ControlHandle theControl, Point thePoint)
{	SInt32	theValue,
			theRange,
			theDistance;
	Rect		box;

	theRange = LCGetMaximum(theControl) - LCGetMinimum(theControl);

	GetControlBounds(theControl, &box);

	if ((box.right - box.left) > (box.bottom - box.top)) {
		// Assume a horizontal scroll bar.

		// Scroll distance adjusted for scroll arrows and the thumb
		theDistance = box.right - box.left - kTotalWidthAdjust;

		// Pin thePoint to the middle of the thumb
		thePoint.h -= box.left + (kTotalWidthAdjust / 2);

		// Calculate new value.
		theValue = LCGetMinimum(theControl) + FixMul(FixRatio(thePoint.h, theDistance), theRange);
	} else {
		// Assume a vertical scroll bar.

		// Scroll distance adjusted for scroll arrows and the thumb
		theDistance = box.bottom - box.top - kTotalWidthAdjust;

		// Pin thePoint to the middle of the thumb
		thePoint.v -= box.top + (kTotalWidthAdjust / 2);

		// Calculate new value.
		theValue = LCGetMinimum(theControl) + FixMul(FixRatio(thePoint.v, theDistance), theRange);
	}

	if (theValue < LCGetMinimum(theControl))
		theValue = LCGetMinimum(theControl);
	else if (theValue > LCGetMaximum(theControl))
		theValue = LCGetMaximum(theControl);

	return theValue;
}

static pascal void local_ALHorzThumbActionProc(void)
{ 	Rect				box;
	Point				thePoint;
	SInt32			theValue, oldValue;
	ALHandle			scrollAL = (ALHandle)LCGetRefCon(dynamicCntl);

	GetControlBounds(dynamicCntl, &box);
	InsetRect(&box, -kThumbTrackLengthSlop, -kThumbTrackWidthSlop);
 
	// Assumes the port is correctly set up
	GetMouse(&thePoint);
	// If we're in the slop rectangle, calculate the new value, otherwise bounce back to the original.
	if ( PtInRect(thePoint, &box) )
		theValue = local_ALCalcValueFromPoint(dynamicCntl, thePoint);
	else
		theValue = dynamicOriginalValue;

	oldValue = LCGetValue(dynamicCntl);
	if (theValue != oldValue) {
		local_ALEnableDrawing();

		ALScrollCells(oldValue - theValue, 0, scrollAL);

		local_ALDisableDrawing();
	}

	return;
} // _ALHorzThumbActionProc

static pascal void local_ALVertThumbActionProc(void)
{ 	Rect				box;
	Point				thePoint;
	SInt32			theValue, oldValue;
	ALHandle			scrollAL = (ALHandle)LCGetRefCon(dynamicCntl);

	GetControlBounds(dynamicCntl, &box);
	InsetRect(&box, -kThumbTrackWidthSlop, -kThumbTrackLengthSlop);
 
	// Assumes the port is correctly set up
	GetMouse(&thePoint);
	// If we're in the slop rectangle, calculate the new value, otherwise bounce back to the original.
	if ( PtInRect(thePoint, &box) )
		theValue = local_ALCalcValueFromPoint(dynamicCntl, thePoint);
	else
		theValue = dynamicOriginalValue;

	oldValue = LCGetValue(dynamicCntl);
	if (theValue != oldValue) {
		local_ALEnableDrawing();

		ALScrollCells(0, oldValue - theValue, scrollAL);

		local_ALDisableDrawing();
	}

	return;
} // _ALVertThumbActionProc

static void local_ALCommandClick(ALHandle hAL)
{	ALPtr	pAL = *hAL;
	ALCell	theCell = pAL->lastClickCell, oldCell;
	Point		mouseLoc = pAL->clickLoc;
	LongRect	selectRect;
	short	edge;
	Boolean	setIt;

	// Toggle the setting on and off.
	setIt = !_ALCellIsSelected(&theCell, hAL);

	// Now switch the setting on this cell.
	selectRect.top = theCell.v;
	selectRect.left = theCell.h;
	selectRect.bottom = theCell.v;
	selectRect.right = theCell.h;
	_ALSelectRect(setIt, true, &selectRect, hAL);

	// Set the alFMouseTracking bit while we track the mouse.
	BSET(pAL->flags, alFMouseTracking);

	// MOUSE TRACKING LOOP
	do {
		// Keep track of which cell it was in.
		oldCell = theCell;

		// get cell offset corresponding to mouse position
		edge = ALGetCellAndEdge(mouseLoc, &theCell, hAL);

		if (edge != kCaretNotInCell && (theCell.v != oldCell.v || theCell.h != oldCell.h)) {
			// Now set the setting on this cell.
			selectRect.top = theCell.v;
			selectRect.left = theCell.h;
			selectRect.bottom = theCell.v;
			selectRect.right = theCell.h;
			_ALSelectRect(setIt, true, &selectRect, hAL);
		}

		// Do the standard scrolling behavior.
		if (!local_ALStdClickLoop(hAL))
			break;

		// call the click loop callback, if any
		if (pAL->clickLoop != nil)
			if (!CallALClickLoopProc(hAL, pAL->clickLoop))
				break;

		// update mouse position
		GetMouse(&mouseLoc);

	} while(WaitMouseUp());

	// clear the alFMouseTracking bit
	BCLR(pAL->flags, alFMouseTracking);
}

static void local_ALShiftClick(ALHandle hAL)
{	ALPtr	pAL = *hAL;
	LongRect	selectRect, resetRect;
	ALCell	anchor, theCell = pAL->lastClickCell, oldCell;
	Boolean	setIt;
	Point		mouseLoc = pAL->clickLoc;
	short	edge;

	// First calculate the anchor.
	if (BTST(pAL->features, alFRowsOnly)) {
		anchor.h = pAL->dataBounds.left;
		setIt = false;
		for (anchor.v = pAL->dataBounds.top; anchor.v < pAL->dataBounds.bottom; anchor.v++) {
			// If we find a selected cell, great.
			if (_ALCellIsSelected(&anchor, hAL))
				setIt = true;
			// If either found one or we hit theCell clicked, break.
			if (setIt || anchor.v == theCell.v)
				break;
		} // end for anchor.v

		if (!setIt && anchor.v != pAL->dataBounds.bottom) {
			// Haven't found the anchor yet.  Look from the bottom.
			for (anchor.v = pAL->dataBounds.bottom - 1; anchor.v > pAL->dataBounds.top; anchor.v--) {
				// If we find a selected cell, great.
				if (_ALCellIsSelected(&anchor, hAL))
					setIt = true;
				// If either found one or we hit theCell clicked, break.
				if (setIt || anchor.v == theCell.v)
					break;
			} // end for anchor.v
		} // end if
	} else if (BTST(pAL->features, alFColumnsOnly)) {
		anchor.v = pAL->dataBounds.top;
		setIt = false;
		for (anchor.h = pAL->dataBounds.left; anchor.h < pAL->dataBounds.right; anchor.h++) {
			// If we find a selected cell, that's the anchor.
			if (_ALCellIsSelected(&anchor, hAL))
				setIt = true;
			// If either found one or we hit theCell clicked, break.
			if (setIt || anchor.h == theCell.h)
				break;
		} // end for anchor.h

		if (!setIt && anchor.h != pAL->dataBounds.right) {
			// Haven't found the anchor yet.  Look from the right.
			for (anchor.h = pAL->dataBounds.right - 1; anchor.h > pAL->dataBounds.left; anchor.h--) {
				// If we find a selected cell, great.
				if (_ALCellIsSelected(&anchor, hAL))
					setIt = true;
				// If either found one or we hit theCell clicked, break.
				if (setIt || anchor.h == theCell.h)
					break;
			} // end for anchor.h
		} // end if
	} else {// end columns only
		setIt = false;
		for (anchor.h = pAL->dataBounds.left; anchor.h < pAL->dataBounds.right; anchor.h++) {
			for (anchor.v = pAL->dataBounds.top; anchor.v < pAL->dataBounds.bottom; anchor.v++) {
				// If we find a selected cell, that's the anchor.
				if (_ALCellIsSelected(&anchor, hAL))
					setIt = true;
				// If either found one or we hit theCell clicked, break.
				if (setIt || anchor.v == theCell.v)
					break;
			}
			// If either found one or we hit theCell clicked, break.
			if (setIt || anchor.h == theCell.h)
				break;
		} // end for anchor.h

		if (!setIt && (anchor.h != pAL->dataBounds.right || anchor.v != pAL->dataBounds.bottom)) {
			// Haven't found the anchor yet.  Look from the right/bottom.
			for (anchor.h = pAL->dataBounds.right - 1; anchor.h > pAL->dataBounds.left; anchor.h--) {
				for (anchor.v = pAL->dataBounds.bottom - 1; anchor.v > pAL->dataBounds.top; anchor.v--) {
					// If we find a selected cell, great.
					if (_ALCellIsSelected(&anchor, hAL))
						setIt = true;
					// If either found one or we hit theCell clicked, break.
					if (setIt || anchor.v == theCell.v)
						break;
				}
				// If either found one or we hit theCell clicked, break.
				if (setIt || anchor.h == theCell.h)
					break;
			} // end for anchor.h
		} // end if
	}

	// Now calculate the rectangle to select.
	selectRect.top = anchor.v;
	selectRect.left = anchor.h;
	selectRect.bottom = theCell.v;
	selectRect.right = theCell.h;
	// Make sure it's a proper rectangle.
	_ALReorder(&selectRect.top, &selectRect.bottom);
	_ALReorder(&selectRect.left, &selectRect.right);

	// Determine if we should be selecting or de-selecting cells.
	if (BTST(pAL->features, alFSelUseSense))
		setIt = _ALCellIsSelected(&theCell, hAL);
	else
		setIt = true;

	_ALSelectRect(setIt, true, &selectRect, hAL);

	// Set the alFMouseTracking bit while we track the mouse.
	BSET(pAL->flags, alFMouseTracking);

	// Initialize edge so oldCell will be kept the first time through the loop.
	edge = kCaretWholeCell;

	// MOUSE TRACKING LOOP
	do {
		// Keep track of old cell.
		if (edge != kCaretNotInCell)
			oldCell = theCell;

		// get cell offset corresponding to mouse position
		edge = ALGetCellAndEdge(mouseLoc, &theCell, hAL);

		// If the user is changing cells, turn the old one off if it's not in the rectangle.
		if (edge != kCaretNotInCell && (theCell.v != oldCell.v || theCell.h != oldCell.h)) {
			selectRect.top = anchor.v;
			selectRect.left = anchor.h;
			selectRect.bottom = theCell.v;
			selectRect.right = theCell.h;
			// Make sure it's a proper rectangle.
			_ALReorder(&selectRect.top, &selectRect.bottom);
			_ALReorder(&selectRect.left, &selectRect.right);

			if (!LongPointInLongRect(&oldCell, &selectRect)) {
				// Reset every cell from oldCell to theCell.
				resetRect.top = oldCell.v;
				resetRect.left = oldCell.h;
				resetRect.bottom = theCell.v;
				resetRect.right = theCell.h;

				// Don't want to reset theCell itself.
				if (oldCell.v < theCell.v)
					resetRect.bottom--;
				else if (oldCell.v > theCell.v)
					resetRect.bottom++;
				if (oldCell.h < theCell.h)
					resetRect.right--;
				else if (oldCell.h > theCell.h)
					resetRect.right++;

				// Make sure it's a proper rectangle.
				_ALReorder(&resetRect.top, &resetRect.bottom);
				_ALReorder(&resetRect.left, &resetRect.right);
				_ALSelectRect(!setIt, true, &resetRect, hAL);
			}

			_ALSelectRect(setIt, true, &selectRect, hAL);
		}

		if (!local_ALStdClickLoop(hAL))
			break;

		// call the click loop callback, if any
		if (pAL->clickLoop != nil)
			if (!CallALClickLoopProc(hAL, pAL->clickLoop))
				break;

		// update mouse position
		GetMouse(&mouseLoc);

	} while(WaitMouseUp());

	// clear the alFMouseTracking bit
	BCLR(pAL->flags, alFMouseTracking);
}

#if ALIST_HEIRARCHICAL
// Returns true if the event was handled here.
static Boolean local_ALTrackDisclosureTriangle( Point mouseLoc, EventModifiers modifiers, const ALCellPtr theCell, ALHandle hAL )
{	Rect		box, cellRect;
	short	triangleCurrentState, trianglePressedState, saveRedrawState;
	Boolean	result = false;

	// Set up the states of the triangle.
	if ( ALIsRowExpanded( theCell->v, hAL) ) {
		triangleCurrentState = triEnabledDown;
		trianglePressedState = triPressedDown;
	} else {
		triangleCurrentState = triEnabledRight;
		trianglePressedState = triPressedRight;
	}

	_ALCalcCellRect( &cellRect, theCell, false, hAL);

	// Calculate the location of the triangle.
	box = cellRect;
	box.right = box.left - 2;
	box.left -= 12;
	box.top = (box.bottom + box.top) / 2 - 6;
	box.bottom = box.top + 12;

	if ( PtInRect( mouseLoc, &box ) ) {
		saveRedrawState = ALFeatureFlag( alFInhibitRedraw, alBitClear, hAL );

		_ALDrawDisclosureTriangle( &cellRect, true, trianglePressedState, hAL );
		while ( StillDown( ) ) {
			_ALDrawDisclosureTriangle( &cellRect, true,
						( PtInRect( mouseLoc, &box ) ) ? trianglePressedState : triangleCurrentState, hAL );
			GetMouse( &mouseLoc );
		}

		if ( PtInRect( mouseLoc, &box ) ) {
			KeyMap	keyMap;

			// Check the keyboard NOW for the option key.
			GetKeys( keyMap );
			if ( (keyMap[1] >> 2) & 0x01 )
				modifiers &= optionKey;

			// Change the state of the super row.
			if ( triangleCurrentState == triEnabledRight )
				ALExpandRow( theCell->v, modifiers != 0, hAL );
			else
				ALCollapseRow( theCell->v, modifiers != 0, hAL );

			result = true;
		}

		ALFeatureFlag( alFInhibitRedraw, saveRedrawState, hAL );
	}

	return result;
}
#endif

void _ALUpdateDragCaret(ALCellPtr dragLoc, short dragPos, ALHandle hAL)
{	ALPtr		pAL = *hAL;	// assume AL record is already locked
	unsigned long	currentTime;

	// get current time
	currentTime = TickCount();

	if (EqualLongPoint(dragLoc, &pAL->dragCaretLoc) && dragPos == pAL->dragCaretPos) {
		// drag caret location didn't change; blink the caret
		if ((currentTime > pAL->caretTime + GetCaretTime()) && (dragPos != kCaretNotInCell)) {
			_ALDrawCaret(&pAL->dragCaretLoc, pAL->dragCaretPos, hAL);
			BCHG(pAL->flags, alFDragCaretVisible);	// invert flag
			pAL->caretTime = currentTime;
		}
	} else {
		// drag caret offset did change
		// hide old caret, if it's showing
		if (BTST(pAL->flags, alFDragCaretVisible))
			_ALDrawCaret(&pAL->dragCaretLoc, pAL->dragCaretPos, hAL);

		// show new caret (unless the dragPos is kCaretNotInCell)
		if (dragPos != kCaretNotInCell) {
			_ALDrawCaret(dragLoc, dragPos, hAL);
			BSET(pAL->flags, alFDragCaretVisible);
			pAL->caretTime = currentTime;
		} else
			BCLR(pAL->flags, alFDragCaretVisible);
	
		// Remember drag caret position and location.
		pAL->dragCaretLoc = *dragLoc;
		pAL->dragCaretPos = dragPos;
	}
} // _ALUpdateDragCaret

ItemReference _ALGetItemRefFromCell(const ALCellPtr theCell)
{	return (theCell->v << 16) + (theCell->h + 1);
}

#if ALIST_FOR_WX_WINDOWS

/* Added this function to ignore controls other than the ones attached to the
   list box itself.  This is because wxwindows abuses SetOrigin horribly, and
   so _all_ controls are registered with top-left corners of 0,0, and if you
   let FindControl loose, it will come back with whatever control was registered
   first. Ugh. - John Clements, 2000 April */
   
static short local_ALFindControl(Point mouseLoc, ALPtr pAL, ControlHandle *whichControl);

static short local_ALFindControl(Point mouseLoc, ALPtr pAL, ControlHandle *whichControl)
{	short part;

	if ( PtInRect( mouseLoc, &pAL->dispRect ) ) {
		*whichControl = NULL;
		part = kControlListBoxPart;
	} else if ( pAL->vScroll && ( part = TestControl( pAL->vScroll, mouseLoc ) ) ) {
		*whichControl = pAL->vScroll;
		return part;
	} else if ( pAL->hScroll && ( part = TestControl( pAL->hScroll, mouseLoc ) ) ) {
		*whichControl = pAL->hScroll;
		return part;
	}
	*whichControl = NULL;
	return kControlNoPart;
}

#endif

#pragma mark -

ALIST_API void ALGetCellFromItemRef(ItemReference theItem, ALCell *theCell)
{	if (theCell != nil) {
		theCell->v = HiWord(theItem);
		theCell->h = LoWord(theItem) - 1;
	}
}

ALIST_API Boolean ALCanAcceptDrag(DragReference theDrag, ALHandle hAL)
{	ALPtr		pAL;
	unsigned short	numDragItems;
	unsigned short	dragItemIndex;
	ItemReference	theItem;
	Boolean		saveALLock;
	OSErr		err;
	Boolean		retval = false;

	if (hAL == nil || *hAL == nil || theDrag == nil || !BTST((*hAL)->features, alFReceiveDrags))
		return retval;

	// Lock the AL record.
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	// Check to see if we should accept drags from self.
	if (theDrag == pAL->currentDrag)
		if (!BTST(pAL->features, alFDragToSelf))
			goto cleanup;

	// Count items in this theDrag.
	if ((err = CountDragItems(theDrag, &numDragItems)) != noErr)
		goto cleanup;

	for (dragItemIndex = 1; dragItemIndex <= numDragItems && !retval; dragItemIndex++) {
		// Get item reference number for current drag item.
		err = GetDragItemReferenceNumber(theDrag, dragItemIndex, &theItem);

		// Get the flavors, if any.
		if (err == noErr && pAL->inputFlavorsHook != nil) {
			ALDataDescriptor	flavorDesc = {typeNull, nil};
			FlavorFlags		theFlags;
			Boolean			more;
			OSErr			tempErr;
			short			index;

			for (more = true, index = 0; more && !retval; index++) {
				tempErr = CallALInputFlavorsProc(index, &more, &flavorDesc, nil, nil, hAL, (*hAL)->inputFlavorsHook);
				_ALForgetHandle(&flavorDesc.dataHandle);
				if (tempErr == noErr) {
					tempErr = GetFlavorFlags(theDrag, theItem, flavorDesc.descriptorType, &theFlags);
					if (tempErr == noErr)
						// This item has an appropriate flavor.
						retval = true;
				}
			}
		} else if (err != noErr)
			goto cleanup;
	} // for

//	retval = true;

cleanup:
	// unlock the AL record
	_ALSetHandleLock((Handle) hAL, saveALLock);

	return retval;
} // ALCanAcceptDrag

ALIST_API Boolean ALClick(Point mouseLoc0, Point mouseLoc, EventModifiers modifiers, unsigned long clickTime, ALHandle hAL)
{	ALPtr			pAL;
	short			edge;
	ALCell			theCell, oldLastClickCell;
	Boolean			isMultipleClick = false, toContinue;
	Boolean			saveALLock;
	short			part;
	long				value;
	ControlHandle		whichControl;
	QDDrawingState	saveState;
#if ALIST_HEIRARCHICAL
	unsigned long		offset;
#endif

	if (hAL == nil || *hAL == nil)
		return false;

	SaveQDDrawingState( &saveState, true );

	// lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	oldLastClickCell.h = pAL->lastClickCell.h;
	oldLastClickCell.v = pAL->lastClickCell.v;
	pAL->lastClickCell.h = pAL->lastClickCell.v = -1;

	part = 0;
	whichControl = NULL;

	if (pAL->hScroll) {
	  part = TestControl(pAL->hScroll, mouseLoc0);
	  if (part)
	    whichControl = pAL->hScroll;
	}
	if (!part && pAL->vScroll) {
	  part = TestControl(pAL->vScroll, mouseLoc0);
	  if (part)
	    whichControl = pAL->vScroll;
	}

	if (!part) {
#if ALIST_FOR_WX_WINDOWS
	  part = local_ALFindControl( mouseLoc0, pAL, &whichControl );
#else
	  part = FindControl( mouseLoc0, pAL->winRef, &whichControl );
#endif
	}

	if ( part != kControlListBoxPart && whichControl != nil && part != kControlNoPart ) {

		// Need this for a non-dynamic scroll indicator.
		value = LCGetValue(whichControl);

		// Track while mouseDown, using special tracking procedure if we want a dynamic scroll bar.
		if ( part == kControlIndicatorPart && BTST((*hAL)->flags, alFHasLiveScrollbars) == 0 ) {
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
			// Can't do this on Carbon because the ControlActionProc scroll indicator hack is not good.
			if (BTST(pAL->features, alFDynamicScroll)) {
				IndicatorActionUPP	thumbTracker;

				// Use the dynamic thumb tracker actionProc.
				if ( whichControl == pAL->hScroll )
					thumbTracker = NewIndicatorActionProc( local_ALHorzThumbActionProc );
				else
					thumbTracker = NewIndicatorActionProc( local_ALVertThumbActionProc );

				dynamicCntl = whichControl;
				dynamicOriginalValue = value;
				local_ALDisableDrawing();
				TrackControl(whichControl, mouseLoc0, (ControlActionUPP)thumbTracker);
				local_ALEnableDrawing();

				DisposeRoutineDescriptor( thumbTracker );
			} else
#endif
				// Don't use the actionProc that we installed for the scroll bar.
				TrackControl( whichControl, mouseLoc0, nil );
		} else
			// Use the actionProc installed for the scroll bar.
			TrackControl( whichControl, mouseLoc0, (ControlActionUPP)-1L );

		// Almost all of the redrawing is done in the tracking procedure, but this case needs to be done here.
		if ( part == kControlIndicatorPart && BTST((*hAL)->flags, alFHasLiveScrollbars) == 0 ) {
			// Want to always do part of this if there was a change and we were dragging the indicator.
			// Mainly, while dragging the indicator is darkened.  SetControlValue needs to be called,
			// here via _ALCalcVisibleCells, in order to let the control know it's done being dragged around.
			// Trust me, indicators are wierd!
			if (BTST(pAL->features, alFDynamicScroll)) {
				LCSetValue(whichControl, LCGetValue(whichControl));
				// This remove excess highlighting.
				DrawOneControl( whichControl );
			} else {
				// If we're not using a dynamic scroll, synch the long control to the Toolbox control value.
				LCSynch(whichControl);

				if (whichControl == pAL->vScroll)
					pAL->visCells.top = LCGetValue(whichControl);
				else
					pAL->visCells.left = LCGetValue(whichControl);

				_ALCalcVisibleCells(hAL);
				ALUpdate(nil, hAL);
			}
		}

		// Done!
		goto cleanup;
	}

	// find click offset
	edge = ALGetCellAndEdge(mouseLoc, &theCell, hAL);

	// Must have hit a non-active scroll bar or the grow zone, or beyond the end of the list.
	if (edge == kCaretNotInCell) {
		pAL->clickCount = 0;
		// ALSetSelectNone(true, hAL);
		goto cleanup;
	}

	// determine whether this click is part of a sequence
	isMultipleClick = ( (clickTime < pAL->clickTime + GetDblTime()) && EqualLongPoint(&theCell, &oldLastClickCell) );
	if ( isMultipleClick )
		pAL->clickCount++;
	else
		pAL->clickCount = 0;

	// remember click time, click offset and cell value
	pAL->clickTime = clickTime;
	pAL->clickLoc = mouseLoc;
	pAL->lastClickCell = theCell;

	if ( pAL->clickCellHook != nil ) {
		toContinue = CallALClickCellProc( &theCell, mouseLoc, modifiers, pAL->clickCount + 1, hAL, pAL->clickCellHook );
		if ( !toContinue ) {
			// Set the clickTime back; this resets the multiple click values.
			pAL->clickTime = clickTime - GetDblTime( );
			isMultipleClick = false;
		}
	} else
		toContinue = true;

	if (!toContinue)
		goto cleanup;

#if ALIST_HEIRARCHICAL
	// Check to see if the user clicked in a heirarchical triangle.
	if ( BTST( pAL->features, alFHeirarchical ) ) {
		offset = _ALCalcOffsetFromCell( &theCell, &pAL->dataBounds );
		if ( BTST( (*pAL->hSelected)[ offset ], alSsuperrow ) ) {
			// Could have been in the triangle.
			if ( local_ALTrackDisclosureTriangle( mouseLoc, modifiers, &theCell, hAL ) ) {
				// Make sure that clicking in the triangles do not produce multiple clicks.
				isMultipleClick = false;
				pAL->clickCount = 0;
				goto cleanup;
			}
		}
	}
#endif

	// If the shift key was held down and we can select more than one at a timeŠ
	if ((modifiers & shiftKey) != 0 && !BTST(pAL->features, alFSelOnlyOne)) {
		pAL->clickCount = 0;
		local_ALShiftClick(hAL);
		goto cleanup;
	}

	// If the command key was held down and we can select more than one at a timeŠ
	if ((modifiers & cmdKey) != 0 && !BTST(pAL->features, alFSelOnlyOne)) {
		pAL->clickCount = 0;
		local_ALCommandClick(hAL);
		goto cleanup;
	}

	// is this click part of a sequence or is it a single click?
	if ( !isMultipleClick ) {
		// single-click
		// if the Drag Manager is available and the click went into a selected cell,
		// this click may be the beginning of a drag gesture
		if (BTST(pAL->flags, alFHasDragManager) && BTST(pAL->features, alFStartDrags))
			if (ALGetSelect(false, &theCell, hAL))
				if (_ALDrag(mouseLoc, modifiers, clickTime, hAL) != alNoDragErr)
					goto cleanup;

		_ALSelectOnlyOne(true, &theCell, hAL);
	}

	// set the alFMouseTracking bit while we track the mouse
	BSET( pAL->flags, alFMouseTracking );

	// MOUSE TRACKING LOOP
	do {

		// get cell offset corresponding to mouse position
		edge = ALGetCellAndEdge(mouseLoc, &theCell, hAL);

		if (edge != kCaretNotInCell && theCell.v < pAL->visCells.bottom && theCell.h < pAL->visCells.right) {
			// Single click.
			if (!_ALCellIsSelected(&theCell, hAL))
				// Changed which cell is selected.
				_ALSelectOnlyOne(true, &theCell, hAL);
		} // end theCell is visible
		else
			ALSetSelectNone(true, hAL);

		if (!local_ALStdClickLoop(hAL))
			break;

		// call the click loop callback, if any
		if (pAL->clickLoop != nil)
			if (!CallALClickLoopProc(hAL, pAL->clickLoop))
				break;

		// update mouse position
		GetMouse( &mouseLoc );

	} while( WaitMouseUp( ) );

	// clear the alFMouseTracking bit
	BCLR(pAL->flags, alFMouseTracking);

cleanup:
	// unlock the AL record
	_ALSetHandleLock((Handle)hAL, saveALLock);

	RestoreQDDrawingState( &saveState, true );

	return (isMultipleClick && (*hAL)->clickCount == 1);
} // ALClick

ALIST_API void ALLastClick( ALCell *theCell, ALHandle hAL )
{	if (hAL == nil || *hAL == nil || theCell == nil)
		return;

	*theCell = (*hAL)->lastClickCell;
} // ALLastClick

ALIST_API OSErr ALReceiveDrag( DragReference theDrag, ALHandle hAL )
{	ALPtr			pAL;
	ALData			theData;
	Point				mouse;
	ALCell			insertLocation, theCell;
	short			insertEdge, redrawStatus;
	unsigned short		dragItemIndex;
	unsigned short		numDragItems;
	ItemReference		theItem;
	char				space = kSpace;
	Boolean			isMove = false;
	Boolean			saveALLock;
	OSErr			err;
	long				rowAdded = alAddedNone, numberAdded, numberDeleted;
	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	QDDrawingState	saveState;

	if (hAL == nil || *hAL == nil || theDrag == nil)
		return paramErr;

	// lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	// set up the port
	GetGWorld( &saveWorld, &saveDevice );
	SetPortWindowPort(pAL->winRef);

	// hide the drag caret
	_ALUpdateDragCaret(&pAL->dragCaretLoc, kCaretNotInCell, hAL);

	// refuse this drag if it doesn't taste good
	err = badDragFlavorErr;
	if (!ALCanAcceptDrag(theDrag, hAL))
		goto cleanup;

	// If the drag hilite is showing, get rid of it.
	if (BTST(pAL->flags, alFHilited)) {
		SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
		if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
			SetThemeBackground( kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
		else
#endif
			if (ALFeatureFlag(alFNotepadBackground, alBitTest, hAL))
			_ALSetNotepadBackgroundColor( );
		HideDragHilite(theDrag);
		RestoreQDDrawingState( &saveState, true );
		BCLR(pAL->flags, alFHilited);
	}

	// Make sure that the list won't show the changes as I move things around.
	redrawStatus = ALFeatureFlag( alFInhibitRedraw, alBitSet, hAL );

	// get drop location in local coordinates
	if ((err = GetDragMouse(theDrag, &mouse, nil)) != noErr)
		goto cleanup;
	GlobalToLocal(&mouse);

	// For the drag to be accepted, the drop location must be within the view region.
	err = dragNotAcceptedErr;
	if (!PtInRgn(mouse, pAL->viewRgn))
		goto cleanup;

	// Get drop offset into the list.
	insertEdge = ALGetCellAndEdge(mouse, &insertLocation, hAL);
	if (insertEdge == kCaretNotInCell) {
		insertEdge = kCaretBottom;
		insertLocation.v = (*hAL)->dataBounds.bottom - 1;
	}

	// The drop location must not be in a selected cell if it is from the same A List.
	if (theDrag == (*hAL)->currentDrag && _ALCellIsSelected(&insertLocation, hAL))
		goto cleanup;

	// Make sure it goes after the correct cell.
	if (insertEdge == kCaretTop) {
		insertLocation.v--;
		insertEdge = kCaretBottom;
	}

	if (insertEdge == kCaretLeft) {
		insertLocation.h--;
		insertEdge = kCaretRight;
	}

	// Check to see if the user wants to move cells within the same list.
	if (theDrag == pAL->currentDrag)
		isMove = !_ALIsOptionDrag(theDrag);

	// Hide selection highlighting.
	_ALHiliteSelected(hAL);

	// Count items in this drag.
	if ((err = CountDragItems(theDrag, &numDragItems)) != noErr)
		goto cleanup;

	numberAdded = 0;
	theCell = insertLocation;

	for (dragItemIndex = 1; dragItemIndex <= numDragItems; dragItemIndex++) {
		// Get item reference number for current drag item.
		if ((err = GetDragItemReferenceNumber(theDrag, dragItemIndex, &theItem)) != noErr)
			break;

		// See if this drag item contains a good flavor.
		err = _ALExtractFlavor(theDrag, theItem, &theData, hAL);
		if (err == noErr) {
			// Insert a new cell at the insertLocation.
			if (insertEdge == kCaretBottom)
				rowAdded = ALAddRow(1, ++theCell.v, hAL);
			else if (insertEdge == kCaretRight)
				rowAdded = ALAddColumn(1, ++theCell.h, hAL);

			if (rowAdded == alAddedNone) {
				err = errAEEventNotHandled;
				break;
			} else
				numberAdded++;

			err = ALSetCell(theData, &theCell, hAL);
		}
		else if (err != alSkipDragItem)
			break;
	} // for dragItemIndex

	if (isMove && numberAdded != 0) {
		numberDeleted = 0;

		// Need to remove the old cells.
		for (dragItemIndex = 1; dragItemIndex <= numDragItems; dragItemIndex++) {
			// Get item reference number for current drag item.
			if ((err = GetDragItemReferenceNumber(theDrag, dragItemIndex, &theItem)) != noErr)
				continue;

			ALGetCellFromItemRef(theItem, &theCell);
			if (insertEdge == kCaretBottom) {
				// Delete the appropriate row.
				if (theCell.v > insertLocation.v)
					theCell.v += numberAdded;
				theCell.v -= numberDeleted;

				ALDelRow(1, theCell.v, hAL);
			} else if (insertEdge == kCaretRight) {
				// Delete the appropriate column.
				if (theCell.h > insertLocation.h)
					theCell.h += numberAdded;
				theCell.h -= numberDeleted;

				ALDelColumn(1, theCell.h, hAL);
			}

			numberDeleted++;
		}
	} // if isMove

	// Reset the redraw status.
	ALFeatureFlag(alFInhibitRedraw, redrawStatus, hAL);

	// Redraw the list.
	_ALCalcVisibleCells(hAL);
	ALUpdate(nil, hAL);

cleanup:
	// restore the port
	SetGWorld( saveWorld, saveDevice );

	// unlock the AL record
	_ALSetHandleLock((Handle) hAL, saveALLock);

	// If we added something, but skipped the last item, there isn't an error.
	if (rowAdded != alAddedNone && err == alSkipDragItem)
		err = noErr;

	// return result code
	return err;
} // ALReceiveDrag

ALIST_API OSErr	ALTrackDrag(DragTrackingMessage theMessage, DragReference theDrag, ALHandle hAL)
{	ALPtr			pAL;
	Point				mouse;
	RgnHandle			tmpRgn;
	ALCell			location;
	short			position;
	unsigned long		currentTime;
	Boolean			saveALLock;
	OSErr			err;
	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	QDDrawingState	saveState;

	if (hAL == nil || *hAL == nil || theDrag == nil)
		return dragNotAcceptedErr;

	GetGWorld( &saveWorld, &saveDevice );

	// lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	// dispatch on theMessage
	switch (theMessage) {
#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0300)
	case dragTrackingEnterWindow:
#else
	case kDragTrackingEnterWindow:
#endif
		// determine whether we can accept this drag
		if (ALCanAcceptDrag(theDrag, hAL))
			BSET(pAL->flags, alFCanAcceptDrag);
		else {
			BCLR(pAL->flags, alFCanAcceptDrag);
			err = badDragFlavorErr;
			goto cleanup;
		}

		// reset clickTime
		pAL->clickTime = 0;
	break;

#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0300)
	case dragTrackingInWindow:
#else
	case kDragTrackingInWindow:
#endif
		if (BTST(pAL->flags, alFCanAcceptDrag)) {
			SetPortWindowPort( pAL->winRef );

			// get current mouse location in local coordinates
			if ( ( err = GetDragMouse( theDrag, &mouse, nil  ) ) != noErr )
				goto cleanup;
			GlobalToLocal(&mouse);

			if (PtInRgn(mouse, pAL->viewRgn)) {
				// mouse is in list area
				// hilite the list rectangle, if we haven't already
				// and if the drag has left sender window since drag tracking started
				if ((!BTST(pAL->flags, alFHilited))) {
					tmpRgn = NewRgn();
					CopyRgn(pAL->viewRgn, tmpRgn);
					// Save the background color and alternate it to the notepad background
					SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
					if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL)) {
						SetThemeBackground( kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
					} else
#endif
						if (ALFeatureFlag(alFNotepadBackground, alBitTest, hAL))
						_ALSetNotepadBackgroundColor( );
					ShowDragHilite( theDrag, tmpRgn, true );
					DisposeRgn(tmpRgn);
					RestoreQDDrawingState( &saveState, true );
					BSET(pAL->flags, alFHilited);
				}

				// Get the cell location and position corresponding to mouse location.
				position = ALGetCellAndEdge(mouse, &location, hAL);
				if (position == kCaretNotInCell) {
					position = kCaretBottom;
					location.v = (*hAL)->dataBounds.bottom - 1;
				}

				// If the cell location is within the original selection range, don't display drag feedback.
				if (theDrag == pAL->currentDrag)
					if (ALGetSelect(false, &location, hAL))
						position = kCaretNotInCell;

				// Provide a drag feedback in the form of a blinking caret.
				_ALUpdateDragCaret(&location, position, hAL);

				// clear clickTime
				pAL->clickTime = 0;
			} else {

				// mouse is outside list area
				// dehilite the list rectangle, if it's hilited
				if (BTST(pAL->flags, alFHilited)) {
					// Save the background color and alternate it to the notepad background
					SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
					if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
						SetThemeBackground(kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
					else
#endif
						if (ALFeatureFlag(alFNotepadBackground, alBitTest, hAL))
						_ALSetNotepadBackgroundColor( );
					HideDragHilite(theDrag);
					RestoreQDDrawingState( &saveState, true );
					BCLR(pAL->flags, alFHilited);
				}

				// hide the drag caret, if it's showing
				_ALUpdateDragCaret(&pAL->dragCaretLoc, kCaretNotInCell, hAL);

				// if the mouse has been remaining outside the view region for 10 ticks or more
				// and this drag was not created by this AL instance, call the click loop routine
				currentTime = TickCount( );
				if (pAL->clickTime == 0)
					pAL->clickTime = currentTime;
				else if (currentTime > pAL->clickTime + kAL_AutoScrollDelay) {
					local_ALStdClickLoop(hAL);

					if ( pAL->clickLoop != nil )
						CallALClickLoopProc(hAL, pAL->clickLoop);
				}
			}
		} else {
			err = dragNotAcceptedErr;
			goto cleanup;
		}
	break; // case dragTrackingInWindow

#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0300)
	case dragTrackingLeaveWindow:
#else
	case kDragTrackingLeaveWindow:
#endif
		// drag has left this window
		// dehilite the list area if necessary
		if (BTST(pAL->flags, alFHilited)) {
			SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
			if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
				SetThemeBackground( kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
			else
#endif
				if (ALFeatureFlag(alFNotepadBackground, alBitTest, hAL))
				_ALSetNotepadBackgroundColor( );
			HideDragHilite(theDrag);
			RestoreQDDrawingState( &saveState, true );
			BCLR(pAL->flags, alFHilited);
		}

		// hide the drag caret, if it's showing
		_ALUpdateDragCaret(&pAL->dragCaretLoc, kCaretNotInCell, hAL);

	break;

	default:
	break;
	} // case theMessage

	// clear result code
	err = noErr;

cleanup:
	// unlock the AL record
	_ALSetHandleLock((Handle) hAL, saveALLock);

	SetGWorld( saveWorld, saveDevice );

	// return result code
	return err;
} // ALTrackDrag
