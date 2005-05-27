/*
 *	ALSelecting.c
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
#ifndef __TOOLUTILS__
	#include <ToolUtils.h>
#endif
#include "AListInternal.h"
#include "QDDrawingState.h"

static void local_ALHiliteCell(const ALCellPtr theCell, unsigned long offset, ALHandle hAL);

static void refresh(ALHandle hAL)
{
  void *ctl;
  ALGetInfo(alRefCon, &ctl, hAL);
  HIViewSetNeedsDisplay((ControlHandle)ctl, TRUE);
}

static void local_ALHiliteCell(const ALCellPtr theCell, unsigned long offset, ALHandle hAL)
{	ALPtr	pAL;
	RgnHandle	saveClip, auxRgn;
	CGrafPtr	savePort;
	GDHandle saveDev;
#if ALIST_HAVE_CELLDATA
	Boolean	saveDataLock;
#else
	#pragma unused( offset )
#endif
	Rect		cellRect;

	if (BTST((*hAL)->features, alFInhibitRedraw)) {
	  refresh(hAL);
	  return;
	}

	// The AL record must be already locked.
	pAL = *hAL;

	// Set up the port.
	GetGWorld(&savePort, &saveDev);
	SetPortWindowPort(pAL->winRef);

	// Create auxiliary regions.
	saveClip = NewRgn();
	auxRgn = NewRgn();

	// Restrict the clip region to the view rectangle.
	GetClip(saveClip);
	SectRgn(saveClip, pAL->viewRgn, auxRgn);
	SetClip(auxRgn);

	_ALCalcCellRect(&cellRect, theCell, false, hAL);

#if ALIST_HAVE_CELLDATA
	saveDataLock = _ALSetHandleLock((Handle)pAL->hData, true);
#endif

	if (pAL->hiliteCellHook != nil)
#if ALIST_HAVE_CELLDATA
		CallALHiliteCellProc((*pAL->hData)[offset], theCell, BTST(pAL->flags, alFActive) ? true : false,
							BTST(pAL->features, alFOutlineHilite) ? true : false,
							&cellRect, hAL, pAL->hiliteCellHook);
#else
		CallALHiliteCellProc( nil, theCell, BTST(pAL->flags, alFActive) ? true : false,
							BTST(pAL->features, alFOutlineHilite) ? true : false,
							&cellRect, hAL, pAL->hiliteCellHook );
#endif

#if ALIST_HAVE_CELLDATA
	_ALSetHandleLock( (Handle)pAL->hData, saveDataLock );
#endif

	// restore the clip region
	SetClip(saveClip);

	// dispose of all regions
	DisposeRgn(saveClip);
	DisposeRgn(auxRgn);

	// restore the port
	SetGWorld(savePort, saveDev);
} // local_ALHiliteCell

void _ALHiliteSelected(ALHandle hAL)
{	Boolean			saveLock;
	LongRect			r, bounds;
	ALCell			theCell;
	ALSelectionPtr		selPtr;
	RgnHandle			hiliteRgn, auxRgn;
	unsigned long		offset;
	unsigned char		newMode, saveMode;
	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	QDDrawingState	saveState;

	if (BTST((*hAL)->features, alFInhibitRedraw)) {
	  refresh(hAL);
	  return;
	}

	// Hilite the selection range
	if (BTST((*hAL)->flags, alFActive)) {
		// If we're active, hilite each cell accordingly.

		r = (*hAL)->visCells;
		bounds = (*hAL)->dataBounds;

		saveLock = _ALSetHandleLock((Handle)(*hAL)->hSelected, true);
		selPtr = *(*hAL)->hSelected;

		// Go across the columns.
		for (theCell.h = r.left; theCell.h < r.right; theCell.h++)
			// Go down the rows.
			for (theCell.v = r.top; theCell.v < r.bottom; theCell.v++) {
				if (BTST(selPtr[offset = _ALCalcOffsetFromCell(&theCell, &bounds)], alSselected))
					local_ALHiliteCell(&theCell, offset, hAL);
			} // end rows

		_ALSetHandleLock((Handle)(*hAL)->hSelected, saveLock);
	} else {
		// If we're inactive, put an outline around the hilited cells.
		hiliteRgn = ALGetSelectedHiliteRgn(hAL);
		auxRgn = NewRgn();
		// We need just the outline of this region
		CopyRgn(hiliteRgn, auxRgn);
		InsetRgn(auxRgn, 2, 2);
		DiffRgn(hiliteRgn, auxRgn, hiliteRgn);
		DisposeRgn( auxRgn );

		// Save the old hilite mode.
		saveMode = LMGetHiliteMode();
		// Set the hilite mode.
		newMode = saveMode;
		BitClr((Ptr)(&newMode), (long)pHiliteBit);
		LMSetHiliteMode(newMode);

		// Set the background color appropriately.
		GetGWorld(&saveWorld, &saveDevice);
		SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
		if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
			SetThemeBackground( kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
		else
#endif
			if ( ALFeatureFlag(alFNotepadBackground, alBitTest, hAL ) )
			_ALSetNotepadBackgroundColor( );

		InvertRgn(hiliteRgn);
		DisposeRgn(hiliteRgn);
		// Restore the old hilite mode.
		LMSetHiliteMode(saveMode);
		// Restore the background color.
		RestoreQDDrawingState( &saveState, true );
	}
}

Boolean _ALCellIsSelected(const ALCellPtr theCell, ALHandle hAL)
{	unsigned long	offset;

	offset = _ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds);

	return BTST((*(*hAL)->hSelected)[offset], alSselected);
} // _ALCellIsSelected;

void _ALSelectRect(Boolean turnOn, Boolean redrawIfChange, const LongRect *selectRect, ALHandle hAL)
{	unsigned long	offset;
	Boolean		saveSLLock;
	ALSelectionPtr	pSelect;
	ALCell		aCell;
	LongRect		theRect, dataBounds;

	saveSLLock = _ALSetHandleLock((Handle)(*hAL)->hSelected, true);
	pSelect = *(*hAL)->hSelected;

	// Make a local copy for speed.
	dataBounds = (*hAL)->dataBounds;

	// Limit the selection rectangle to the dataBounds.
	theRect = *selectRect;
	if (theRect.left < dataBounds.left)
		theRect.left = dataBounds.left;
	if (theRect.right > dataBounds.right)
		theRect.right = dataBounds.right;
	if (theRect.top < dataBounds.top)
		theRect.top = dataBounds.top;
	if (theRect.bottom >  dataBounds.bottom)
		theRect.bottom = dataBounds.bottom;

	for (aCell.h = theRect.left; aCell.h <= theRect.right; aCell.h++) {
		for (aCell.v = theRect.top; aCell.v <= theRect.bottom; aCell.v++) {
			offset = _ALCalcOffsetFromCell(&aCell, &dataBounds);
			if (turnOn && !BTST(pSelect[offset], alSselected)) {
				BSET(pSelect[offset], alSselected);
				if (redrawIfChange)
					ALDrawCell(&aCell, hAL);
			} else if (!turnOn && BTST(pSelect[offset], alSselected)) {
				BCLR(pSelect[offset], alSselected);
				if (redrawIfChange)
					ALDrawCell(&aCell, hAL);
			}
		} // end row loop
	} // end column loop

	// Reset the handle lock
	_ALSetHandleLock((Handle)(*hAL)->hSelected, saveSLLock);
} // _ALSelectRect

void _ALSelectOnlyOne(Boolean redrawIfChange, const ALCellPtr theCell, ALHandle hAL)
{	unsigned long	offset, length;
	Boolean		saveSLLock;
	ALSelectionPtr	pSelect;
	register long	i;
	ALCell		aCell;

	offset = _ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds);
	length = (*hAL)->dataLength;

	saveSLLock = _ALSetHandleLock((Handle)(*hAL)->hSelected, true);
	pSelect = *(*hAL)->hSelected;

	for (i = 0; i < length; i++) {
		// If it is on and not the one I want on, turn it off.
		if (BTST(pSelect[i], alSselected) && i != offset) {
			BCLR(pSelect[i], alSselected);
			if ( redrawIfChange ) {
				_ALCalcCellFromOffset(&aCell, i, &(*hAL)->dataBounds);
				ALDrawCell( &aCell, hAL );
			}
		}
	}

	// Turn this one on.
	if (!BTST(pSelect[offset], alSselected)) {
		BSET(pSelect[offset], alSselected);
		if (redrawIfChange)
			ALDrawCell( theCell, hAL );
	}

	// Reset the handle lock
	_ALSetHandleLock((Handle)(*hAL)->hSelected, saveSLLock);
} // _ALSelectOnlyOne

#pragma mark -

// Modifies theCell to contain the cell location and returns the closest edge.
ALIST_API short ALGetCellAndEdge(Point thePoint, ALCellPtr theCell, ALHandle hAL)
{	// Given a long point in local coordinates,
	// find the corresponding cell number and which edge is closest.
	ALCell		calcCell;
	Point			pt;
	Rect			box;
	short		angle, result;
#if ALIST_HEIRARCHICAL
	long			tempValue;
	ALCell		tempCell;
	unsigned long	superRowOffset, offset;
#endif

	// Sanity check!
	if ( hAL == nil || *hAL == nil || theCell == nil )
		return kCaretNotInCell;

	// If the point wasn't inside the dispRect, it's not in any cell!
	if (thePoint.h < (*hAL)->dispRect.left || thePoint.h > (*hAL)->dispRect.right ||
			thePoint.v < (*hAL)->dispRect.top || thePoint.v > (*hAL)->dispRect.bottom)
		return kCaretNotInCell;

	// Make a box of size cellSize.
	box.top = box.left = 0;
	box.right = (*hAL)->dispRect.right - (*hAL)->dispRect.left;
	// Check for bad rectangle and limit it to the cellSize.
	if (box.right <= 0)
		box.right = 1;
	else if ((*hAL)->cellSize.h < box.right)
		box.right = (*hAL)->cellSize.h;
	box.bottom = (*hAL)->dispRect.bottom - (*hAL)->dispRect.top;
	// Check for bad rectangle and limit it to the cellSize.
	if (box.bottom <= 0)
		box.bottom = 1;
	else if ((*hAL)->cellSize.v < box.bottom)
		box.bottom = (*hAL)->cellSize.v;

	// Get some temporary offsets.
	calcCell.h = thePoint.h - (*hAL)->dispRect.left;
	calcCell.v = thePoint.v - (*hAL)->dispRect.top;

	// Calculate position within cell.
	pt.h = calcCell.h % box.right;
	pt.v = calcCell.v % box.bottom;

	// Calculate actual cell number.
	calcCell.h = calcCell.h / box.right + (*hAL)->visCells.left;
	calcCell.v = calcCell.v / box.bottom;

#if ALIST_HEIRARCHICAL
	if ( BTST( (*hAL)->features, alFHeirarchical ) ) {
		// Found the cell within the display rectangle.  Now, which item in the list does that correspond to?
		tempCell.h = (*hAL)->dataBounds.left;
		tempCell.v = (*hAL)->visCells.top;
		for ( tempValue = 0; tempValue < calcCell.v; tempValue++, tempCell.v++ ) {
			superRowOffset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );
			if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
						!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) ) {
				for ( tempCell.v++; tempCell.v < (*hAL)->visCells.bottom; tempCell.v++ ) {
					offset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );
					if ( (*(*hAL)->hLevels)[ offset ] <= (*(*hAL)->hLevels)[ superRowOffset ] ) {
						tempCell.v--;
						break;
					}
				}
			}
		}
		calcCell.v = tempCell.v;
	} else
#endif
		calcCell.v += (*hAL)->visCells.top;

	// Copy it to the return variable.
	*theCell = calcCell;

	if (calcCell.v >= (*hAL)->dataBounds.bottom || calcCell.h >= (*hAL)->dataBounds.right)
		return kCaretNotInCell;

	// Now determine which part of the cell the caret is in.
	if (BTST((*hAL)->features, alFRowsOnly)) {
		if (pt.v <= box.top + (box.bottom - box.top) / 2)
			result = kCaretTop;
		else if (pt.v <= box.bottom)
			result = kCaretBottom;
		else
			result = kCaretWholeCell;
	} else if (BTST((*hAL)->features, alFColumnsOnly)) {
		if (pt.h <= box.left + (box.right - box.left) / 2)
			result = kCaretLeft;
		else if (pt.h <= box.right)
			result = kCaretRight;
		else
			result = kCaretWholeCell;
	} else {
		if (pt.v <= box.top + kAL_CellMargin || pt.h <= box.left + kAL_CellMargin
				|| pt.v >= box.right - kAL_CellMargin || pt.h >= box.right - kAL_CellMargin) {
			// Near an edge of box.
			PtToAngle(&box, pt, &angle);
			if (angle > 45 && angle < 135)
				result = kCaretRight;
			else if (angle >= 135 && angle <= 225)
				result = kCaretBottom;
			else if (angle > 225 && angle < 315)
				result = kCaretLeft;
			else
				result = kCaretTop;
		} else
			// In middle of box.
			result = kCaretWholeCell;
	}

	return result;
} // ALGetCellAndEdge

ALIST_API RgnHandle ALGetCellHiliteRgn(const ALCellPtr theCell, ALHandle hAL)
{	// Returns the hilite region corresponding to the selected range.
	// The caller is responsible for disposing of the returned region
	// when it's finished with it.

	ALPtr		pAL;
	RgnHandle		hiliteRgn;
	Rect			selRect;
	CGrafPtr		savePort;
        GDHandle        saveDev;
	Boolean		saveALLock, saveSLLock;
	ALSelectionPtr	sPtr;
	unsigned long	offset;

	if (hAL == nil || *hAL == nil || theCell == nil)
		return nil;

	// lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	saveSLLock = _ALSetHandleLock((Handle)pAL->hSelected, true);
	sPtr = *pAL->hSelected;

	// Set up the port.
	GetGWorld(&savePort, &saveDev);
	SetPortWindowPort(pAL->winRef);

	// Open a region: rects to be hilited will be accumulated in this.
	OpenRgn();

	offset = _ALCalcOffsetFromCell(theCell, &pAL->dataBounds);

	_ALCalcCellRect(&selRect, theCell, false, hAL);
	FrameRect( &selRect );

	// copy the accumulated region into a new region
	hiliteRgn = NewRgn();
	CloseRgn(hiliteRgn);

	// restrict this region to the view region
	SectRgn(hiliteRgn, pAL->viewRgn, hiliteRgn);

	// restore the port
	SetGWorld(savePort, saveDev);

	// unlock the AL record
	_ALSetHandleLock((Handle)hAL, saveALLock);
	_ALSetHandleLock((Handle)(*hAL)->hSelected, saveSLLock);

	// return the hilite region
	return hiliteRgn;
} // ALGetCellHiliteRgn

ALIST_API long ALGetNumberSelected(ALHandle hAL)
{	long			numRows, dataLength;
	ALSelectionPtr	sPtr;
	long			i;

	if (hAL == nil || *hAL == nil)
		return 0;

	// Lock it down.
	HLock((Handle)(*hAL)->hSelected);
	sPtr = *(*hAL)->hSelected;

	dataLength = (*hAL)->dataLength;
	numRows = 0;

	for (i = 0; i < dataLength; i++)
		if (BTST(sPtr[i], alSselected))
			numRows++;

	// Unlock it.
	HUnlock((Handle)(*hAL)->hSelected);

	return numRows;
} // ALGetNumberSelected

ALIST_API RgnHandle ALGetSelectedHiliteRgn(ALHandle hAL)
{	// Returns the hilite region corresponding to the selected range.
	// The caller is responsible for disposing of the returned region
	// when it's finished with it.
	ALPtr		pAL;
	RgnHandle		hiliteRgn;
	Rect			selRect;
	CGrafPtr		savePort;
        GDHandle        saveDev;
	Boolean		saveALLock, saveSLLock;
	ALCell		theCell;
	ALSelectionPtr	sPtr;
	unsigned long	offset;

	if (hAL == nil || *hAL == nil)
		return nil;

	// lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	saveSLLock = _ALSetHandleLock((Handle)pAL->hSelected, true);
	sPtr = *pAL->hSelected;

	// Set up the port.
	GetGWorld(&savePort, &saveDev);
	SetPortWindowPort(pAL->winRef);

	// Open a region: rects to be hilited will be accumulated in this.
	OpenRgn();

	// Go across the columns.
	for (theCell.h = pAL->visCells.left; theCell.h < pAL->visCells.right; theCell.h++)
		// Go down the rows.
		for (theCell.v = pAL->visCells.top; theCell.v < pAL->visCells.bottom; theCell.v++) {
			offset = _ALCalcOffsetFromCell(&theCell, &pAL->dataBounds);
			// If this cell is selected, add it to the hilite region.
			if (BTST(sPtr[offset], alSselected)) {
				_ALCalcCellRect(&selRect, &theCell, false, hAL);
				FrameRect(&selRect);
			} // end if
		} // end for theCell.v

	// copy the accumulated region into a new region
	hiliteRgn = NewRgn();
	CloseRgn(hiliteRgn);

	// restrict this region to the view region
	SectRgn(hiliteRgn, pAL->viewRgn, hiliteRgn);

	// restore the port
	SetGWorld(savePort, saveDev);

	// unlock the AL record
	_ALSetHandleLock((Handle) hAL, saveALLock);
	_ALSetHandleLock((Handle)(*hAL)->hSelected, saveSLLock);

	// return the hilite region
	return hiliteRgn;
} // ALGetHiliteRgn

ALIST_API Boolean ALGetSelect(Boolean next, ALCell *theCell, ALHandle hAL)
{	Boolean		selected = false;
	ALSelectionPtr	sPtr;
	register long	i;
	unsigned long	offset, max;

	// Sanity check!
	if (hAL == nil || *hAL == nil || theCell == nil)
		return selected;

	// Make sure theCell is within dataBounds.
	if (!_ALCheckInsideBounds(theCell, &(*hAL)->dataBounds))
		return selected;

	HLock((Handle)(*hAL)->hSelected);
	sPtr = *(*hAL)->hSelected;

	// Search for theCell.
	offset = _ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds);

	if (BTST(sPtr[offset], alSselected))
		selected = true;

	if (!selected && next) {
		max = GetHandleSize((Handle)(*hAL)->hSelected) / sizeof(ALSelection);
		for (i = offset; i < max && !selected; i++) {
			if (BTST(sPtr[i], alSselected)) {
				_ALCalcCellFromOffset(theCell, i, &(*hAL)->dataBounds);
				selected = true;
			}
		}
	}

	HUnlock((Handle)(*hAL)->hSelected);

	return selected;
} // ALGetSelect

ALIST_API void ALSetSelect(Boolean setIt, ALCellPtr theCell, ALHandle hAL)
{	ALSelectionPtr	sPtr;
	Boolean		drawIt = false;
	unsigned long	offset;

	// Sanity check!
	if (hAL == nil || *hAL == nil || theCell == nil)
		return;

	// Make sure theCell is within dataBounds.
	if (!_ALCheckInsideBounds(theCell, &(*hAL)->dataBounds))
		return;

	HLock((Handle)(*hAL)->hSelected);
	sPtr = *(*hAL)->hSelected;

	offset = _ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds);

	if (setIt && !BTST(sPtr[offset], alSselected)) {
		// Switch it on.
		BSET(sPtr[offset], alSselected);
		drawIt = true;
	}
	else if (!setIt && BTST(sPtr[offset], alSselected)) {
		// Switch it off.
		BCLR(sPtr[offset], alSselected);
		drawIt = true;
	}

	HUnlock((Handle)(*hAL)->hSelected);

	// Redraw this cell, maybe.
	if ( drawIt && !BTST( (*hAL)->features, alFInhibitRedraw ) )
		ALDrawCell(theCell, hAL);
}

ALIST_API void ALSetSelectNone(Boolean redrawIfChange, ALHandle hAL)
{	unsigned long	length;
	Boolean		saveSLLock;
	ALSelectionPtr	pSelect;
	register long	i;
	ALCell		aCell;

	if (hAL == nil || *hAL == nil)
		return;

	length = (*hAL)->dataLength;

	saveSLLock = _ALSetHandleLock((Handle)(*hAL)->hSelected, true);
	pSelect = *(*hAL)->hSelected;

	for (i = 0; i < length; i++) {
		// If it is on, turn it off.
		if (BTST(pSelect[i], alSselected)) {
			BCLR(pSelect[i], alSselected);
			if (redrawIfChange) {
				_ALCalcCellFromOffset(&aCell, i, &(*hAL)->dataBounds);
				ALDrawCell(&aCell, hAL);
			}
		}
	}

	// Reset the handle lock
	_ALSetHandleLock((Handle)(*hAL)->hSelected, saveSLLock);
} // ALSetSelectNone

ALIST_API void ALSetSelectAll(Boolean redrawIfChange, ALHandle hAL)
{	unsigned long	length;
	Boolean		saveSLLock;
	ALSelectionPtr	pSelect;
	register long	i;
	ALCell		aCell;

	if (hAL == nil || *hAL == nil)
		return;

	length = (*hAL)->dataLength;

	saveSLLock = _ALSetHandleLock((Handle)(*hAL)->hSelected, true);
	pSelect = *(*hAL)->hSelected;

	for (i = 0; i < length; i++) {
		// If it is off, turn it on.
		if (!BTST(pSelect[i], alSselected)) {
			BSET(pSelect[i], alSselected);
			if (redrawIfChange) {
				_ALCalcCellFromOffset(&aCell, i, &(*hAL)->dataBounds);
				ALDrawCell(&aCell, hAL);
			}
		}
	}

	// Reset the handle lock
	_ALSetHandleLock((Handle)(*hAL)->hSelected, saveSLLock);
} // ALSetSelectAll
