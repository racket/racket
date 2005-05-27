/*
 *	ALScrolling.c
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
#include "AListInternal.h"
#include "LongControls.h"

ALIST_API Boolean ALAutoScroll( Point moveTo, const ALCellPtr whichCell, ALHandle hAL )
{	// If whichCell is nil, it attempts to move the first selected cell
	// (otherwise it attempts to move the given cell)
	// to the coordinates given (moveTo) in the port coordinate system.
	// Returns true if the view was changed, false if it did not move anything.
	ALSelectionPtr	pSelected;
	Boolean		result = false;
	Rect			r;
	ALCell		aCell, moveToCell;
	long			offset, dCols, dRows;

	// Sanity check!
	if (hAL == nil || *hAL == nil)
		return result;

	// Can't move to it if it's outside the list rectangle.
	r = (*hAL)->dispRect;
	if (!PtInRect(moveTo, &r))
		return result;

	if ( whichCell != nil )
		aCell = *whichCell;
	else {
		// Search for first selected cell.
		HLock((Handle)(*hAL)->hSelected);
		pSelected = *(*hAL)->hSelected;
		for (offset = 0; offset < (*hAL)->dataLength && !BTST(pSelected[offset], alSselected); offset++)	;
		HUnlock((Handle)(*hAL)->hSelected);

		// If we found it, determine which cell it is, otherwise we're done.
		if (offset != (*hAL)->dataLength)
			_ALCalcCellFromOffset( &aCell, offset, &(*hAL)->dataBounds );
		else
			return result;
	}

	// Move aCell to the point, if possible.
	// Determine which cell to move to.
	ALGetCellAndEdge( moveTo, &moveToCell, hAL );

	// Limit the horizontal scrolling to the limits of the list.
	dCols = moveToCell.h - aCell.h;
	if ((*hAL)->visCells.left - dCols < (*hAL)->dataBounds.left)
		dCols = (*hAL)->visCells.left - (*hAL)->dataBounds.left;
	else if ((*hAL)->hScroll != nil && (*hAL)->visCells.left - dCols > LCGetMaximum((*hAL)->hScroll))
		dCols = (*hAL)->visCells.left - LCGetMaximum((*hAL)->hScroll);

	// Limit the vertical scrolling to the limits of the list.
	dRows = moveToCell.v - aCell.v;
	if ( (*hAL)->vScroll != nil ) {
		if ( LCGetValue( (*hAL)->vScroll ) - dRows < LCGetMinimum( (*hAL)->vScroll ) )
			dRows = LCGetValue( (*hAL)->vScroll ) - LCGetMinimum( (*hAL)->vScroll );
		else if ( LCGetValue( (*hAL)->vScroll ) - dRows > LCGetMaximum( (*hAL)->vScroll ) )
			dRows = LCGetValue( (*hAL)->vScroll ) - LCGetMaximum( (*hAL)->vScroll );
	}

	// Go ahead and scroll if required.
	if (dCols != 0 || dRows != 0) {
		ALScrollCells( dCols, dRows, hAL );
		result = true;
	}

	return result;
} // ALAutoScroll

ALIST_API void ALScrollCells(long dCols, long dRows, ALHandle hAL)
{	long	hOffset, vOffset;

	// Do nothing if both scroll offsets are zero
	if ( hAL == nil || *hAL == nil || ( dCols == 0 && dRows == 0 ) )
		return;

	// Calculate the number of pixels to offset the list.
	hOffset = dCols * (*hAL)->cellSize.h;
	vOffset = dRows * (*hAL)->cellSize.v;

#if ALIST_HEIRARCHICAL
	if ( dRows != 0 && BTST( (*hAL)->features, alFHeirarchical ) ) {
		// Need to test if we're scrolling past hidden rows.
		ALCell		tempCell;
		LongRect		bounds = (*hAL)->dataBounds;
		unsigned long	superRowOffset, offset;
		long			checkingRows;

		if ( dRows < 0 ) {
			// We're trying to scroll down through the list.
			// If top-most cell(s) were hierarchical and not expanded, need to skip all the hidden rows.

			tempCell.v = (*hAL)->visCells.top;
			tempCell.h = bounds.left;
			for ( checkingRows = -dRows; checkingRows > 0 && tempCell.v < bounds.bottom; checkingRows--, tempCell.v++ ) {
				superRowOffset = _ALCalcOffsetFromCell( &tempCell, &bounds );
				if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
							!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) ) {
					for ( tempCell.v++; tempCell.v < bounds.bottom; tempCell.v++ ) {
						offset = _ALCalcOffsetFromCell( &tempCell, &bounds );
						if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] )
							// Skip all hidden rows.
							dRows--;
						else
							// Found the end of the collapsed rows.  Break out of the inner loop here.
							break;
					}
				}
			}
		} else {
			// We're scrolling UP through the list.
			tempCell.v = (*hAL)->visCells.top - 1;
			tempCell.h = bounds.left;
			for ( checkingRows = dRows; checkingRows > 0 && tempCell.v > bounds.top; checkingRows--, tempCell.v-- ) {
				while ( ALIsRowHidden( tempCell.v, hAL ) ) {
					// Skip all hidden rows.
					dRows++;
					tempCell.v--;
				}
			}
		}
	}
#endif

	// Offset the visible cells.
	OffsetLongRect( &(*hAL)->visCells, -dCols, -dRows );

	// Recalculate the visible cells.  This is necessary because you can have parts of cells showing.
	_ALCalcVisibleCells( hAL );

	ALScrollPixels( hOffset, vOffset, hAL );

	if ( hOffset != 0 && (*hAL)->scrollHorzProc != nil )
		CallALScrollProc( hAL, (*hAL)->scrollHorzProc );

	if ( vOffset != 0 && (*hAL)->scrollVertProc != nil )
		CallALScrollProc( hAL, (*hAL)->scrollVertProc );
} // ALScrollCells

ALIST_API void ALScrollPixels(long hOffset, long vOffset, ALHandle hAL)
{	ALPtr	pAL;
	Rect		viewRect;
	CGrafPtr	savePort;
	GDHandle saveDev;
	Boolean	hideOutline, saveALLock;

	// Do nothing if both scroll offsets are zero
	if ((hOffset == 0 && vOffset == 0) || hAL == nil || *hAL == nil)
		return;

	// Lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;
	
	// set up the port
	GetGWorld(&savePort, &saveDev);
	SetPortWindowPort(pAL->winRef);

	// get view rect in short coordinates
	GetRegionBounds( pAL->viewRgn, &viewRect );

	// if we're inactive and outline highlighting is on, we have to temporarily
	// hide the selection outline while scrolling to avoid a cosmetic bug
	hideOutline = false;
	if (!BTST(pAL->flags, alFActive))
		if (BTST(pAL->features, alFOutlineHilite)) {
			hideOutline = true;
			_ALHiliteSelected(hAL);
			BCLR(pAL->features, alFOutlineHilite);
		}

	// Scroll the view rectangle.
	// We use ScrollRect unless the whole text is to be redrawn anyway
	// notice that both ScrollRect and DragPreScroll take short (16-bit)
	// offset parameters, while ALScroll deals with long (32-bit) quantities.
	if ((ABS(hOffset) < (viewRect.right - viewRect.left)) && (ABS(vOffset) < (viewRect.bottom - viewRect.top))) {
		RgnHandle updateRgn = NewRgn();

		// If we're currently tracking a drag, notify the Drag Manager we're about to scroll.
		if (pAL->currentDrag != nil)
			DragPreScroll(pAL->currentDrag, (short) hOffset, (short) vOffset);

		// ScrollRect will set updateRgn to the region to redraw
		ScrollRect(&viewRect, (short) hOffset, (short) vOffset, updateRgn);

		if (pAL->currentDrag != nil)
			DragPostScroll(pAL->currentDrag);
		
		// Redraw the exposed region.
		ALUpdate(updateRgn, hAL);
		DisposeRgn(updateRgn);
	}
	else
		// redraw the whole list but not the borders
		ALUpdate( pAL->viewRgn, hAL );

	// redraw the selection outline, if hidden
	if (hideOutline) {
		BSET(pAL->features, alFOutlineHilite);
		_ALHiliteSelected(hAL);
	}

	// restore the port
	SetGWorld(savePort, saveDev);

	// unlock the AL record
	_ALSetHandleLock((Handle) hAL, saveALLock);
} // ALScrollPixels
