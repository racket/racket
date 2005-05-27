/*
 *	ALCellData.c
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

static Boolean local_ALStdSearchProc( ALData cellData, ALData searchData );

unsigned long _ALCalcOffsetFromCell(const ALCellPtr theCell, const LongRect *bounds)
{	return ((theCell->v - bounds->top) * (bounds->right - bounds->left) + (theCell->h - bounds->left));
} // _ALCalcOffsetFromCell

void _ALCalcCellFromOffset(ALCellPtr theCell, unsigned long offset, const LongRect *bounds)
{	unsigned long	temp;

	temp = bounds->right - bounds->left;
	// Check for bad rectangle.
	if (temp <= 0)
		temp = 1;

	theCell->v = offset / temp + bounds->top;
	theCell->h = offset % temp + bounds->left;
} // _ALCalcCellFromOffset

void _ALCalcVisibleCells(ALHandle hAL)
{	// This function calculates the number of cells visible, based on the cellSize and the dispRect.
	// If not all of the cells are visible, the visCells.top and visCells.left are the basis for the calculation.
	Rect			dispRect = (*hAL)->dispRect;
	Point			cellSize = (*hAL)->cellSize;
	LongRect		bounds = (*hAL)->dataBounds;
	long			numWanted, numCanShow, numShowing;
	Boolean		havePart;
#if ALIST_HEIRARCHICAL
	ALCell		tempCell;
	unsigned long	superRowOffset, offset;
#endif

	// Calculate how many rows we want to display and how much space we have.
	numWanted = bounds.bottom - bounds.top;

#if ALIST_HEIRARCHICAL
	if ( BTST( (*hAL)->features, alFHeirarchical ) ) {
		// Adjust the number of cells we want for any collapsed/hidden cells.
		tempCell.h = bounds.left;
		for ( tempCell.v = bounds.top; tempCell.v < bounds.bottom; tempCell.v++ ) {
			superRowOffset = _ALCalcOffsetFromCell( &tempCell, &bounds );
			if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
						!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) ) {
				for ( tempCell.v++; tempCell.v < bounds.bottom; tempCell.v++ ) {
					// Reduce the number of cells we want for each of the collapsed cells.
					offset = _ALCalcOffsetFromCell( &tempCell, &bounds );
					if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] )
						numWanted--;
					else {
						// Found the end of the collapsed rows.  Break out of the inner loop here.
						tempCell.v--;
						break;
					}
				} // for all collapsed/hidden rows
			} // found a collapsed row
		} // for all cells
	} // heirarchical
#endif

	// Calculate how many cells we can show.
	numCanShow = (dispRect.bottom - dispRect.top) / cellSize.v;
	havePart = ( ( (dispRect.bottom - dispRect.top) % cellSize.v ) != 0);
	if ( havePart )
		numCanShow++;

	if ( numWanted < numCanShow || ( numWanted == numCanShow && !havePart ) ) {
		// If we don't want too many, we have enough room to show them all.
		(*hAL)->visCells.top = bounds.top;
		(*hAL)->visCells.bottom = bounds.bottom;
	} else {
		// There are too many cells to show them all.
		// Leave visCells.top alone.
		(*hAL)->visCells.bottom = (*hAL)->visCells.top + numCanShow;

#if ALIST_HEIRARCHICAL
		if ( BTST( (*hAL)->features, alFHeirarchical ) ) {
			// Adjust the bottom of the visible cells for any collapsed/hidden cells below (*hAL)->visCells.top.
			tempCell.h = bounds.left;
			numShowing = 0;

			for ( tempCell.v = (*hAL)->visCells.top; tempCell.v < bounds.bottom &&
						( numShowing < numCanShow || ( numShowing == numCanShow && !havePart ) ); tempCell.v++ ) {
				superRowOffset = _ALCalcOffsetFromCell( &tempCell, &bounds );
				numShowing++;
				if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
							!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) ) {
					for ( tempCell.v++; tempCell.v < bounds.bottom; tempCell.v++ ) {
						// Increase the bottom of the visible cells for each of the collapsed cells.
						offset = _ALCalcOffsetFromCell( &tempCell, &bounds );
						if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] )
							(*hAL)->visCells.bottom++;
						else {
							// Found the end of the collapsed rows.  Break out of the inner loop here.
							tempCell.v--;
							break;
						}
					} // for all collapsed/hidden rows
				} // found a collapsed row
			} // for all cells
		} // heirarchical
#endif

		// Limit it to the data bounds.
		if ((*hAL)->visCells.bottom > bounds.bottom) {
			(*hAL)->visCells.bottom = bounds.bottom;

#if ALIST_HEIRARCHICAL
			// Need to check for collapsed super rows.
			if ( BTST( (*hAL)->features, alFHeirarchical ) ) {
				// Recalculate the top cell based on the bottom cell.
				(*hAL)->visCells.top = bounds.bottom + 1;
				numShowing = numCanShow;
				for ( tempCell.v = (*hAL)->visCells.bottom; tempCell.v > bounds.top && numShowing > 0; tempCell.v-- ) {
					if ( !ALIsRowHidden( tempCell.v, hAL ) )
						numShowing--;
					(*hAL)->visCells.top--;
				} // for all cells
			} // heirarchical
			else
#endif
			if ( !(numWanted == numCanShow && havePart) )
				// Adjusting the top is OK in this case.
				(*hAL)->visCells.top = bounds.bottom - numCanShow + 1;
		}
	} // there are too many cells to show them all.

	if ((*hAL)->vScroll != nil) {
		// For the scroll bar, allow the user to be able to scroll that last little bit.
		if ( havePart )
			numCanShow--;
		numWanted -= numCanShow;
		if (numWanted < 0)
			numWanted = 0;
		// Now reuse the numShowing variable to calculate the value to set the scroll bar at.
		numShowing = (*hAL)->visCells.top;
#if ALIST_HEIRARCHICAL
		if ( BTST( (*hAL)->features, alFHeirarchical ) ) {
			// We can have collapsed rows off the top of the list which will modify the scroll bar position.
			tempCell.v = bounds.top;		tempCell.h = bounds.left;
			for ( ; tempCell.v < (*hAL)->visCells.top; tempCell.v++ ) {
				superRowOffset = _ALCalcOffsetFromCell( &tempCell, &bounds );
				if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
							!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) ) {
					for ( tempCell.v++; tempCell.v < (*hAL)->visCells.top; tempCell.v++ ) {
						// Decrease the scroll bar value for each hidden row.
						offset = _ALCalcOffsetFromCell( &tempCell, &bounds );
						if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] )
							numShowing--;
						else {
							// Found the end of the collapsed rows.  Break out of the inner loop here.
							tempCell.v--;
							break;
						}
					} // for all collapsed/hidden rows
				} // found a collapsed row
			}
		}
#endif
		LCSetMaximum( (*hAL)->vScroll, numWanted );
		LCSetValue( (*hAL)->vScroll, numShowing );

#if ALIST_USECONTROLMGR2 && TARGET_RT_MAC_CFM
#if defined(UNIVERSAL_INTERFACES_VERSION) && (UNIVERSAL_INTERFACES_VERSION >= 0x0320)
		if ( BTST((*hAL)->flags, alFHasControlMgr2) )
			SetControlViewSize( (*hAL)->vScroll, numCanShow );
#endif
#endif
	}


	// Calculate the visible number of columns.
	numWanted = bounds.right - bounds.left;
	numCanShow = (dispRect.right - dispRect.left) / cellSize.h;
	if ((dispRect.right - dispRect.left) % cellSize.h != 0) {
		havePart = true;
		numCanShow++;
	} else
		havePart = false;

	// If we don't want too many, we have what we want!
	if (numWanted < numCanShow || (numWanted == numCanShow && !havePart)) {
		(*hAL)->visCells.left = bounds.left;
		(*hAL)->visCells.right = bounds.right;
	} else {
		// There are too many cells to show them all.
		(*hAL)->visCells.right = (*hAL)->visCells.left + numCanShow;

		// Limit it to the data bounds.
		if ((*hAL)->visCells.right > bounds.right) {
			(*hAL)->visCells.right = bounds.right;
			if (!(numWanted == numCanShow && havePart))
				(*hAL)->visCells.left = bounds.right - numCanShow + 1;
		}
	}

	if ((*hAL)->hScroll != nil) {
		if (havePart)
			numCanShow--;
		numWanted -= numCanShow;
		if (numWanted <  0)
			numWanted = 0;
		LCSetMaximum((*hAL)->hScroll, numWanted);
		LCSetValue((*hAL)->hScroll, (*hAL)->visCells.left);

#if ALIST_USECONTROLMGR2 && TARGET_RT_MAC_CFM
#if defined(UNIVERSAL_INTERFACES_VERSION) && (UNIVERSAL_INTERFACES_VERSION >= 0x0320)
		if ( BTST((*hAL)->flags, alFHasControlMgr2) )
			SetControlViewSize( (*hAL)->hScroll, numCanShow );
#endif
#endif
	}
} // _ALCalcVisibleCells

Boolean _ALCheckInsideBounds(const ALCellPtr theCell, const LongRect *bounds)
{	if (theCell->h >= bounds->left && theCell->h < bounds->right)
		if (theCell->v >= bounds->top && theCell->v < bounds->bottom)
			return true;

	return false;
} // _ALCheckInsideBounds

static Boolean local_ALStdSearchProc(ALData cellData, ALData searchData)
{	if (cellData == searchData)
		return true;
	else
		return false;
} // _ALStdSearchProc


#pragma mark -


ALIST_API OSErr ALClearCell(const ALCellPtr theCell, ALHandle hAL)
{
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
#endif

	// Sanity check that theCell is actually in the data set.
	if (hAL == nil || *hAL == nil || theCell == nil || !_ALCheckInsideBounds(theCell, &(*hAL)->dataBounds))
		return paramErr;

#if ALIST_HAVE_CELLDATA
	// Lock the data.
	HLock((Handle)(*hAL)->hData);
	pData = *(*hAL)->hData;

	pData[_ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds)] = nil;

	// Unlock the data.
	HUnlock((Handle)(*hAL)->hData);
#endif

	// Redraw this cell
	if (!BTST((*hAL)->features, alFInhibitRedraw))
		ALDrawCell(theCell, hAL);

	return noErr;
} // ALClearCell

ALIST_API OSErr ALGetCell(ALData *dataPtr, const ALCellPtr theCell, ALHandle hAL)
{
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
	Boolean		saveDataLock;

	// Sanity check that theCell is actually in the data set.
	if (hAL == nil || *hAL == nil || theCell == nil || !_ALCheckInsideBounds(theCell, &(*hAL)->dataBounds))
		return paramErr;

	// Lock the data.
	saveDataLock = _ALSetHandleLock((Handle)(*hAL)->hData, true);
	pData = *(*hAL)->hData;

	// Return the data (as a Ptr).
	*dataPtr = pData[_ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds)];

	// Reset the data lock.
	_ALSetHandleLock((Handle)(*hAL)->hData, saveDataLock);

	return noErr;
#else
	// We don't have any data, so can't get the cell.
	#pragma unused( dataPtr, theCell, hAL )

	return alNoDataErr;
#endif
} // ALGetCell

ALIST_API void ALGetCellRect(Rect *cellRect, const ALCellPtr theCell, ALHandle hAL)
{	// Sanity check!
	if (hAL == nil || *hAL == nil || theCell == nil || cellRect == nil)
		return;

	_ALCalcCellRect( cellRect, theCell, false, hAL );
} // ALGetCellRect

ALIST_API void ALSetCellSize(Point cSize, ALHandle hAL)
{	// Sanity check!
	if (hAL == nil || *hAL == nil || cSize.h == 0 || cSize.v == 0)
		return;

	(*hAL)->cellSize = cSize;

	// Calculate visible cells.
	_ALCalcVisibleCells(hAL);

	// Redraw the entire list.
	if (!BTST((*hAL)->features, alFInhibitRedraw))
		ALUpdate(nil, hAL);
}

ALIST_API void ALGetCellSize(Point *cSize, ALHandle hAL)
{	// Sanity check!
	if ( hAL == nil || *hAL == nil || cSize == nil )
		return;

	*cSize = (*hAL)->cellSize;
}

ALIST_API Boolean ALNextCell(Boolean hNext, Boolean vNext, ALCell *theCell, ALHandle hAL)
{	LongRect	bounds;
	Boolean	result = false;

	// Sanity check!
	if (hAL == nil || *hAL == nil || theCell == nil)
		return result;

	bounds = (*hAL)->dataBounds;

	// Look horizontally only.
	if (hNext && !vNext && theCell->h < bounds.right && theCell->h >= bounds.left - 1) {
		theCell->h++;
		result = true;
	}
	// Look vertically only.
	else if (!hNext && vNext && theCell->v < bounds.bottom && theCell->v >= bounds.top - 1) {
		theCell->v++;
		result = true;
	}
	// Look vertically and horizontally.
	else if (hNext && vNext && (theCell->v < bounds.bottom && theCell->h < bounds.right) &&
			(theCell->v >= bounds.top - 1 && theCell->h >= bounds.left - 1)) {
		theCell->v++;
		theCell->h++;
		result = true;
	}

	return result;
} // ALNextCell

ALIST_API OSErr ALSetCell(const ALData data, const ALCellPtr theCell, ALHandle hAL)
{
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
#else
	#pragma unused( data )
#endif

	// Sanity check that theCell is actually in the data set.
	if (hAL == nil || *hAL == nil || theCell == nil || !_ALCheckInsideBounds(theCell, &(*hAL)->dataBounds))
		return paramErr;

#if ALIST_HAVE_CELLDATA
	// Lock the data.
	HLock((Handle)(*hAL)->hData);
	pData = *(*hAL)->hData;

	// Set the cells data.
	pData[_ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds)] = data;

	// Unlock the data.
	HUnlock((Handle)(*hAL)->hData);
#endif

	// Redraw this cell, even if we have no data.
	if (!BTST((*hAL)->features, alFInhibitRedraw))
		ALDrawCell(theCell, hAL);

#if !ALIST_HAVE_CELLDATA
	return alNoDataErr;
#endif

	return noErr;
} // ALSetCell

ALIST_API Boolean ALSearch(const ALData searchData, ALSearchUPP searchProc, ALCell *ioCell, ALHandle hAL)
{
#if ALIST_HAVE_CELLDATA
	ALDataPtr				pData;
	ALSearchUPP			stdSearchUPP = nil;
	Boolean				found = false;
	register unsigned long	i, length;

	// Sanity check that theCell is actually in the data set.
	if (hAL == nil || *hAL == nil || ioCell == nil || !_ALCheckInsideBounds(ioCell, &(*hAL)->dataBounds))
		return false;

	length = (*hAL)->dataLength;

	// Use the standard search procedure if none is given.
	if (searchProc == nil) {
		stdSearchUPP = NewALSearchProc( local_ALStdSearchProc );
		searchProc = stdSearchUPP;
	}

	// Lock the data.
	HLock((Handle)(*hAL)->hData);
	pData = *(*hAL)->hData;

	// Search for the given data!
	for (i = _ALCalcOffsetFromCell(ioCell, &(*hAL)->dataBounds); i < length && !found; i++)
		found = CallALSearchProc(pData[i], searchData, searchProc);

	// If it was found, determine which cell it is in.
	// Note that the variable i is one greater than the cell, since the for loop increments at the end.
	if ( found )
		_ALCalcCellFromOffset(ioCell, i - 1, &(*hAL)->dataBounds);

	// Unlock the data.
	HUnlock((Handle)(*hAL)->hData);

#if ALIST_USE_UPPS
	// Dispose of the standard routine descriptor if necessary.
	if ( stdSearchUPP != nil )
		DisposeRoutineDescriptor(stdSearchUPP);
#endif

	return found;
#else
	// Can't find cell if there is no data.
	#pragma unused( searchData, searchProc, ioCell, hAL )

	return false;
#endif
}

#pragma mark -

ALIST_API long ALAddColumn(long count, long afterColNum, ALHandle hAL)
{	ALPtr		pAL;
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
#endif
	ALSelectionPtr	pSelected;
#if ALIST_HEIRARCHICAL
	ALRowLevelPtr	pLevels;
#endif
	ALCell		theCell;
	Boolean		saveALLock;
	long			moreMem, i;
	long			dataOffset, amountToCopy;
	long			colNumAdded = alAddedNone;

	// Sanity check.
	if (hAL == nil || count <= 0 || afterColNum > (*hAL)->dataBounds.right || afterColNum < (*hAL)->dataBounds.left)
		return colNumAdded;

	// Lock the A List handle.
	saveALLock = _ALSetHandleLock((Handle)hAL, true);
	pAL = *hAL;

	// Calculate how much memory to add.
	moreMem = (pAL->dataBounds.bottom - pAL->dataBounds.top) * count;

#if ALIST_HAVE_CELLDATA
	// Reset the size of the data handle.
	SetHandleSize((Handle)pAL->hData, (pAL->dataLength + moreMem) * sizeof(ALData));
#endif

	// Reset the size of the selection handle.
	SetHandleSize((Handle)pAL->hSelected, (pAL->dataLength + moreMem) * sizeof(ALSelection));

#if ALIST_HEIRARCHICAL
	// Reset the size of the selection handle.
	SetHandleSize((Handle)pAL->hLevels, (pAL->dataLength + moreMem) * sizeof(ALRowLevel));
#endif

	if (MemError() == noErr) {
		// Lock the data handle.
#if ALIST_HAVE_CELLDATA
		_ALSetHandleLock((Handle)pAL->hData, true);
		pData = *pAL->hData;
#endif
		_ALSetHandleLock((Handle)pAL->hSelected, true);
		pSelected = *pAL->hSelected;
#if ALIST_HEIRARCHICAL
		_ALSetHandleLock((Handle)pAL->hLevels, true);
		pLevels = *pAL->hLevels;
#endif

		// Move through the data (starting at the end) and shift it up
		for (i = pAL->dataBounds.bottom - 1; i >= 0; i--) {
			// Calculate where to insert the new data and how much to copy.
			dataOffset = i * (pAL->dataBounds.right - pAL->dataBounds.left) + afterColNum;
			amountToCopy = pAL->dataLength - dataOffset;

			if (amountToCopy != 0) {
#if ALIST_HAVE_CELLDATA
				BlockMoveData(&pData[dataOffset], &pData[dataOffset + count], amountToCopy * sizeof(ALData));
#endif
				BlockMoveData(&pSelected[dataOffset], &pSelected[dataOffset + count], amountToCopy * sizeof(ALSelection));
			}

#if ALIST_HAVE_CELLDATA
			_ALBlockClr(&pData[dataOffset], count * sizeof(ALData));
#endif
			_ALBlockClr(&pSelected[dataOffset], count * sizeof(ALSelection));

			pAL->dataLength += count;
		}

#if ALIST_HAVE_CELLDATA
		_ALSetHandleLock((Handle)pAL->hData, false);
#endif
		_ALSetHandleLock((Handle)pAL->hSelected, false);
#if ALIST_HEIRARCHICAL
		_ALSetHandleLock((Handle)pAL->hLevels, false);
#endif

		colNumAdded = afterColNum;
	}

	// If we added anything, do some more calculations and possibly redraw.
	if (colNumAdded != alAddedNone) {
		// Calculate the new size of the list.
		pAL->dataBounds.right += count;

		// Calculate visible cells.
		_ALCalcVisibleCells(hAL);

		// Redraw since some columns were added.
		if (!BTST((*hAL)->features, alFInhibitRedraw)) {
			if (colNumAdded < pAL->visCells.right && (colNumAdded + count) >= pAL->visCells.left) {
				for (theCell.h = colNumAdded; theCell.h < pAL->visCells.right; theCell.h++)
					for (theCell.v = pAL->visCells.top; theCell.v < pAL->visCells.bottom; theCell.v++)
						ALDrawCell(&theCell, hAL);
			} // end if
		} // end OK to redraw
	} // end some columnss added.

cleanup:
	_ALSetHandleLock((Handle)hAL, saveALLock);

	return colNumAdded;
}

ALIST_API void ALDelColumn(long count, long startColNum, ALHandle hAL)
{	// If count == 0, all columns are deleted.
	ALPtr		pAL;
	Boolean		saveLock;
	long			moreMem, i;
	long			dataOffset, amountToCopy;
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
#endif
	ALSelectionPtr	pSelected;
#if ALIST_HEIRARCHICAL
	ALRowLevelPtr	pLevels;
#endif

	// Sanity check. Check for nilS, trying to start deleting a column that doesn't exist, and if there are no rows.
	if (hAL == nil || count < 0 || startColNum > (*hAL)->dataBounds.right || startColNum < (*hAL)->dataBounds.left ||
			(*hAL)->dataBounds.bottom == (*hAL)->dataBounds.top)
		return;

	if (count == 0) {
		// Delete ALL the columns.
		count = ALGetNumberColumns(hAL);
		startColNum = (*hAL)->dataBounds.left;
	}

	if ((*hAL)->disposeCellDataHook != nil) {
		// Call the DisposeCellData hook for every cell being disposed of.
		ALCell	cell;
		ALData	cellData;
		long		endColNum;

		endColNum = count + startColNum;
		if (endColNum > (*hAL)->dataBounds.right)
			endColNum = (*hAL)->dataBounds.right;

		for (cell.v = (*hAL)->dataBounds.top; cell.v < (*hAL)->dataBounds.bottom; cell.v++)
			for (cell.h = startColNum; cell.h < endColNum; cell.h++) {
				// Dispose of the data here.
				if ( ALGetCell(&cellData, &cell, hAL) == noErr )
					CallALDisposeCellDataProc(cellData, &cell, hAL, (*hAL)->disposeCellDataHook);
			}
	}

	// Lock the A List handle.
	saveLock = _ALSetHandleLock((Handle)hAL, true);
	pAL = *hAL;

	if (startColNum == pAL->dataBounds.left && count >= (pAL->dataBounds.right - pAL->dataBounds.left)) {
		// Get rid of everything!
#if ALIST_HAVE_CELLDATA
		SetHandleSize((Handle)pAL->hData, 0);
#endif
		SetHandleSize((Handle)pAL->hSelected, 0);
#if ALIST_HEIRARCHICAL
		SetHandleSize((Handle)pAL->hLevels, 0);
#endif
	} else {
		// Need to remove some of the data, but not all.
#if ALIST_HAVE_CELLDATA
		_ALSetHandleLock((Handle)pAL->hData, true);
		pData = *pAL->hData;
#endif
		_ALSetHandleLock((Handle)pAL->hSelected, true);
		pSelected = *pAL->hSelected;
#if ALIST_HEIRARCHICAL
		_ALSetHandleLock((Handle)pAL->hLevels, true);
		pLevels = *pAL->hLevels;
#endif

		// Move through the data (starting at the end) and shift it down
		for (i = pAL->dataBounds.bottom - 1; i >= 0; i--) {
			// Calculate where to insert the new data and how much to copy.
			dataOffset = i * (pAL->dataBounds.right - pAL->dataBounds.left) + startColNum;
			amountToCopy = pAL->dataLength - dataOffset - count;

			if (amountToCopy != 0) {
#if ALIST_HAVE_CELLDATA
				BlockMoveData(&pData[dataOffset + count], &pData[dataOffset], amountToCopy * sizeof(ALData));
#endif
				BlockMoveData(&pSelected[dataOffset + count], &pSelected[dataOffset], amountToCopy * sizeof(ALSelection));
			}

			pAL->dataLength -= count;
		}

#if ALIST_HAVE_CELLDATA
		_ALSetHandleLock((Handle)pAL->hData, false);
#endif
		_ALSetHandleLock((Handle)pAL->hSelected, false);
#if ALIST_HEIRARCHICAL
		_ALSetHandleLock((Handle)pAL->hLevels, false);
#endif

#if ALIST_HAVE_CELLDATA
		SetHandleSize((Handle)pAL->hData, pAL->dataLength * sizeof(ALData));
#endif
		SetHandleSize((Handle)pAL->hSelected, pAL->dataLength * sizeof(ALSelection));
#if ALIST_HEIRARCHICAL
		SetHandleSize((Handle)pAL->hLevels, pAL->dataLength * sizeof(ALRowLevel));
#endif
	}

	// Calculate the new size of the list.
	moreMem = pAL->dataBounds.bottom - pAL->dataBounds.top;
	pAL->dataBounds.right = (pAL->dataLength / moreMem) + pAL->dataBounds.left;

	_ALSetHandleLock((Handle)hAL, saveLock);

	_ALCalcVisibleCells(hAL);

	// Redraw since some rows were deleted.
	if (!BTST((*hAL)->features, alFInhibitRedraw))
		// Could be done better.
		ALUpdate(nil, hAL);

	return;
}

ALIST_API long ALAddRow(long count, long afterRowNum, ALHandle hAL)
{	ALPtr		pAL;
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
#endif
	ALSelectionPtr	pSelected;
#if ALIST_HEIRARCHICAL
	ALRowLevelPtr	pLevels;
#endif
	ALCell		theCell;
	Boolean		saveALLock;
	long			moreMem;
	long			dataOffset;
	long			rowNumAdded = alAddedNone;

	// Sanity check.
	if (hAL == nil || count <= 0 || afterRowNum > (*hAL)->dataBounds.bottom || afterRowNum < (*hAL)->dataBounds.top ||
		(*hAL)->dataBounds.right == (*hAL)->dataBounds.left)
		return rowNumAdded;

	// Lock the A List handle.
	saveALLock = _ALSetHandleLock((Handle)hAL, true);
	pAL = *hAL;

	// Calculate how much memory to add, and where to put it.
	// Do preliminary calc.
	moreMem = pAL->dataBounds.right - pAL->dataBounds.left;
	// Calc actual numbers.
	dataOffset = moreMem * afterRowNum;
	moreMem *= count;

#if ALIST_HAVE_CELLDATA
	// Reset the size of the data handle.
	SetHandleSize((Handle)pAL->hData, (pAL->dataLength + moreMem) * sizeof(ALData));
#endif

	// Reset the size of the selection handle.
	SetHandleSize((Handle)pAL->hSelected, (pAL->dataLength + moreMem) * sizeof(ALSelection));

#if ALIST_HEIRARCHICAL
	// Reset the size of the selection handle.
	SetHandleSize((Handle)pAL->hLevels, (pAL->dataLength + moreMem) * sizeof(ALRowLevel));
#endif

	if (MemError() == noErr) {
#if ALIST_HAVE_CELLDATA
		// Lock the data handle.
		HLock((Handle)pAL->hData);
		pData = *pAL->hData;
		// Move the other rows down, and clear the added rows.
		if (dataOffset != pAL->dataLength)
			BlockMoveData(&pData[dataOffset], &pData[dataOffset + moreMem], (pAL->dataLength - dataOffset) * sizeof(ALData));
		_ALBlockClr(&pData[dataOffset], moreMem * sizeof(ALData));

		HUnlock((Handle)pAL->hData);
#endif

		HLock((Handle)pAL->hSelected);
		pSelected = *pAL->hSelected;
		if (dataOffset != pAL->dataLength)
			BlockMoveData(&pSelected[dataOffset], &pSelected[dataOffset + moreMem], (pAL->dataLength - dataOffset) * sizeof(ALSelection));
		_ALBlockClr(&pSelected[dataOffset], moreMem * sizeof(ALSelection));
		HUnlock((Handle)pAL->hSelected);

#if ALIST_HEIRARCHICAL
		HLock((Handle)pAL->hLevels);
		pLevels = *pAL->hLevels;
		if (dataOffset != pAL->dataLength)
			BlockMoveData(&pLevels[dataOffset], &pLevels[dataOffset + moreMem], (pAL->dataLength - dataOffset) * sizeof(ALRowLevel));
		_ALBlockClr(&pLevels[dataOffset], moreMem * sizeof(ALRowLevel));
		HUnlock((Handle)pAL->hLevels);
#endif

		rowNumAdded = afterRowNum;
	}

	// If we added anything, do some more calculations and possibly redraw.
	if (rowNumAdded != alAddedNone) {
		// Calculate the new size of the list.
		moreMem = pAL->dataBounds.right - pAL->dataBounds.left;
		pAL->dataLength = GetHandleSize((Handle)pAL->hSelected) / sizeof(ALSelection);
		pAL->dataBounds.bottom = (pAL->dataLength / moreMem) + pAL->dataBounds.top;

		// Calculate visible cells.
		_ALCalcVisibleCells( hAL );

		// Redraw since some rows were added.
		if (!BTST((*hAL)->features, alFInhibitRedraw)) {
			if (rowNumAdded < pAL->visCells.bottom && (rowNumAdded + count) >= pAL->visCells.top) {
				for (theCell.h = pAL->visCells.left; theCell.h < pAL->visCells.right; theCell.h++)
					for (theCell.v = rowNumAdded; theCell.v < pAL->visCells.bottom; theCell.v++)
						ALDrawCell(&theCell, hAL);
			} // end if
		} // end OK to redraw
	} // end some rows added.

cleanup:
	_ALSetHandleLock((Handle)hAL, saveALLock);

	return rowNumAdded;
}

ALIST_API void ALDelRow(long count, long startRowNum, ALHandle hAL)
{	// If count == 0, all rows are deleted.
	ALPtr		pAL;
#if ALIST_HAVE_CELLDATA
	ALDataPtr		pData;
#endif
	ALSelectionPtr	sPtr;
#if ALIST_HEIRARCHICAL
	ALRowLevelPtr	pLevels;
#endif
	long			moreMem, dataOffset;
	Boolean		saveLock;

	// Sanity check.  Check for nils, trying to start deleting a row that doesn't exist, and if there are no columns.
	if (hAL == nil || count < 0 || startRowNum > (*hAL)->dataBounds.bottom || startRowNum < (*hAL)->dataBounds.top ||
			(*hAL)->dataBounds.right == (*hAL)->dataBounds.left)
		return;

	if ( count == 0 ) {
		// Delete ALL the rows.
		count = ALGetNumberRows(hAL);
		startRowNum = (*hAL)->dataBounds.top;
	}

	if ((*hAL)->disposeCellDataHook != nil) {
		ALCell	cell;
		ALData	cellData;
		long		endRowNum;

		endRowNum = count + startRowNum;
		if (endRowNum > (*hAL)->dataBounds.bottom)
			endRowNum = (*hAL)->dataBounds.bottom;

		for (cell.h = (*hAL)->dataBounds.left; cell.h < (*hAL)->dataBounds.right; cell.h++)
			for (cell.v = startRowNum; cell.v < endRowNum; cell.v++) {
				// Dispose of the data here.
				if ( ALGetCell(&cellData, &cell, hAL) == noErr )
					CallALDisposeCellDataProc(cellData, &cell, hAL, (*hAL)->disposeCellDataHook);
			}
	}

	// Lock the A List handle.
	saveLock = _ALSetHandleLock((Handle)hAL, true);
	pAL = *hAL;

	// Calculate how much memory to remove, and where to take it from.
	// Do preliminary calc.
	moreMem = pAL->dataBounds.right - pAL->dataBounds.left;
	// Calc actual numbers.
	dataOffset = moreMem * startRowNum;
	moreMem *= count;

	if (moreMem >= pAL->dataLength && startRowNum == pAL->dataBounds.top) {
		// Get rid of the whole thing!
#if ALIST_HAVE_CELLDATA
		SetHandleSize((Handle)pAL->hData, 0);
#endif
		SetHandleSize((Handle)pAL->hSelected, 0);
#if ALIST_HEIRARCHICAL
		SetHandleSize((Handle)pAL->hLevels, 0);
#endif
	} else if (moreMem > pAL->dataLength) {
		// Reset the size of the data handles.
#if ALIST_HAVE_CELLDATA
		SetHandleSize((Handle)pAL->hData, (dataOffset - 1) * sizeof(ALData));
#endif
		SetHandleSize((Handle)pAL->hSelected, (dataOffset - 1) * sizeof(ALSelection));
#if ALIST_HEIRARCHICAL
		SetHandleSize((Handle)pAL->hLevels, (dataOffset - 1) * sizeof(ALRowLevel));
#endif
	} else {
#if ALIST_HAVE_CELLDATA
		// Lock the data handle.
		HLock((Handle)pAL->hData);
		pData = *pAL->hData;
		// Move the other rows up.
		BlockMoveData(&pData[dataOffset + moreMem], &pData[dataOffset],
						(pAL->dataLength - moreMem - dataOffset) * sizeof(ALData));
		HUnlock((Handle)pAL->hData);
		// Reset the size of the data handle.
		SetHandleSize((Handle)pAL->hData, (pAL->dataLength - moreMem) * sizeof(ALData));
#endif

		// Do the same stuff for the selection handle.
		HLock((Handle)pAL->hSelected);
		sPtr = *pAL->hSelected;
		BlockMoveData(&sPtr[dataOffset + moreMem], &sPtr[dataOffset],
						(pAL->dataLength - moreMem - dataOffset) * sizeof(ALSelection));
		HUnlock((Handle)pAL->hSelected);
		SetHandleSize((Handle)pAL->hSelected, (pAL->dataLength - moreMem) * sizeof(ALSelection));

#if ALIST_HEIRARCHICAL
		// Do the same stuff for the levels handle.
		HLock((Handle)pAL->hLevels);
		pLevels = *pAL->hLevels;
		BlockMoveData(&pLevels[dataOffset + moreMem], &pLevels[dataOffset],
						(pAL->dataLength - moreMem - dataOffset) * sizeof(ALRowLevel));
		HUnlock((Handle)pAL->hLevels);
		SetHandleSize((Handle)pAL->hLevels, (pAL->dataLength - moreMem) * sizeof(ALRowLevel));
#endif
	}

	// Calculate the new size of the list.
	moreMem = pAL->dataBounds.right - pAL->dataBounds.left;
	pAL->dataLength = GetHandleSize((Handle)pAL->hSelected) / sizeof(ALSelection);
	pAL->dataBounds.bottom = (pAL->dataLength / moreMem) + pAL->dataBounds.top;

	_ALSetHandleLock((Handle)hAL, saveLock);

	// Redraw since some rows were deleted.
	_ALCalcVisibleCells( hAL );

	if ( !BTST( (*hAL)->features, alFInhibitRedraw ) )
		// Could be done better.
		ALUpdate( nil, hAL );

	return;
}

ALIST_API long ALGetNumberRows(ALHandle hAL)
{	if (hAL == nil || *hAL == nil)
		return 0;

	return (*hAL)->dataBounds.bottom - (*hAL)->dataBounds.top;
}

ALIST_API long ALGetNumberColumns(ALHandle hAL)
{	if (hAL == nil || *hAL == nil)
		return 0;

	return (*hAL)->dataBounds.right - (*hAL)->dataBounds.left;
}

ALIST_API Boolean ALIsVisible(const ALCellPtr theCell, ALHandle hAL)
{	if (hAL == nil || *hAL == nil || theCell == nil)
		return false;

	if ( _ALCheckInsideBounds(theCell, &(*hAL)->visCells) ) {
#if ALIST_HEIRARCHICAL
		if ( ALIsRowHidden( theCell->v, hAL ) )
			// If the row is hidden, this cell is not visible.
			return false;
#endif

		return true;
	} else
		return false;
}
