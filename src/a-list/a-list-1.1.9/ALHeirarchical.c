/*
 *	ALHeirarchical.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#include "AListInternal.h"

#if ALIST_HEIRARCHICAL

ALIST_API OSErr ALExpandRow(long rowNum, Boolean expandChildren, ALHandle hAL)
{	ALCell		tempCell;
	unsigned long	superRowOffset, offset;

	// Sanity check.
	if (hAL == nil || *hAL == nil || rowNum < (**hAL).dataBounds.top || rowNum >= (**hAL).dataBounds.bottom)
		return paramErr;

	// Make sure the list is heirarchical.
	if ( !BTST( (*hAL)->features, alFHeirarchical ) )
		return false;

	// The left-most cell contains the heirarchical data.
	tempCell.h = (**hAL).dataBounds.left;
	tempCell.v = rowNum;

	superRowOffset = _ALCalcOffsetFromCell(&tempCell, &(*hAL)->dataBounds);

	// Mark the row as expanded.
	BSET((*(*hAL)->hSelected)[superRowOffset], alSexpanded);

	// Check if it's a heirarchical row.
	if ( BTST((*(*hAL)->hSelected)[superRowOffset], alSsuperrow) ) {
		if ( expandChildren ) {
			// Mark all children as expanded.
			for ( tempCell.v++; tempCell.v < (*hAL)->dataBounds.bottom; tempCell.v++ ) {
				offset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );
				if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] )
					BSET( (*(*hAL)->hSelected)[ offset ], alSexpanded );
				else
					break;
			}
		}

		// Recalculate the visible cells.
		_ALCalcVisibleCells( hAL );

		// Redraw the entire list.
		if ( !BTST((*hAL)->features, alFInhibitRedraw) )
			ALUpdate( nil, hAL );
	}

	return noErr;
}

ALIST_API OSErr ALCollapseRow(long rowNum, Boolean collapseChildren, ALHandle hAL)
{	ALCell		tempCell;
	unsigned long	superRowOffset, offset;

	// Sanity check.
	if ( hAL == nil || *hAL == nil || rowNum < (**hAL).dataBounds.top || rowNum >= (**hAL).dataBounds.bottom )
		return paramErr;

	// Make sure the list is heirarchical.
	if ( !BTST( (*hAL)->features, alFHeirarchical ) )
		return false;

	// The left-most cell contains the heirarchical data.
	tempCell.h = (**hAL).dataBounds.left;
	tempCell.v = rowNum;

	superRowOffset = _ALCalcOffsetFromCell(&tempCell, &(*hAL)->dataBounds);

	// Mark the row as collapsed.
	BCLR((*(*hAL)->hSelected)[superRowOffset], alSexpanded);

	// See if it's a heirarchical row.
	if ( BTST((*(*hAL)->hSelected)[superRowOffset], alSsuperrow) ) {
		for ( tempCell.v++; tempCell.v < (*hAL)->dataBounds.bottom; tempCell.v++ ) {
			// Go through looking for children.
			offset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );
			if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] ) {
				if ( collapseChildren )
					// Mark all children as collapsed.
					BCLR( (*(*hAL)->hSelected)[ offset ], alSexpanded );
				// Also, clear all selection bits.
				BCLR( (*(*hAL)->hSelected)[ offset ], alSselected );
			} else
				break;
		}

		// Recalculate the visible cells.
		_ALCalcVisibleCells( hAL );

		// Redraw the entire list.
		if ( !BTST((*hAL)->features, alFInhibitRedraw) )
			ALUpdate( nil, hAL );
	}

	return noErr;
}

ALIST_API Boolean ALIsRowExpanded( long inSuperRowNum, ALHandle hAL )
{	ALCell		tempCell;
	unsigned long	offset;

	// Sanity check.
	if (hAL == nil || *hAL == nil || inSuperRowNum < (**hAL).dataBounds.top || inSuperRowNum >= (**hAL).dataBounds.bottom)
		return false;

	// Make sure the list is heirarchical.
	if ( !BTST( (*hAL)->features, alFHeirarchical ) )
		return false;

	// The left-most cell contains the heirarchical data.
	tempCell.h = (**hAL).dataBounds.left;
	tempCell.v = inSuperRowNum;

	offset = _ALCalcOffsetFromCell(&tempCell, &(*hAL)->dataBounds);

	return (BTST((*(*hAL)->hSelected)[offset], alSexpanded) != 0);
}

ALIST_API Boolean ALIsRowHidden( long inAnyRowNum, ALHandle hAL )
{	ALCell		tempCell;
	unsigned long	superRowOffset;

	// Sanity check.
	if ( hAL == nil || *hAL == nil || inAnyRowNum < (**hAL).dataBounds.top || inAnyRowNum >= (**hAL).dataBounds.bottom )
		return false;

	// Make sure the list is heirarchical.
	if ( !BTST( (*hAL)->features, alFHeirarchical ) )
		return false;

	// The left-most cell contains the heirarchical data.
	tempCell.h = (**hAL).dataBounds.left;
	tempCell.v = inAnyRowNum;

	while ( ( tempCell.v = ALSuperRow( tempCell.v, hAL ) ) != -1 ) {
		// tempCell now contains the cell that is the superrow of the previous tempCell.
		superRowOffset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );

		if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
					!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) )
			// The superrow is not expanded, so we're hidden.
			return true;
	} // end of "found a superrow" loop

	// We're not hidden.
	return false;
}

ALIST_API long ALSuperRow( long subRow, ALHandle hAL )
{	ALCell		tempCell;
	unsigned long	subRowOffset, offset;

	if ( hAL == nil || *hAL == nil || subRow < (*hAL)->dataBounds.top || subRow >= (*hAL)->dataBounds.bottom )
		return -1;

	// Make sure the list is heirarchical.
	if ( !BTST( (*hAL)->features, alFHeirarchical ) )
		return -1;

	tempCell.h = (*hAL)->dataBounds.left;
	tempCell.v = subRow;
	subRowOffset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );

	if ( (*(*hAL)->hLevels)[ subRowOffset ] == 0 )
		// We're at the top level already.
		return -1;

	// Go up the list looking for the nearest level change.
	for ( tempCell.v--; tempCell.v >= (*hAL)->dataBounds.top; tempCell.v-- ) {
		offset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );
		if ( (*(*hAL)->hLevels)[ offset ] < (*(*hAL)->hLevels)[ subRowOffset ] )
			return tempCell.v;
	}

	// Didn't find the super row?  Shouldn't ever happen.
	return -1;
}

ALIST_API long ALAddRowUnder( long count, long superRow, ALHandle hAL )
{	long			result;
	ALCell		tempCell;
	unsigned long	superRowOffset, offset;
	short		saveRedrawState;

	if ( count < 0 || hAL == nil || *hAL == nil || superRow < (*hAL)->dataBounds.top || superRow >= (*hAL)->dataBounds.bottom )
		return 0;

	// Make sure the list is heirarchical.
	if ( !BTST( (*hAL)->features, alFHeirarchical ) )
		return 0;

	saveRedrawState = ALFeatureFlag( alFInhibitRedraw, alBitSet, hAL );

	result = ALAddRow( count, superRow + 1, hAL );

	ALFeatureFlag( alFInhibitRedraw, saveRedrawState, hAL );

	if ( result > 0 ) {
		tempCell.h = (**hAL).dataBounds.left;
		tempCell.v = superRow;
		superRowOffset = _ALCalcOffsetFromCell( &tempCell, &(*hAL)->dataBounds );

		// Mark the superRow as an alSsuperrow, if it wasn't already.
		if ( !BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) ) {
			BSET( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow );

			// The row defaults to being expanded.
			BSET( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded );
		}

		// Set the level of all the added rows to one greater than the superRows level.
		for ( tempCell.v = superRow + 1; tempCell.v <= superRow + count; tempCell.v++ ) {
			offset = _ALCalcOffsetFromCell(&tempCell, &(*hAL)->dataBounds);
			(*(*hAL)->hLevels)[ offset ] = (*(*hAL)->hLevels)[ superRowOffset ] + 1;
		}

		// Redraw the entire list.
		if ( !BTST((*hAL)->features, alFInhibitRedraw) )
			ALUpdate( nil, hAL );
	}

	return result;
}

#endif