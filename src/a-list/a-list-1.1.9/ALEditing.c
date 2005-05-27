/*
 *	ALEditing.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#ifndef __SCRAP__
	#include <Scrap.h>
#endif

#include "AListInternal.h"

static OSErr	local_ALAddDataToGroup(const ALDataDescriptor *flavorDesc, Handle *groupData);
static long		local_ALGetDataFromGroup(Handle dataHandle, OSType flavorType, Handle groupData, long *marker);

static OSErr local_ALAddDataToGroup(const ALDataDescriptor *flavorDesc, Handle *groupData)
{	OSErr	err = noErr;
	long		marker;

	// Sanity check on parameters.
	if (groupData == nil || flavorDesc == nil)
		return paramErr;

	if (*groupData == nil)
		err = _ALAllocate(0, 0, groupData);

	if (err == noErr) {
		marker = GetHandleSize(*groupData);

		// Make room for the type and size fields.
		SetHandleSize(*groupData, marker + 2 * sizeof(long));
		err = MemError();
	}

	if (err == noErr) {
		*(long *)&((*(*groupData))[marker]) = flavorDesc->descriptorType;
		marker += sizeof(long);

		// Make room for the data itself, if there is any.
		if (flavorDesc->dataHandle != nil) {
			*(long *)&((*(*groupData))[marker]) = GetHandleSize(flavorDesc->dataHandle);
			SetHandleSize(*groupData, GetHandleSize(*groupData) + GetHandleSize(flavorDesc->dataHandle));
			err = MemError();

			if (err == noErr) {
				// Copy the data.
				marker += sizeof(long);
				BlockMoveData(*flavorDesc->dataHandle, &(*(*groupData))[marker], GetHandleSize(flavorDesc->dataHandle));
			}
		} else
			*(long *)&((*(*groupData))[marker]) = 0;
	}

	return err;
}

static long local_ALGetDataFromGroup(Handle dataHandle, OSType flavorType, Handle groupData, long *marker)
{	long		dataSize;
	OSErr	err;

	if ( dataHandle == nil || groupData == nil || marker == nil )
		return 0;

	// Check to see if we've got the right flavor of data.
	if ( *(long *)&((*groupData)[*marker]) != flavorType )
		return 0;

	// Set the dataHandle's size to match the data from the group.
	*marker += sizeof(long);
	dataSize = *(long *)&((*groupData)[*marker]);
	SetHandleSize(dataHandle, dataSize);
	err = MemError();

	if (err == noErr) {
		*marker += sizeof(long);
		BlockMoveData(&((*groupData)[ *marker ]), *dataHandle, dataSize);
		*marker += dataSize;

		return dataSize;
	} else
		return 0;
}

#pragma mark -

// Undo is not supported yet.
ALIST_API Boolean	ALCanUndo(Boolean *isRedo, ALHandle hAL)
{
#pragma unused( hAL )
	if (isRedo != nil)
		*isRedo = false;

	return false;
}

ALIST_API void		ALUndo(ALHandle hAL)
{
#pragma unused( hAL )
}

ALIST_API void		ALCut(ALHandle hAL)
{	// First copy...
	ALCopy(hAL);

	// Then delete.
	ALDelete(hAL);
}

ALIST_API void		ALCopy(ALHandle hAL)
{	Boolean			more, first;
	ALDataDescriptor	flavorDesc = {typeNull, nil};
	LongRect			theBounds;
	ALCell			cell;
	ALData			cellData;
	int				index;
	OSErr			err;
	Handle			groupData;
#if defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 )
	ScrapRef			scrapRef;
#endif

	if (hAL == nil || *hAL == nil || (*hAL)->outputFlavorsHook == nil)
		return;

	// Do some initialization.
	first = true;
	groupData = nil;
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
	err = ZeroScrap();
#else
	ClearCurrentScrap( );
	err = GetCurrentScrap( &scrapRef );
#endif

	theBounds = (*hAL)->dataBounds;
	// Check every cell...copy the selected cells to the Scrap.
	for (cell.h = theBounds.left; cell.h < theBounds.right; cell.h++)
		for (cell.v = theBounds.top; cell.v < theBounds.bottom; cell.v++)
			if (_ALCellIsSelected(&cell, hAL)) {
				// The cell is selected...Add it's contents to the group.
				ALGetCell(&cellData, &cell, hAL);

				for (more = true, index = 0; more && err == noErr; index++) {
					err = CallALOutputFlavorsProc(index, &more, false, &flavorDesc, cellData, &cell, hAL, (*hAL)->outputFlavorsHook);
					if (flavorDesc.dataHandle != nil) {
						_ALSetHandleLock(flavorDesc.dataHandle, true);
						if (first)
							// Put the data on the scrap.
							// This will cause ALL of the first selected cell's data to be on the scrap.
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
							err = PutScrap(GetHandleSize(flavorDesc.dataHandle), flavorDesc.descriptorType,
											*flavorDesc.dataHandle);
#else
							err = PutScrapFlavor( scrapRef, flavorDesc.descriptorType, kScrapFlavorMaskNone,
										GetHandleSize( flavorDesc.dataHandle ), *flavorDesc.dataHandle );
#endif
						else
							// If there's more than one selected cell, that data goes into a group structure.
							// Add the data to the group.
							err = local_ALAddDataToGroup(&flavorDesc, &groupData);
						_ALSetHandleLock(flavorDesc.dataHandle, false);
						_ALForgetHandle(&flavorDesc.dataHandle);
					}
				} // for more

				first = false;
			} // cell is selected

	// Throw the group of data onto the Scrap also.
	if ( groupData != nil ) {
		_ALSetHandleLock(groupData, true);
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
		err = PutScrap(GetHandleSize(groupData), kALType_ListData, *groupData);
#else
		err = PutScrapFlavor( scrapRef, kALType_ListData, kScrapFlavorMaskNone, GetHandleSize( groupData ), *groupData );
#endif
		_ALSetHandleLock(groupData, false);
		_ALForgetHandle(&groupData);
	}
}

ALIST_API Boolean	ALCanPaste(ALHandle hAL)
{	Boolean			more;
	ALDataDescriptor	flavorDesc = {typeNull, nil};
	LongRect			theBounds;
	ALCell			cell;
	ALData			cellData;
	int				index;
	OSErr			err;

	if (hAL == nil || *hAL == nil || (*hAL)->inputFlavorsHook == nil)
		return false;

	theBounds = (*hAL)->dataBounds;
	// Check every cell...there may be different flavors in different cells.
	for (cell.h = theBounds.left; cell.h < theBounds.right; cell.h++)
		// This goes one beyond the bottom of the list, in order to make sure it's called for an empty list.
		for (cell.v = theBounds.top; cell.v <= theBounds.bottom; cell.v++) {
			ALGetCell(&cellData, &cell, hAL);
			for (more = true, index = 0; more; index++) {
				err = CallALInputFlavorsProc(index, &more, &flavorDesc, &cellData, &cell, hAL, (*hAL)->inputFlavorsHook);

				// Just in case somebody put data into it.
				_ALForgetHandle(&flavorDesc.dataHandle);

				if ( err == noErr ) {
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
					SInt32	scrapOffset;
					if ( GetScrap(nil, flavorDesc.descriptorType, &scrapOffset) > 0)
						return true;
#else
					ScrapRef			scrapRef;
					ScrapFlavorFlags	flavorFlags;
					err = GetCurrentScrap( &scrapRef );
					if ( err == noErr && GetScrapFlavorFlags( scrapRef, flavorDesc.descriptorType, &flavorFlags ) == noErr )
						return true;
#endif
				} // err == noErr
			}
		}

	return false;
}

ALIST_API void		ALPaste(ALHandle hAL)
{	Boolean			more, rowAdded, dataUsed;
	ALDataDescriptor	flavorDesc = {typeNull, nil};
	ALCell			cell;
	ALData			cellData = nil;
	int				index;
	OSErr			err;
	Handle			groupData = nil;
	long				marker;
#if defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 )
	ScrapRef			scrapRef = nil;
	Size				scrapSize;
#else
	SInt32			scrapOffset;
#endif

	if (hAL == nil || *hAL == nil || (*hAL)->inputFlavorsHook == nil)
		return;

	// We haven't added the row yet.
	rowAdded = false;
	// Find the first selected cell.  If none, add items at the end of the list.
	cell.h = cell.v = 0;
	if (!ALGetSelect(true, &cell, hAL))
		cell.v = (*hAL)->dataBounds.bottom;

	// Check if there is data from an AList copy procedure; get data if it's there.
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
	err = _ALAllocate(0, kAllocTemp, &groupData);
	if (GetScrap(groupData, kALType_ListData, &scrapOffset) <= 0)
		// If there's no data, dispose of the handle.
		_ALForgetHandle( &groupData );
#else
	err = GetCurrentScrap( &scrapRef );
	if ( err == noErr ) {
		err = GetScrapFlavorSize( scrapRef, kALType_ListData, &scrapSize );
		if ( err == noErr && scrapSize > 0 ) {
			err = _ALAllocate( scrapSize, kAllocTemp, &groupData );
			if ( err == noErr ) {
				_ALSetHandleLock( groupData, true );
				err = GetScrapFlavorData( scrapRef, kALType_ListData, &scrapSize, *groupData );
				_ALSetHandleLock( groupData, false );
			}
		}
	}
#endif

	for ( more = true, index = 0; more; index++ ) {
		// Ask for what flavors it supports
		err = CallALInputFlavorsProc(index, &more, &flavorDesc, &cellData, &cell, hAL, (*hAL)->inputFlavorsHook);

		if (err == noErr) {
#if !defined( TARGET_API_MAC_CARBON ) || ( TARGET_API_MAC_CARBON == 0 )
			// allocate a handle to hold a scrap item
			if (_ALAllocate(0, kAllocTemp, &flavorDesc.dataHandle) != noErr)
					return;

			if (GetScrap(flavorDesc.dataHandle, flavorDesc.descriptorType, &scrapOffset) > 0)
#else
			err = GetCurrentScrap( &scrapRef );
			if ( err == noErr ) {
				err = GetScrapFlavorSize( scrapRef, flavorDesc.descriptorType, &scrapSize );
				if ( err == noErr && scrapSize > 0 ) {
					err = _ALAllocate( scrapSize, kAllocTemp, &flavorDesc.dataHandle );
					if ( err == noErr ) {
						_ALSetHandleLock( flavorDesc.dataHandle, true );
						err = GetScrapFlavorData( scrapRef, flavorDesc.descriptorType, &scrapSize, *flavorDesc.dataHandle );
						_ALSetHandleLock( flavorDesc.dataHandle, false );
					}
				}
			}
			if ( err == noErr && flavorDesc.dataHandle != nil )
#endif
				// Add this data to the cell.
			{	err = CallALInputFlavorsProc(index, &more, &flavorDesc, &cellData, &cell, hAL, (*hAL)->inputFlavorsHook);
				if (err == noErr) {
					// Only add the row if there are no errors and it hasn't been added yet.
					if (!rowAdded)
						ALAddRow(1, cell.v, hAL);

					ALSetCell(cellData, &cell, hAL);
					cell.v++;
				}
				_ALForgetHandle(&flavorDesc.dataHandle);
			} // OK scrap item
		} // noErrs
	} // for loop

	if ( groupData != nil ) {
		// Need to go through the group data.  NOTE:  The first item will be handled by the above code, since it was put on
		// the regular scrap.
		for ( marker = 0; marker < GetHandleSize( groupData ); ) {
			dataUsed = false;
			for ( more = true, index = 0; more; index++ ) {
				// Ask for what flavors it supports
				err = CallALInputFlavorsProc(index, &more, &flavorDesc, &cellData, &cell, hAL, (*hAL)->inputFlavorsHook);

				if (err == noErr) {
					// allocate a handle to hold a scrap item
					if (_ALAllocate( 0, kAllocTemp, &flavorDesc.dataHandle ) != noErr)
							return;

					if ( local_ALGetDataFromGroup( flavorDesc.dataHandle, flavorDesc.descriptorType, groupData, &marker ) > 0 ) {
						// Add this data to the cell.
						dataUsed = true;
						err = CallALInputFlavorsProc(index, &more, &flavorDesc, &cellData, &cell, hAL, (*hAL)->inputFlavorsHook);
						if (err == noErr) {
							// Only add the row if there are no errors and it hasn't been added yet.
							if (!rowAdded)
								ALAddRow(1, cell.v, hAL);

							ALSetCell(cellData, &cell, hAL);
							cell.v++;
						}
					} // OK scrap item

					_ALForgetHandle( &flavorDesc.dataHandle );
				} // noErrs
			} // for more data in one cell loop

			if ( !dataUsed ) {
				// If no data was used, we need to advance the marker here so we don't sit in an infinite loop.

				// First, advance past the data type.
				marker += sizeof(long);
				// Now, advance past the actual data.
				marker += *(long *)&((*groupData)[ marker ] );
				// Adjust for the dataSize parameter itself.
				marker += sizeof(long);
			}

		} // for more data in group loop
	}

	_ALForgetHandle(&flavorDesc.dataHandle);
	_ALForgetHandle(&groupData);
}

ALIST_API void		ALDelete(ALHandle hAL)
{	ALCell	cell;
	short	oldStatus;

	if (hAL == nil || *hAL == nil)
		return;

	// Turn drawing off.
	oldStatus = ALFeatureFlag(alFInhibitRedraw, alBitSet, hAL);

	// Look for selected cells and delete them
	cell.h = (*hAL)->dataBounds.left;
	for (cell.v = (*hAL)->dataBounds.bottom - 1; cell.v >= (*hAL)->dataBounds.top; cell.v--) {
		if (_ALCellIsSelected(&cell, hAL))
			ALDelRow(1, cell.v, hAL);
	}

	_ALCalcVisibleCells(hAL);

	// Reset drawing to previous status.
	ALFeatureFlag( alFInhibitRedraw, oldStatus, hAL );

	// Redraw if necessary.
	if (!BTST((*hAL)->features, alFInhibitRedraw))
		ALUpdate(nil, hAL);
}
