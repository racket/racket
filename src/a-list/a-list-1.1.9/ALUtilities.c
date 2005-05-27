/*
 *	ALUtilities.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#include "AListInternal.h"

Boolean _ALBlockCmp(const void *block1, const void *block2, register long blockSize)
{	register const char	*p1 = (const char *) block1;
	register const char	*p2 = (const char *) block2;

	while ( --blockSize >= 0 )
		if ( *p1++ != *p2++ )
			return false;
	
	return true;
}

void _ALBlockClr(void *block, register long blockSize)
{	register char	*p = (char *) block;

	while ( --blockSize >= 0 )
		*p++ = 0;
}

void _ALForgetHandle(Handle *h)
{	Handle	theHandle;

	if (h == nil)
		return;

	if ((theHandle = *h) != nil) {
		*h = nil;
		DisposeHandle(theHandle);
	}
}

Boolean _ALSetHandleLock(Handle h, Boolean lock)
{	Boolean	oldLock = (HGetState(h) & (1 << 7)) != 0;
	
	if (lock != oldLock) {
		if (lock)
			HLock(h);
		else
			HUnlock(h);
	}

	return oldLock;
}

void _ALReorder(long *a, long *b)
{	// Make sure that *b is greater than or equal to *a.
	if (*a > *b) {
		long temp = *a;
		*a = *b;
		*b = temp;
	}
}

OSErr _ALAllocate(long blockSize, short allocFlags, Handle *h)
{
	// Allocate a new relocatable block.
	// AllocFlags may specify whether the block should be cleared and whether
	// temporary memory should be used.

	Handle	theHandle = nil;
	OSErr	err;

	// if kAllocTemp is specified, try tapping temporary memory
	if (allocFlags & kAllocTemp)
		theHandle = TempNewHandle(blockSize, &err);

	// if kAllocTemp isn't specified, or TempNewHandle failed, try with current heap
	if (theHandle == nil)  {
		theHandle = NewHandle(blockSize);
		err = MemError();
	}
	
	// if kAllocClear is specified, zero the block
	if ((allocFlags & kAllocClear) && (theHandle != nil))
		_ALBlockClr(*theHandle, blockSize);

	*h = theHandle;	
	return err;
}
