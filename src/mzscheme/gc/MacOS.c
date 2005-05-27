/*
	MacOS.c
	
	Some routines for the Macintosh OS port of the Hans-J. Boehm, Alan J. Demers
	garbage collector.
	
	<Revision History>
	
	11/22/94  pcb  StripAddress the temporary memory handle for 24-bit mode.
	11/30/94  pcb  Tracking all memory usage so we can deallocate it all at once.
	02/10/96  pcb  Added routine to perform a final collection when unloading shared library.
	
	by Patrick C. Beard.

	PLTSCHEME: this file is substantially modified for MzScheme/MrEd.
 */
/* Boehm, November 17, 1995 11:50 am PST */

#include <Resources.h>
#include <Memory.h>
#include <LowMem.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "private/gc_priv.h"

// use 'CODE' resource 0 to get exact location of the beginning of global space.

typedef struct {
	unsigned long aboveA5;
	unsigned long belowA5;
	unsigned long JTSize;
	unsigned long JTOffset;
} *CodeZeroPtr, **CodeZeroHandle;

void* GC_MacGetDataStart()
{
	CodeZeroHandle code0 = (CodeZeroHandle)GetResource('CODE', 0);
	if (code0) {
		long belowA5Size = (**code0).belowA5;
		ReleaseResource((Handle)code0);
		return (LMGetCurrentA5() - belowA5Size);
	}
	fprintf(stderr, "Couldn't load the jump table.");
	exit(-1);
	return 0;
}

/* PLTSCHEME: Function for handling CW Pro 3 far data */
void* GC_MacGetDataEnd()
{
	CodeZeroHandle code0 = (CodeZeroHandle)GetResource('CODE', 0);
	if (code0) {
		long aboveA5Size = (**code0).aboveA5;
		ReleaseResource((Handle)code0);
		return (LMGetCurrentA5() + aboveA5Size);
	}
	fprintf(stderr, "Couldn't load the jump table.");
	exit(-1);
	return 0;
}

/* track the use of temporary memory so it can be freed all at once. */

typedef struct TemporaryMemoryBlock TemporaryMemoryBlock, **TemporaryMemoryHandle;

struct TemporaryMemoryBlock {
	TemporaryMemoryHandle nextBlock;
	char data[];
};

static TemporaryMemoryHandle theTemporaryMemory = NULL;
static Boolean firstTime = true;

void GC_MacFreeTemporaryMemory(void);

Ptr GC_MacTemporaryNewPtr(size_t size, Boolean clearMemory)
{
	static Boolean firstTime = true;
	OSErr result;
	TemporaryMemoryHandle tempMemBlock;
	Ptr tempPtr;

	/* PLTSCHEME: IM requests that temp memory not be locked across 
	   calls to GetNextEvent or WaitNextEvent. So, we'll use regular
	   pointers, but grab temp memory if we run out completely. 
	   Also, we'll be nicer about not grabbing *all* of temp memory,
	   as this seems to bring down the whole system. */
	if ((FreeMem() - size) >= 65536) { /* resort to tmp mem if local < some amount */ 
	  if (clearMemory)
	  	tempPtr = NewPtrClear(size);
	  else
	  	tempPtr = NewPtr(size);
	  if (tempPtr)
	    return tempPtr;		
	} else
	  tempPtr = NULL;
	
	if ((TempFreeMem() - size) < 65536) {
	  /* Not much temp mem availabl, either. Give up. */
	  return NULL;
	}

	tempMemBlock = (TemporaryMemoryHandle)TempNewHandle(size + sizeof(TemporaryMemoryBlock), &result);
	if (tempMemBlock && result == noErr) {
		HLockHi((Handle)tempMemBlock);
		tempPtr = (**tempMemBlock).data;
		if (clearMemory) memset(tempPtr, 0, size);
		tempPtr = StripAddress(tempPtr);

		// keep track of the allocated blocks.
		(**tempMemBlock).nextBlock = theTemporaryMemory;
		theTemporaryMemory = tempMemBlock;
	}

#if !defined(SHARED_LIBRARY_BUILD)
	// install an exit routine to clean up the memory used at the end.
	if (firstTime) {
		atexit(&GC_MacFreeTemporaryMemory);
		firstTime = false;
	}
#endif

	return tempPtr;
}

static void perform_final_collection()
{
	int i;
	
	/* adjust the stack bottom, because CFM calls us from another stack location. */
	GC_stackbottom = (ptr_t)&i;
	
	/* try to collect everything in sight (from test.c). Is this safe? */
	while (GC_collect_a_little()) ;
	for (i = 0; i < 16; i++)
		GC_gcollect();
}

void GC_MacFreeTemporaryMemory()
{
#if defined(SHARED_LIBRARY_BUILD)
	/* collect all memory, and invoke all finalizers. */
	perform_final_collection();
#endif

	if (theTemporaryMemory != NULL) {
		long totalMemoryUsed = 0;
		TemporaryMemoryHandle tempMemBlock = theTemporaryMemory;
		while (tempMemBlock != NULL) {
			TemporaryMemoryHandle nextBlock = (**tempMemBlock).nextBlock;
			totalMemoryUsed += GetHandleSize((Handle)tempMemBlock);
			DisposeHandle((Handle)tempMemBlock);
			tempMemBlock = nextBlock;
		}
		theTemporaryMemory = NULL;
		
#if !defined(SILENT) && !defined(SHARED_LIBRARY_BUILD)
		fprintf(stdout, "[total memory used:  %ld bytes.]\n", totalMemoryUsed);
		fprintf(stdout, "[total collections:  %ld.]\n", GC_gc_no);
#endif
	}
}
