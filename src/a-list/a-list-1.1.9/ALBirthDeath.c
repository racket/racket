/*
 *	ALBirthDeath.c
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
#ifndef __APPEARANCE__
	#include <Appearance.h>
#endif
#if defined( TARGET_API_MAC_CARBON )
	#if !defined( __CONTROLDEFINITIONS__ )
		#include <ControlDefinitions.h>
	#endif
#endif
#ifndef __TEXTUTILS__
	#include <TextUtils.h>
#endif
#ifndef __TOOLUTILS__
	#include <ToolUtils.h>
#endif

#ifndef __DRAG__
# include <Drag.h>
#endif

// Include the DragManagerAdditions only if we're using an old version of the Universal Interfaces.
#ifdef __MWERKS__
#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0300)
	#include "DragManagerAdditions.h"
#endif
#endif

#include "AListInternal.h"
#include "LongControls.h"
#include "QDDrawingState.h"

#if TARGET_RT_MAC_CFM
#ifndef __CODEFRAGMENTS__
	#include <CodeFragments.h>
#endif
#endif

static ALIST_API void	local_ALStdDrawCell(ALData cellData, ALCellPtr cell, const Rect *cellRect, ALHandle hAL);
static ALIST_API void	local_ALStdDrawBackground(const Rect *cellRect, ALHandle hAL);
static ALIST_API void	local_ALStdHiliteCell(ALData cellData, ALCellPtr cell, Boolean active, Boolean doOutline,
								const Rect *cellRect, ALHandle hAL);
static ALIST_API Boolean	local_ALStdClickInCell(const ALCellPtr theCell, Point mouseLoc, EventModifiers modifiers, short numberClicks,
								ALHandle hAL);
static ALIST_API OSErr	local_ALStdInputFlavors(short index, Boolean *more, ALDataDescriptor *flavorDesc, ALData *cellDataPtr,
								const ALCellPtr cell, ALHandle hAL);
static ALIST_API OSErr	local_ALStdOutputFlavors(short index, Boolean *more, Boolean forDrag, ALDataDescriptor *flavorDesc,
								ALData cellData, const ALCellPtr cell, ALHandle hAL);
static ALIST_API void	local_ALStdDisposeCellData(ALData cellData, const ALCellPtr cell, ALHandle hAL);
static pascal OSErr		local_ALStdSendData(FlavorType requestedType, void *dragSendRefCon, ItemReference theItem,
								DragReference theDrag);
static ALIST_API Boolean	local_ALStringSearch( StringHandle theString, ALCell *cell, ALHandle hAL );

// static variables
static struct {
	int					numOfUsers;
	ALDrawCellUPP			_alStdDrawCellProc;
	ALDrawBackgroundUPP	_alStdDrawBackgroundProc;
	ALHiliteCellUPP			_alStdHiliteCellProc;
	ALInputFlavorsUPP		_alStdInputFlavorsProc;
	ALOutputFlavorsUPP		_alStdOutputFlavorsProc;
	ALDisposeCellDataUPP	_alStdDisposeCellDataProc;
	DragSendDataUPP		_alStdSendDataDragProc;
	ControlActionUPP		_alHorzTrackerUPP, _alVertTrackerUPP;
	ALStringSearchUPP		_alStdStringSearchProc;
} uppInfo = { 0, nil, nil, nil, nil, nil, nil, nil, nil, nil, nil };

ALIST_API short ALVersion( void )
{	return 0x0118;	}

ALIST_API OSErr ALNew(const WindowPtr wr, const Rect *dispRect, const LongRect *dataBounds, Point cellSize,
				unsigned long features, ALHandle *hAL)
{	ALPtr	pAL = nil;
	short	allocFlags = kAllocClear;
	long		response;
	long		temp;
	Rect		r;
	OSErr	err;

	// Do a few sanity checks first.
	// Check for NULLs, zeros and bad rectangles.
	if (wr == nil || dispRect == nil || dataBounds == nil || hAL == nil ||
			cellSize.v == 0 || cellSize.h == 0 || dispRect->top > dispRect->bottom || dispRect->left > dispRect->right ||
			dataBounds->top > dataBounds->bottom || dataBounds->left > dataBounds->right)
		return paramErr;

	// allocate the AL record
	if ((err = _ALAllocate(sizeof(ALRec), allocFlags, (Handle *)hAL)) != noErr)
		goto cleanup;
	
	// lock it down
	HLock((Handle)*hAL);
	pAL = **hAL;

	pAL->winRef = wr;

	// Check for the presence of various system software features.
	// determine whether Color Quickdraw is available
	if ((Gestalt(gestaltQuickdrawVersion, &response) == noErr) && (response >= gestalt8BitQD))
		BSET(pAL->flags, alFHasColorQD);
	
	// Determine whether the Drag Manager is available
	if ((Gestalt(gestaltDragMgrAttr, &response) == noErr) && BTST(response, gestaltDragMgrPresent)) {
#if TARGET_RT_MAC_CFM
		if ((unsigned long) NewDrag != kUnresolvedCFragSymbolAddress)
#endif
			BSET(pAL->flags, alFHasDragManager);

		// Determine whether translucent drags are available.
		if (BTST(response, gestaltDragMgrHasImageSupport))
			BSET(pAL->flags, alFHasTranslucentDrags);
	}

#if ALIST_USEAPPEARANCEMGR
	if ((Gestalt(gestaltAppearanceAttr, &response) == noErr) && BTST(response, gestaltAppearanceExists)) {
#if TARGET_RT_MAC_CFM
		if ((unsigned long) RegisterAppearanceClient != kUnresolvedCFragSymbolAddress)
#endif
			BSET(pAL->flags, alFHasAppearanceMgr);
	}
#endif

#if ALIST_USECONTROLMGR2
#if defined(UNIVERSAL_INTERFACES_VERSION) && (UNIVERSAL_INTERFACES_VERSION >= 0x0320)
	if ((Gestalt(gestaltControlMgrAttr, &response) == noErr) && (response & gestaltControlMgrPresent)) {
#if TARGET_RT_MAC_CFM
		if ( (unsigned long) SetControlViewSize != kUnresolvedCFragSymbolAddress )
#endif
			BSET(pAL->flags, alFHasControlMgr2);
	}
#endif
#endif

	// determine whether temporary memory should be used for data structures
	if (BTST(features, alFUseTempMem))
		allocFlags += kAllocTemp;

	pAL->dataLength = (dataBounds->right - dataBounds->left) * (dataBounds->bottom - dataBounds->top);

#if ALIST_HAVE_CELLDATA
	// Allocate the data handle.
	if ((err = _ALAllocate(sizeof(ALData) * pAL->dataLength, allocFlags, (Handle *)&pAL->hData)) != noErr)
		goto cleanup;
#endif

	// Allocate the selection handle.
	if ((err = _ALAllocate(sizeof(ALSelection) * pAL->dataLength, allocFlags, (Handle *)&pAL->hSelected)) != noErr)
		goto cleanup;

#if ALIST_HEIRARCHICAL
	// Allocate the levels handle.
	if ((err = _ALAllocate(sizeof(ALRowLevel) * pAL->dataLength, allocFlags, (Handle *)&pAL->hLevels)) != noErr)
		goto cleanup;
#endif

	// Calculate which cells are visible.
	pAL->visCells.top = dataBounds->top;
	pAL->visCells.left = dataBounds->left;

	// Calculate what the vertical scroll bar maximum should be.
	temp = (dataBounds->bottom - dataBounds->top) - (dispRect->bottom - dispRect->top) / cellSize.v;
	if ((dispRect->bottom - dispRect->top) % cellSize.v != 0)
		temp += 1;

	if (temp < 0)
		temp = 0;

	// Calculate the visible cell rows.
	if (temp == 0)
		pAL->visCells.bottom = dataBounds->bottom;
	else {
		pAL->visCells.bottom = (dispRect->bottom - dispRect->top) / cellSize.v + dataBounds->top;
		if ((dispRect->bottom - dispRect->top) % cellSize.v != 0)
			pAL->visCells.bottom++;

		if (pAL->visCells.bottom > dataBounds->bottom)
			pAL->visCells.bottom = dataBounds->bottom;
	}

	// create the vertical scroll bar
	if (BTST(features, alFVertScroll)) {
		r = *dispRect;
		r.left = r.right - kScrollBarWidth;
		r.top -= 1;
		r.bottom += 1;

		// Leave room for the grow box
		if (BTST(features, alFHasGrow))
			r.bottom -= kScrollBarWidth - 1;

#if ALIST_USEAPPEARANCEMGR
		if ( BTST(pAL->flags, alFHasAppearanceMgr) ) {
			if ( BTST( features, alFDynamicScroll ) ) {
				pAL->vScroll = NewControl(wr, &r, "\p", true, 0, 0, temp, kControlScrollBarLiveProc, (long)*hAL);
				BSET( pAL->flags, alFHasLiveScrollbars );
			} else
				pAL->vScroll = NewControl(wr, &r, "\p", true, 0, 0, temp, kControlScrollBarProc, (long)*hAL);
		} else
#endif
			pAL->vScroll = NewControl(wr, &r, "\p", true, 0, 0, temp, scrollBarProc, (long)*hAL);

		if (pAL->vScroll == nil)
			goto cleanup;

		// Make it into a LongControl.
		if ((err = LCAttach(pAL->vScroll)) != noErr)
			goto cleanup;
	}
	else
		pAL->vScroll = nil;

	// Calculate what the scroll bar maximum should be.
	temp = (dataBounds->right - dataBounds->left) - (dispRect->right - dispRect->left) / cellSize.h;
	if ((dispRect->right - dispRect->left) % cellSize.h != 0)
		temp += 1;

	if (temp < 0)
		temp = 0;

	// Calculate the visible columns.
	if (temp == 0)
		pAL->visCells.right = dataBounds->right;
	else {
		pAL->visCells.right = (dispRect->right - dispRect->left) / cellSize.h + dataBounds->left;
		if ((dispRect->right - dispRect->left) % cellSize.h != 0)
			pAL->visCells.right++;

		if (pAL->visCells.right > dataBounds->right)
			pAL->visCells.right = dataBounds->right;
	}

	// create the horizontal scroll bar
	if (BTST(features, alFHorzScroll)) {
		r = *dispRect;
		r.top = r.bottom - kScrollBarWidth;
		r.left -= 1;
		r.right += 1;

		// Leave room for the grow box
		if (BTST(features, alFHasGrow))
			r.right -= kScrollBarWidth - 1;

#if ALIST_USEAPPEARANCEMGR
		if ( BTST(pAL->flags, alFHasAppearanceMgr) ) {
			if ( BTST( features, alFDynamicScroll ) ) {
				pAL->hScroll = NewControl(wr, &r, "\p", true, 0, 0, temp, kControlScrollBarLiveProc, (long)*hAL);
				BSET( pAL->flags, alFHasLiveScrollbars );
			} else
				pAL->hScroll = NewControl(wr, &r, "\p", true, 0, 0, temp, kControlScrollBarProc, (long)*hAL);
		} else
#endif
			pAL->hScroll = NewControl(wr, &r, "\p", true, 0, 0, temp, scrollBarProc, (long)*hAL);

		if (pAL->hScroll == nil)
			goto cleanup;

		// Make it into a LongControl.
		if ((err = LCAttach(pAL->hScroll)) != noErr)
			goto cleanup;
	}
	else
		pAL->hScroll = nil;

	// Initialize miscellaneous fields of the AL record.
	pAL->dataBounds = *dataBounds;
	pAL->cellSize = cellSize;
	pAL->lastClickCell.v = pAL->lastClickCell.h = -1;
	pAL->features = features;
	pAL->dragCaretLoc.v = pAL->dragCaretLoc.h = -1;
	pAL->offscreenPort = nil;
	// We're active by default.
	BSET(pAL->flags, alFActive);

	// create a region to hold the view rectangle
	pAL->viewRgn = NewRgn();
	r = *dispRect;
	if (pAL->vScroll != nil)
		r.right -= kScrollBarWidth;
	if (pAL->hScroll != nil)
		r.bottom -= kScrollBarWidth;

	pAL->dispRect = r;

	RectRgn(pAL->viewRgn, &r);

	// unlock the AL record
	HUnlock((Handle)*hAL);

	// initialize hook fields with the addresses of the standard hooks
	_ALSetStandardHooks( *hAL );

	// Set the standard string hook here, since the user can set the StringSearch hook to nil.
	(**hAL)->stringSearchHook = uppInfo._alStdStringSearchProc;

	// Go ahead and draw everything.
	ALUpdate( nil, *hAL );

	// skip clean-up section
	return noErr;

cleanup:
	// clean up
	if (pAL != nil) {
#if ALIST_HAVE_CELLDATA
		if (pAL->hData != nil)
			_ALForgetHandle( (Handle *) &pAL->hData );
#endif
		if (pAL->hSelected != nil)
			_ALForgetHandle((Handle *)&pAL->hSelected );
#if ALIST_HEIRARCHICAL
		if (pAL->hLevels != nil)
			_ALForgetHandle((Handle *)&pAL->hLevels );
#endif
		if (pAL->viewRgn != nil)
			DisposeRgn(pAL->viewRgn);
		if (pAL->hScroll != nil)
			DisposeControl(pAL->hScroll);
		if (pAL->vScroll != nil)
			DisposeControl(pAL->vScroll);
	}
	_ALForgetHandle((Handle *)hAL);

	return err;
}

ALIST_API void ALDispose(ALHandle hAL)
{	ALPtr pAL;

	// sanity check: make sure AL isn't nil
	if ( hAL == nil || *hAL == nil )
		return;

	// Make sure all of the data is disposed properly.  This calls the DisposeCellData callback routine for every cell.
	BSET( (*hAL)->features, alFInhibitRedraw );
	ALDelRow(0, 0, hAL);

	// lock the AL record
	HLock((Handle) hAL);
	pAL = *hAL;

	// dispose of the offscreen graphics world
	if (pAL->offscreenPort != nil)  {
		DisposeGWorld(pAL->offscreenPort);
		pAL->offscreenPort = nil;
	}

	// dispose of auxiliary data structures
#if ALIST_HAVE_CELLDATA
	_ALForgetHandle( (Handle *) &pAL->hData );
#endif
	_ALForgetHandle( (Handle *) &pAL->hSelected );
#if ALIST_HEIRARCHICAL
	_ALForgetHandle( (Handle *) &pAL->hLevels );
#endif
	_ALForgetHandle( (Handle *) &pAL->keyString );
	DisposeRgn(pAL->viewRgn);

	// dispose of the scroll bars
	if (pAL->hScroll != nil) {
		LCDetach( pAL->hScroll );
		DisposeControl(pAL->hScroll);
	}
	if (pAL->vScroll != nil) {
		LCDetach( pAL->vScroll );
		DisposeControl(pAL->vScroll);
	}

	// dispose of the AL record
	DisposeHandle((Handle) hAL);

	_ALRemoveStandardHookUser( );
} // ALDispose

ALIST_API void ALShow(int on, ALHandle hAL)
{
  if (on) {
    if ((*hAL)->vScroll != nil)
      ShowControl((*hAL)->vScroll);
    if ((*hAL)->hScroll != nil)
      ShowControl((*hAL)->hScroll);
  } else {
    if ((*hAL)->vScroll != nil)
      HideControl((*hAL)->vScroll);
    if ((*hAL)->hScroll != nil)
      HideControl((*hAL)->hScroll);
  }
  (*hAL)->scrolls_visible = on;
}

ALIST_API void ALSetViewRect(const Rect *viewRect, ALHandle hAL)
{	short	temp;
	Rect		box;

	// Sanity check!
	if (hAL == nil || *hAL == nil || viewRect == nil || viewRect->right < viewRect->left || viewRect->bottom < viewRect->top)
		return;

	// Set up the size of the list itself (without scroll bars).
	box = *viewRect;

	// Leave room for the scroll bars.
	if ((*hAL)->vScroll != nil)
		box.right -= kScrollBarWidth;
	if ((*hAL)->hScroll != nil)
		box.bottom -= kScrollBarWidth;

	(*hAL)->dispRect = box;

	// Create a new view rgn.
	RectRgn((*hAL)->viewRgn, &box);

	// Move and resize the vertical scroll bar.
	if ((*hAL)->vScroll != nil) {
	        if ((*hAL)->scrolls_visible)
		  HideControl((*hAL)->vScroll);
		MoveControl((*hAL)->vScroll, box.right, box.top - 1);

		// Calculate the hieght of the scroll bar.
		temp = box.bottom - box.top + 2;
		if ((*hAL)->hScroll != nil || BTST((*hAL)->features, alFHasGrow))
			temp -= kScrollBarWidth - 1;

		SizeControl((*hAL)->vScroll, kScrollBarWidth, temp);
		if ((*hAL)->scrolls_visible)
		  ShowControl((*hAL)->vScroll);
	}

	// Move and resize the horizontal scroll bar.
	if ((*hAL)->hScroll != nil) {
                if ((*hAL)->scrolls_visible)
		  HideControl((*hAL)->hScroll);
		MoveControl((*hAL)->hScroll, box.left - 1, box.bottom);

		// Calculate the hieght of the scroll bar.
		temp = box.right - box.left + 2;
		if ((*hAL)->vScroll != nil || BTST((*hAL)->features, alFHasGrow))
			temp -= kScrollBarWidth - 1;

		SizeControl((*hAL)->hScroll, temp, kScrollBarWidth);
		if ((*hAL)->scrolls_visible)
		  ShowControl((*hAL)->hScroll);
	}

	_ALCalcVisibleCells(hAL);

	if (!BTST((*hAL)->features, alFInhibitRedraw))
		ALUpdate(nil, hAL);
} // ALSetViewRect

ALIST_API void ALGetViewRect(Rect *viewRect, ALHandle hAL)
{	Rect		box;

	// Sanity check!
	if (hAL == nil || *hAL == nil || viewRect == nil)
		return;

	box = (*hAL)->dispRect;

	if ((*hAL)->vScroll != nil)
		box.right += kScrollBarWidth - 1;
	if ((*hAL)->hScroll != nil)
		box.bottom += kScrollBarWidth - 1;

	// To include the boundaries (and make the scroll bars work properly).
	InsetRect(&box, -1, -1);

	*viewRect = box;

	return;
} // ALGetViewRect


#pragma mark -

void _ALSetStandardHooks(ALHandle hAL)
{	ALPtr	pAL;
	Boolean	saveLock;

	// If there are no ALReferences, we need to allocate the UPPs.
	if ( uppInfo.numOfUsers == 0 ) {
		uppInfo._alStdDrawCellProc = NewALDrawCellProc( local_ALStdDrawCell );
		uppInfo._alStdDrawBackgroundProc = NewALDrawBackgroundProc( local_ALStdDrawBackground );
		uppInfo._alStdHiliteCellProc = NewALHiliteCellProc( local_ALStdHiliteCell );
		uppInfo._alStdInputFlavorsProc = NewALInputFlavorsProc( local_ALStdInputFlavors );
		uppInfo._alStdOutputFlavorsProc = NewALOutputFlavorsProc( local_ALStdOutputFlavors );
		uppInfo._alStdDisposeCellDataProc = NewALDisposeCellDataProc( local_ALStdDisposeCellData );
		uppInfo._alStdSendDataDragProc = NewDragSendDataUPP( local_ALStdSendData );
		uppInfo._alHorzTrackerUPP = NewControlActionUPP( _ALHorzScrollActionProc );
		uppInfo._alVertTrackerUPP = NewControlActionUPP( _ALVertScrollActionProc );
		uppInfo._alStdStringSearchProc = NewALStringSearchProc( local_ALStringSearch );
	} // if called for the first time

	uppInfo.numOfUsers++;

	saveLock = _ALSetHandleLock((Handle)hAL, true);

	pAL = *hAL;

	// replace null hook fields with the addresses of the standard hooks

	if (pAL->drawCellHook == nil)
		pAL->drawCellHook = uppInfo._alStdDrawCellProc;

	if (pAL->drawBackgroundHook == nil)
		pAL->drawBackgroundHook = uppInfo._alStdDrawBackgroundProc;

	if (pAL->hiliteCellHook == nil)
		pAL->hiliteCellHook = uppInfo._alStdHiliteCellProc;

// The clickLoop can be nil.	
//	if (pAL->clickLoop == nil)
//		pAL->clickLoop = uppInfo._alStdClickLoopProc;

	if (pAL->disposeCellDataHook == nil)
		pAL->disposeCellDataHook = uppInfo._alStdDisposeCellDataProc;

	if (pAL->inputFlavorsHook == nil)
		pAL->inputFlavorsHook = uppInfo._alStdInputFlavorsProc;

	if (pAL->outputFlavorsHook == nil)
		pAL->outputFlavorsHook = uppInfo._alStdOutputFlavorsProc;

	if (pAL->sendDataDragHook == nil)
		pAL->sendDataDragHook = uppInfo._alStdSendDataDragProc;

// The stringSearchHook can be nil.
//	if (pAL->stringSearchHook == nil)
//		pAL->stringSearchHook = uppInfo._alStdStringSearchProc;

	if (pAL->vScroll != nil)
		SetControlAction(pAL->vScroll, uppInfo._alVertTrackerUPP);

	if (pAL->hScroll != nil)
		SetControlAction(pAL->hScroll, uppInfo._alHorzTrackerUPP);

	_ALSetHandleLock((Handle)hAL, saveLock);
} // _ALSetStandardHooks

void _ALRemoveStandardHookUser( void )
{	uppInfo.numOfUsers--;

	if ( uppInfo.numOfUsers <= 0 ) {
		uppInfo.numOfUsers = 0;
		DisposeALDrawCellUPP( uppInfo._alStdDrawCellProc );
		DisposeALDrawBackgroundUPP( uppInfo._alStdDrawBackgroundProc );
		DisposeALHiliteCellUPP( uppInfo._alStdHiliteCellProc );
		DisposeALInputFlavorsUPP( uppInfo._alStdInputFlavorsProc );
		DisposeALOutputFlavorsUPP( uppInfo._alStdOutputFlavorsProc );
		DisposeALDisposeCellDataUPP( uppInfo._alStdDisposeCellDataProc );
#if !defined(UNIVERSAL_INTERFACES_VERSION) ||  (UNIVERSAL_INTERFACES_VERSION < 0x0330)
		DisposeRoutineDescriptor( uppInfo._alStdSendDataDragProc );
		DisposeRoutineDescriptor( uppInfo._alHorzTrackerUPP );
		DisposeRoutineDescriptor( uppInfo._alVertTrackerUPP );
#else
		DisposeDragSendDataUPP( uppInfo._alStdSendDataDragProc );
		DisposeControlActionUPP( uppInfo._alHorzTrackerUPP );
		DisposeControlActionUPP( uppInfo._alVertTrackerUPP );
#endif
		DisposeALStringSearchUPP( uppInfo._alStdStringSearchProc );
	}
} // _ALRemoveStandardHookUser

#pragma mark -

static ALIST_API void local_ALStdDrawCell( ALData cellData, ALCellPtr cell, const Rect *cellRect, ALHandle hAL )
{
#if !ALIST_USEAPPEARANCEMGR
	#pragma unused( hAL )
#endif
#pragma unused( cell )
	unsigned char		*ptr;
	Str255			theText;
	long				width;
	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	const Point		kOneToOneScaling = { 1, 1 };
	QDDrawingState	saveState;

	// Don't try to do anything with a nil handle.
	if ( cellData == nil )
		return;

	GetGWorld( &saveWorld, &saveDevice );
	SaveQDDrawingState( &saveState, true );

	HLock((Handle)cellData);
	ptr = *(StringHandle)cellData;

	// Copy the string to a local variable so it can be truncated if necessary.
	BlockMoveData( ptr, theText, ptr[ 0 ] + 1 );

	HUnlock((Handle)cellData);

	width = cellRect->right - cellRect->left - kAL_CellMargin;

#if ALIST_USEAPPEARANCEMGR
	if ( BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 )
		SetThemeTextColor( kThemeListViewTextColor, (*(*saveDevice)->gdPMap)->pixelSize,
					(*(*saveDevice)->gdPMap)->pixelSize > 1 );
	else
#endif
	{	RGBColor		black = { 0, 0, 0 };
		RGBForeColor( &black );
	}

	if ( StringWidth( theText ) > width ) {
		// Condense the font if it's not already.
		if ( !BTST( saveState.textStyle.tsFace, condense ) )
			TextFace( saveState.textStyle.tsFace + condense );
		(void)TruncString( width, theText, truncEnd );
	}

	// Draw the text.
	MoveTo( cellRect->left + kAL_CellMargin, cellRect->bottom - kAL_CellMargin );
	DrawJustified( (char *)&theText[ 1 ], theText[ 0 ], 1.0, onlyStyleRun, kOneToOneScaling, kOneToOneScaling );

	RestoreQDDrawingState( &saveState, true );
} // _ALStdDrawCell

static ALIST_API void local_ALStdDrawBackground( const Rect *cellRect, ALHandle hAL )
{	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	Rect				box;
	RGBColor			notepadColor;

	GetGWorld(&saveWorld, &saveDevice);

	box = *cellRect;

#if ALIST_HEIRARCHICAL
	if (ALFeatureFlag(alFHeirarchical, alBitTest, hAL))// && theCell.h = (*hAL)->dataBounds.left)
		box.left -= 14;
#endif

#if ALIST_USEAPPEARANCEMGR
	if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL)) {
		SetThemeBackground(kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
		EraseRect( &box );

		// Draw separators between cells.
		SetThemePen( kThemeBrushListViewSeparator, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
		if ( ALFeatureFlag( alFRowsOnly, alBitTest, hAL ) ) {
			MoveTo( box.left, box.bottom - 1 );
			LineTo( box.right, box.bottom - 1 );
		} else if ( ALFeatureFlag( alFColumnsOnly, alBitTest, hAL ) ) {
			MoveTo( box.right - 1, box.top );
			LineTo( box.right - 1, box.bottom );
		} else {
			box.top -= 1;
			box.left -= 1;
			FrameRect(&box);
			box.top += 1;
			box.left += 1;
		}
	} else
#endif
		if ( ALFeatureFlag( alFNotepadBackground, alBitTest, hAL ) ) {
		_ALSetNotepadBackgroundColor( );
		EraseRect( &box );

		// Draw blue lines between cells.
		notepadColor.red = notepadColor.green = 32767;
		notepadColor.blue = 65535;
		RGBForeColor(&notepadColor);
		if ( ALFeatureFlag( alFRowsOnly, alBitTest, hAL ) ) {
			MoveTo( box.left, box.bottom - 1 );
			LineTo( box.right, box.bottom - 1 );
		} else if ( ALFeatureFlag( alFColumnsOnly, alBitTest, hAL ) ) {
			MoveTo( box.right - 1, box.top );
			LineTo( box.right - 1, box.bottom );
		} else {
			// Make lines at top/bottom and right/left intersect.
			box.top -= 1;
			box.left -= 1;
			FrameRect(&box);
		}

#if ALIST_HEIRARCHICAL
		if (ALFeatureFlag(alFHeirarchical, alBitTest, hAL)) {// && theCell.h = (*hAL)->dataBounds.left ) {
			box.left += 14;
			notepadColor.red = 65535;
			notepadColor.green = notepadColor.blue = 10000;
			RGBForeColor(&notepadColor);
			MoveTo(box.left, box.top);
			LineTo(box.left, box.bottom);
		}
#endif
	} else
		// Do a simple erase.
		EraseRect(&box);
}

static ALIST_API void local_ALStdHiliteCell(ALData cellData, ALCellPtr cell, Boolean active, Boolean doOutline,
					const Rect *cellRect, ALHandle hAL)
{
#pragma unused( cellData, cell )
	unsigned char		saveMode, newMode;
	Rect				box;
	char				width;
	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	QDDrawingState	saveState;
	Boolean			useHiliteMode = true;

	GetGWorld( &saveWorld, &saveDevice );

#if ALIST_USEAPPEARANCEMGR
	if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
		// If we're using the Appearance Manager background, the background could be a pattern
		// so we'll simply invert without using the hilite mode bit.
		useHiliteMode = false;
#endif

	if ( useHiliteMode ) {
		// Save the old hilite mode.
		saveMode = LMGetHiliteMode();
		// Set the hilite mode.
		newMode = saveMode;
		BitClr((Ptr)(&newMode), (long)pHiliteBit);
		LMSetHiliteMode( newMode );
	}

	SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
	if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
		SetThemeBackground( kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
	else
#endif
		if ( ALFeatureFlag(alFNotepadBackground, alBitTest, hAL ) )
		_ALSetNotepadBackgroundColor( );

	if (active && !doOutline)
		// Invert the entire rectangle
		InvertRect( cellRect );
	else {
		// Invert around the edges.
		if (ALFeatureFlag(alFDrawLines, alBitTest, hAL) == alBitSet)
			width = 2;
		else
			width = 1;

		// Do the top.
		box = *cellRect;
		box.bottom = box.top + width;
		InvertRect(&box);
		if ( useHiliteMode )
			LMSetHiliteMode(newMode);
		// Do the bottom.
		box = *cellRect;
		box.top = box.bottom - width;
		InvertRect(&box);
		if ( useHiliteMode )
			LMSetHiliteMode(newMode);
		// Do the left.
		box = *cellRect;
		box.top += width;
		box.bottom -= width;
		box.right = box.left + width;
		InvertRect(&box);
		if ( useHiliteMode )
			LMSetHiliteMode(newMode);
		// Do the right.
		box = *cellRect;
		box.top += width;
		box.bottom -= width;
		box.left = box.right - width;
		InvertRect(&box);
	}

	if ( useHiliteMode )
		// Restore the old hilite mode.
		LMSetHiliteMode(saveMode);

	RestoreQDDrawingState( &saveState, true );
} // _ALStdHiliteCell

static ALIST_API OSErr	local_ALStdInputFlavors(short index, Boolean *more, ALDataDescriptor *flavorDesc, ALData *cellDataPtr,
										const ALCellPtr cell, ALHandle hAL)
{
#pragma unused( index, cell, hAL )
	Size		theSize;
	OSErr	err;

	err = noErr;

	*more = false;

	if (flavorDesc->dataHandle == nil)
		// We are just being asked for the types we support.
		flavorDesc->descriptorType = kALType_Text;
	else if (flavorDesc->descriptorType == kALType_Text) {
		// Copy the data into the cellDataPtr as a StringHandle.
		theSize = GetHandleSize(flavorDesc->dataHandle);

		// If the text is longer than 255 characters, take only the first 255.
		if (theSize > 255)
			theSize = 255;

		err = _ALAllocate(theSize + 1, 0, (Handle *)cellDataPtr);
		if (err == noErr) {
			BlockMoveData( *flavorDesc->dataHandle, &(**(Handle *)cellDataPtr)[1], theSize );
			(**(Handle *)cellDataPtr)[0] = theSize;
		}
	} else
		return alSkipDragItem;

	return err;
} // _ALStdInputFlavors

static ALIST_API OSErr	local_ALStdOutputFlavors(short index, Boolean *more, Boolean forDrag, ALDataDescriptor *flavorDesc,
										ALData cellData, const ALCellPtr cell, ALHandle hAL)
{
#pragma unused( index, cell, hAL )
	Size		theSize;
	OSErr	err;

	// Just copy the cellData into the flavorDesc as a text handle.

	err = noErr;

	*more = false;
	(*flavorDesc).descriptorType = kALType_Text;

	if (cellData == nil)
		err = errAENoSuchObject;
	else if (!forDrag) {
		// If it's for a drag, don't put any data in.  We'll do a 'lazy' drag instead.

		// Don't copy the length byte.
		theSize = GetHandleSize((Handle)cellData) - 1;

		err = _ALAllocate(theSize, 0, &(*flavorDesc).dataHandle);

		if (err == noErr)
			BlockMoveData( &(*(Handle)cellData)[1], *(*flavorDesc).dataHandle, theSize );
	}

	return err;
} // _ALStdOutputFlavors

static ALIST_API void local_ALStdDisposeCellData(ALData cellData, const ALCellPtr cell, ALHandle hAL )
{
#pragma unused( cell, hAL )
	_ALForgetHandle( (Handle *) &cellData );
} // _ALStdDisposeData

static pascal OSErr local_ALStdSendData(FlavorType requestedType, void *dragSendRefCon, ItemReference theItem,
								DragReference theDrag)
{	OSErr	err;
	ALData	cellData;
	ALCell	cell;
	ALHandle	theAL = (ALHandle)dragSendRefCon;

#if !TARGET_RT_MAC_CFM
	long saveA5 = SetCurrentA5( );	// this fixes a conflict with HoverBar
									// (well, probably a bug in the Drag Manager)
#endif

	// Make sure it's the right type.
	if (requestedType != kALType_Text)
		err = badDragFlavorErr;
	else {
		ALGetCellFromItemRef(theItem, &cell);
		ALGetCell(&cellData, &cell, theAL);

		if (cellData != nil) {
			_ALSetHandleLock((Handle)cellData, true);
			// set the drag flavor data
			err = SetDragItemFlavorData(theDrag, theItem, requestedType,
						&(*(Handle)cellData)[1], GetHandleSize((Handle)cellData) - 1, 0);
			_ALSetHandleLock((Handle)cellData, false);
		} else
			err = cantGetFlavorErr;
	}

#if !TARGET_RT_MAC_CFM
	SetA5( saveA5 );
#endif

	// return result code
	return err;
} // _ALStdSendData

static ALIST_API Boolean local_ALStringSearch( StringHandle theString, ALCell *cell, ALHandle hAL )
{	StringHandle	cellData;
	ALCell		tempCell;
	char			hIncr, vIncr;
	short		size;

	if ( theString == nil || cell == nil || hAL == nil )
		return false;

	if ( ALFeatureFlag( alFRowsOnly, alBitTest, hAL ) ) {
		hIncr = 0;
		vIncr = 1;
	} else if ( ALFeatureFlag( alFColumnsOnly, alBitTest, hAL ) ) {
		hIncr = 1;
		vIncr = 0;
	} else {
		hIncr = 1;
		vIncr = 1;
	}

	for ( tempCell.h = 0, tempCell.v = 0; tempCell.v < ALGetNumberRows( hAL ) && tempCell.h < ALGetNumberColumns( hAL );
									tempCell.v += vIncr, tempCell.h += hIncr ) {
		// Go through all the cells and get the data.
		ALGetCell( (ALData *)&cellData, &tempCell, hAL );
		if ( cellData != nil ) {
			size = (*cellData)[ 0 ];
			if ( (*theString)[ 0 ] < size )
				size = (*theString)[ 0 ];

			if ( _ALStrNCmp( &(*theString)[1], &(*cellData)[1], size ) == 0 ) {
				*cell = tempCell;
				return true;
			}
		}
	}

	return false;
} // _ALStringSearch

#pragma mark -

#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	Rect *_ALGetRegionBounds( const RgnHandle inRgn, Rect *outBox )
	{	*outBox = (**inRgn).rgnBBox;
		return outBox;
	}
	
	PixMapHandle _ALGetPortPixMap( const CGrafPtr port )
	{	return port->portPixMap;	}

	RgnHandle _ALGetPortVisibleRegion( const CGrafPtr port, RgnHandle rgn )
	{	CopyRgn( port->visRgn, rgn );
		return rgn;
	}
#endif
