/*
 *	ALDrawing.c
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
#include "AListInternal.h"
#include "QDDrawingState.h"

static short local_AppearanceMgrVersion( void );

enum {
	colorW	= 65535,
	color2	= 56797,
	color3	= 52428,
	color4	= 48059,
	color5	= 43690,
	color6	= 39321,
	color7	= 34952,
	color8	= 30583,
	color10	= 21845,
	color12	= 13107,
	colorB	= 0
};

static void local_ALDrawCells( Boolean doErase, ALHandle hAL );

static void refresh(ALHandle hAL)
{
  void *ctl;
  ALGetInfo(alRefCon, &ctl, hAL);
  HIViewSetNeedsDisplay((ControlHandle)ctl, TRUE);
}

INLINE void local_ALClearHiliteBit(void)
{	LMSetHiliteMode(LMGetHiliteMode() & 0x7F);	}

static short local_AppearanceMgrVersion( void )
{	OSErr	err;
	long		result;
	short	version;

	err = Gestalt( gestaltAppearanceAttr, &result );
	if ( (err == noErr) && (result & (1 << gestaltAppearanceExists)) ) {
		err = Gestalt( gestaltAppearanceVersion, &result );
		if ( err == noErr )
			version = LoWord( result );
		else
			version = 0x0100;
	} else
		version = 0x0000;

	return version;
}

void _ALCalcCellRect( Rect *cellRect, const ALCellPtr theCell, Boolean includeHeirarchical, ALHandle hAL )
{	Point			cellSize;
	LongRect		r;
	long			skippedCells;
#if ALIST_HEIRARCHICAL
	unsigned long	superRowOffset, offset;
	ALCell		tempCell;
#endif

	// Sanity check!
	if (hAL == nil || theCell == nil || cellRect == nil)
		return;

	r = (*hAL)->dataBounds;
	// Check for out of range cell.
	if (theCell->v < r.top || theCell->v > r.bottom || theCell->h < r.left || theCell->h > r.right)
		cellRect->top = cellRect->left = cellRect->bottom = cellRect->right = 0;
	else {
		skippedCells = 0;
#if ALIST_HEIRARCHICAL
		if ( BTST( (*hAL)->features, alFHeirarchical ) ) {
			// May need to compensate for any collapsed rows above this one.
			tempCell.h = 0;
			r = (*hAL)->dataBounds;
			for ( tempCell.v = (*hAL)->visCells.top; tempCell.v < theCell->v; tempCell.v++ ) {
				superRowOffset = _ALCalcOffsetFromCell( &tempCell, &r );

				if ( BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSsuperrow ) &&
							!BTST( (*(*hAL)->hSelected)[ superRowOffset ], alSexpanded ) ) {
					// Found a collapsed superRow.  Skip all cells underneath it.
					for ( tempCell.v++; tempCell.v <= theCell->v; tempCell.v++ ) {
						offset = _ALCalcOffsetFromCell( &tempCell, &r );
						if ( (*(*hAL)->hLevels)[ offset ] > (*(*hAL)->hLevels)[ superRowOffset ] )
							skippedCells++;
						else {
							tempCell.v--;
							break;
						}
					}
				} // found collapsed superRow
			} // checking for skipped cells.
		} // heirarchical list
#endif

		cellSize = (*hAL)->cellSize;
		r = (*hAL)->visCells;
		cellRect->top = cellSize.v * (theCell->v - r.top - skippedCells);
		cellRect->bottom = cellRect->top + cellSize.v;

		cellRect->left = cellSize.h * (theCell->h - r.left);
		cellRect->right = cellRect->left + cellSize.h;
	}

	// Put it into the dispRect.
	OffsetRect(cellRect, (*hAL)->dispRect.left, (*hAL)->dispRect.top);

	if ( !includeHeirarchical ) {
#if ALIST_HEIRARCHICAL
		// Leave room for the triangle on the left if we're in a heirarchical list.
		if (BTST((*hAL)->features, alFHeirarchical) && theCell->h == (*hAL)->dataBounds.left) {
			offset = _ALCalcOffsetFromCell( theCell, &(*hAL)->dataBounds );
			cellRect->left += 14 * (*(*hAL)->hLevels)[ offset ] + 14;
		}
#endif
	}
} // _ALCalcCellRect

void	_ALDrawCaret(ALCellPtr caretLoc, short caretPos, ALHandle hAL)
{	Rect				cellRect;
	GWorldPtr			saveWorld;
	GDHandle			saveDevice;
	RgnHandle			saveClip;
	QDDrawingState	saveState;

	// Calculate where this cell is.
	_ALCalcCellRect(&cellRect, caretLoc, true, hAL);

	// Calculate the caret rectangle.
	switch (caretPos) {
	case kCaretTop:
		cellRect.bottom = cellRect.top + 1;
		cellRect.left += kAL_CellMargin;
		cellRect.right -= kAL_CellMargin;
		break;
	case kCaretBottom:
		cellRect.bottom++;
		cellRect.top = cellRect.bottom - 1;
		cellRect.left += kAL_CellMargin;
		cellRect.right -= kAL_CellMargin;
		break;
	case kCaretWholeCell:
		InsetRect(&cellRect, kAL_CellMargin, kAL_CellMargin);
		break;
	case kCaretLeft	:
		cellRect.right = cellRect.left + 1;
		cellRect.top += kAL_CellMargin;
		cellRect.bottom -= kAL_CellMargin;
		break;
	case kCaretRight:
		cellRect.right++;
		cellRect.left = cellRect.right - 1;
		cellRect.top += kAL_CellMargin;
		cellRect.bottom -= kAL_CellMargin;
		break;
	default:
		cellRect.left = cellRect.right = cellRect.top = cellRect.bottom = 0;
		break;
	}

	// set up the port
	GetGWorld(&saveWorld, &saveDevice);
	SetPortWindowPort((*hAL)->winRef);

	// Save the background color and alternate it to the notepad background
	SaveQDDrawingState( &saveState, false );
#if ALIST_USEAPPEARANCEMGR
	if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL))
		SetThemeBackground( kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
	else
#endif
		if (ALFeatureFlag(alFNotepadBackground, alBitTest, hAL))
		_ALSetNotepadBackgroundColor( );

	// clip to the view region
	saveClip = NewRgn();
	GetClip(saveClip);
	SetClip((*hAL)->viewRgn);

	// draw the caret
	local_ALClearHiliteBit();
	InvertRect(&cellRect);

	RestoreQDDrawingState( &saveState, true );

	// restore the clip region
	SetClip(saveClip);
	DisposeRgn(saveClip);

	// restore the port
	SetGWorld(saveWorld, saveDevice);
} // _ALDrawCaret

#if ALIST_HEIRARCHICAL

void _ALDrawDisclosureTriangle( const Rect *cellRect, Boolean active, short triangleState, ALHandle hAL )
{	Rect		box;
	RGBColor	saveColor, tempColor;

	GetForeColor(&saveColor);

	box = *cellRect;
	box.right = box.left - 2;
	box.left -= 12;
	box.top = (box.bottom + box.top) / 2 - 6;
	box.bottom = box.top + 12;

#if ALIST_USEAPPEARANCEMGR && TARGET_RT_MAC_CFM
	if ( (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0) ) {
		long	result;

		// Need Appearance Manager 1.1 or later for this code. ( CFM only - not straight 680x0 ).
		if ( Gestalt( gestaltAppearanceVersion, &result ) == noErr && LoWord( result ) >= 0x0110 ) {
			ThemeButtonDrawInfo	drawInfo;
			if ( active )
				drawInfo.state = (triangleState == triPressedRight || triangleState == triPressedDown) ? kThemeStatePressed : kThemeStateActive;
			else
				drawInfo.state = kThemeStateInactive;
			if ( triangleState == triEnabledRight || triangleState == triPressedRight )
				drawInfo.value = 0;
			else
				drawInfo.value = 1;
			drawInfo.adornment = kThemeAdornmentDrawIndicatorOnly;
			DrawThemeButton( &box, kThemeDisclosureButton, &drawInfo, nil, nil, nil, 0 );
			return;
		}
	}
#else
	#pragma unused( hAL )
#endif

	// Now draw the triangle outline.

	if (!active) {
		tempColor.red = tempColor.green = tempColor.blue = color5;
		RGBForeColor(&tempColor);
	}

	if (triangleState == triEnabledRight || triangleState == triPressedRight) {
		MoveTo(box.left + 1, box.top);
		Line(0, 10);
		Line(5, -5);
		Line(-4, -4);
	} else if (triangleState == triPressedDown || triangleState == triEnabledDown) {
		MoveTo(box.left, box.top + 2);
		Line(5, 5);
		Line(5, -5);
		Line(-10, 0);
	}

	if (active) {
	// Draw some shading.
		if (triangleState == triEnabledRight) {
			tempColor.red = tempColor.green = tempColor.blue = color7;
			RGBForeColor(&tempColor);
			MoveTo(box.left + 2, box.bottom - 2);
			Line(4, -4);

			tempColor.red = tempColor.green = tempColor.blue = color4;
			Move(1, 0);
			Line(-5, 5);
		} else if (triangleState == triEnabledDown) {
			tempColor.red = tempColor.green = tempColor.blue = color7;
			RGBForeColor(&tempColor);
			MoveTo(box.right, box.top + 3);
			Line(-4, 4);

			tempColor.red = tempColor.green = tempColor.blue = color4;
			Move(0, 1);
			Line(5, -5);
		}
	}

	// Fill in the triangle.
	if (active) {
		tempColor.red = tempColor.green = 0;
		if ( triangleState == triEnabledRight || triangleState == triEnabledDown ) {
			tempColor.red = tempColor.green = 30000;
			tempColor.blue = 65000;
		} else
			tempColor.blue = 30000;
	} else
		tempColor.red = tempColor.green = tempColor.blue = color5;

	RGBForeColor(&tempColor);

	if ( triangleState == triPressedRight || triangleState == triEnabledRight ) {
		MoveTo(box.left + 2, box.top + 2);
		Line(0, 6);
		Line(3, -3);
		Line(-2, -2);
		Line(0, 3);
		Line(1, -1);
	} else if ( triangleState == triPressedDown || triangleState == triEnabledDown ) {
		MoveTo(box.left + 2, box.top + 3);
		Line(3, 3);
		Line(3, -3);
		Line(-5, 0);
		Line(2, 2);
		Line(1, -1);
		Line(-1, 0);
	}

	RGBForeColor(&saveColor);
}
#endif

static void local_ALDrawCells( Boolean doErase, ALHandle hAL )
{	// Find out which cells need to be redrawn and draw them.
	// If doErase is TRUE, erase each rectangle before redrawing.
	ALPtr			pAL = *hAL;				// assume AL record is locked
#if ALIST_HAVE_CELLDATA
	ALDataPtr			pData;					// points to the list data
#endif
	Rect				bounds;					// bounds of the offscreen buffer, in global coordinates
	Boolean			usingColor;				// true if we're drawing in color
	Boolean			usingOffscreen;				// true if we're using an offscreen port
	Boolean			drawingOffscreen;			// true if actually drawing to an offscreen buffer
	GDHandle			saveDevice;
	GWorldPtr			saveGWorld;
	ALCell			theCell;
	QDDrawingState	saveDrawingState;
	PixMapHandle		offscreenPixels, screenPixMap;
	Rect				cellRect, drawRect, box;
	RGBColor			theColorBlack = { 0, 0, 0 }, theColorWhite = { 65535, 65535, 65535 };
	unsigned long		offset;
	RgnHandle			tempRgn;

	// Do nothing if our graphics port is not visible.
	tempRgn = NewRgn( );
	GetPortVisibleRegion( GetWindowPort( pAL->winRef ), tempRgn );
	if ( EmptyRgn( tempRgn ) ) {
		DisposeRgn( tempRgn );
		return;
	}
	DisposeRgn( tempRgn );

	usingOffscreen = false;
	drawingOffscreen = false;
	usingColor = (BTST(pAL->flags, alFHasColorQD) && !BTST(pAL->features, alFInhibitColor)) ? true : false;

	// save graphics world
	GetGWorld( &saveGWorld, &saveDevice );

	// If doErase is true, we're drawing over old cells, so we must erase each cell
	// before redrawing it.  But if the alFDrawOffscreen feature is enabled, we draw
	// the entire cell offscreen and we copy the image right over the old cell,
	// without erasing it, thus achieving a very smooth drawing effect.

	if ( ( doErase ) && BTST( pAL->features, alFDrawOffscreen ) ) {
		// has an offscreen world already been allocated?
		if (pAL->offscreenPort == nil) {
			GWorldPtr	aGWorld;
			// Nope,  create one; its bounds are set initially to an arbitrary rectangle
			SetRect(&bounds, 0, 0, 1, 1);
			if (NewGWorld(&aGWorld, 0, &bounds, nil, nil, pixPurge + noNewDevice + useTempMem) != noErr)
				aGWorld = nil;
			pAL->offscreenPort = aGWorld;
		}
		usingOffscreen = (pAL->offscreenPort != nil);
	}

	// set up the port
	SetPortWindowPort( pAL->winRef );
	// save the QuickDraw environment
	SaveQDDrawingState( &saveDrawingState, true );

#if ALIST_HAVE_CELLDATA
	HLock((Handle)pAL->hData);
	pData = *pAL->hData;
#endif

	for (theCell.h = pAL->visCells.left; theCell.h < pAL->visCells.right; theCell.h++) {
		for (theCell.v = pAL->visCells.top; theCell.v < pAL->visCells.bottom; theCell.v++) {

			// Skip over any cells that aren't visible.
			if ( !ALIsVisible( &theCell, hAL ) )
				continue;

			// Calculate the cell rectangle.
			_ALCalcCellRect(&cellRect, &theCell, true, hAL);

			// Calculate the visible portion of this rectangle.
			// We do this by intersecting the line rectangle with the view rectangle.
			drawRect = pAL->dispRect;
			if (SectRect( &cellRect, &drawRect, &drawRect ) ) { 
				if (usingOffscreen) {
					// calculate the boundary rectangle for the offscreen buffer
					// this is simply drawRect converted to global coordinates
					bounds = drawRect;
					LocalToGlobal((Point *) &bounds.top);
					LocalToGlobal((Point *) &bounds.bottom);

					// update the offscreen graphics world for the new bounds (this could fail)
					drawingOffscreen = false;
					if (UpdateGWorld(&pAL->offscreenPort, 0, &bounds, (CTabHandle)nil,
									(GDHandle)nil, (GWorldFlags)0) >= 0) {
						// NOTE: when running on a 68000 machine with the original Quickdraw,
						// a GWorld is just an extended GrafPort, and GetGWorldPixMap actually
						// returns a handle to a _copy_ of the GrafPort portBits (a BitMap, not a PixMap).
						// An important side-effect of this is that when we call SetOrigin,
						// only the original portBits is offset, not the copy.
						// get the pixel map associated with the offscreen graphics world
						offscreenPixels = GetGWorldPixMap(pAL->offscreenPort);

						// lock it down
						if ( LockPixels( offscreenPixels ) ) {
							// offscreen pixel buffer allocation was successful
							drawingOffscreen = true;
							// switch graphics world
							SetGWorld(pAL->offscreenPort, nil);
							// synchronize the coordinate system of the offscreen port with that of the screen port
							SetOrigin(drawRect.left, drawRect.top);
							// reset the offscreen clip region
							ClipRect(&drawRect);
						}
					} // if pixel buffer allocation was successful
				} // if usingOffscreen

				// If doErase is true, erase the drawable area before drawing text.
				if ( doErase )
					CallALDrawBackgroundProc( &cellRect, hAL, pAL->drawBackgroundHook );

				_ALCalcCellRect( &cellRect, &theCell, false, hAL );

				// If drawingOffscreen, synchronize text attributes.
				if ( drawingOffscreen ) {
					TextFont( saveDrawingState.textStyle.tsFont );
					TextFace( saveDrawingState.textStyle.tsFace );
					TextSize( saveDrawingState.textStyle.tsSize );
				} // if drawingOffscreen

				// Set the foreground color.
				if (usingColor)
					RGBForeColor( &saveDrawingState.textStyle.tsColor );

				offset = _ALCalcOffsetFromCell(&theCell, &pAL->dataBounds);

				// draw the cell
				if (pAL->drawCellHook != nil)
#if ALIST_HAVE_CELLDATA
					CallALDrawCellProc( pData[offset], &theCell, &cellRect, hAL, pAL->drawCellHook );
#else
					CallALDrawCellProc( nil, &theCell, &cellRect, hAL, pAL->drawCellHook );
#endif

#if ALIST_HEIRARCHICAL
				// Draw the heirarchical disclosure triangles.
				if (BTST(pAL->features, alFHeirarchical) && theCell.h == pAL->dataBounds.left &&
						BTST((*pAL->hSelected)[offset], alSsuperrow))
					_ALDrawDisclosureTriangle( &cellRect, BTST(pAL->flags, alFActive) != 0,
								(BTST((*pAL->hSelected)[offset], alSexpanded)) ? triEnabledDown : triEnabledRight, hAL );
#endif

				if (BTST(pAL->features, alFDrawLines)) {
					if ( BTST( pAL->features, alFRowsOnly ) ) {
						MoveTo( cellRect.left, cellRect.bottom - 1 );
						LineTo( cellRect.right, cellRect.bottom - 1 );
					} else if ( BTST( pAL->features, alFColumnsOnly ) ) {
						MoveTo( cellRect.right - 1, cellRect.top );
						LineTo( cellRect.right - 1, cellRect.bottom );
					} else {
						box = cellRect;
						// Make bottom and top lines intersect.  Also right and left.
						box.bottom += 1;
						box.right += 1;
						FrameRect(&box);
					}
				}

				if ( drawingOffscreen ) {
					// after drawing offscreen the last segment,
					// prepare to copy the offscreen buffer to video RAM

					// first set the graphics world to the screen port
					SetGWorld( saveGWorld, saveDevice);

					// Before calling CopyBits, set the foreground color to black and the background color to white
					// to avoid colorization (color only).
					if ( usingColor ) {
						RGBForeColor( &theColorBlack );
						RGBBackColor( &theColorWhite );
					}

					screenPixMap = GetPortPixMap( saveGWorld );

					// copy the offscreen image of the [visible portion of the] line to the screen
					CopyBits( (BitMapPtr)*offscreenPixels, (BitMapPtr)*screenPixMap,
							&drawRect, &drawRect, srcCopy, (RgnHandle)nil );

					// restore the original offscreen coordinate system and unlock the pixel image
					SetGWorld( pAL->offscreenPort, nil );
					SetOrigin(0, 0);
					if ( usingColor ) {
						RGBForeColor( &theColorBlack );	// this fixes a bug in Style 1.3
						RGBBackColor( &theColorWhite );
					}
					UnlockPixels(offscreenPixels);

					// restore the screen port
					SetGWorld( saveGWorld, saveDevice );
				} // if drawingOffscreen
			} // needs to be drawn
		} // looping over columns
	} // looping over rows

#if ALIST_HAVE_CELLDATA
	HUnlock( (Handle)pAL->hData );
#endif

	// Restore the QuickDraw environment
	RestoreQDDrawingState( &saveDrawingState, true );

	// restore graphics world
	SetGWorld( saveGWorld, saveDevice );
} // local_ALDrawCells

void _ALDrawListBorder(ALHandle hAL)
{	Rect		box;
	RGBColor	saveColor, tempColor;
	Boolean	focused, appearance = false;
	OSStatus	status = noErr;

	if (BTST((*hAL)->features, alFInhibitRedraw))
	  return;

	// Get the rectangle to draw.
	ALGetViewRect(&box, hAL);
	--box.top;
	box.right++;
	// InsetRect(&box, 1, 1);

	// Check the focused state.  Also, check for the Appearance Manager for a bit of speed.
	focused = (BTST((*hAL)->flags, alFFocused) != 0);
#if ALIST_USEAPPEARANCEMGR
	appearance = (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0);
#endif

	// If we're not focused or not active, erase the focus rectangle first.
	if ( 0 && BTST((*hAL)->features, alFDrawFocus) && ( !focused || !BTST((*hAL)->flags, alFActive) ) ) {
#if ALIST_USEAPPEARANCEMGR
	  if (appearance) {
			status = DrawThemeFocusRect(&box, false);
	  } else
#endif
		{	GetForeColor(&saveColor);
			// Erase to the background color.
			GetBackColor(&tempColor);
			RGBForeColor(&tempColor);
			PenSize(2, 2);
			MoveTo(box.left - 2, box.top - 3);
			LineTo(box.right, box.top - 3);
			Line(0, 1);
			Line(1, 0);
			LineTo(box.right + 1, box.bottom);
			MoveTo(box.left - 3, box.top - 2);
			LineTo(box.left - 3, box.bottom);
			Line(1, 0);
			Line(0, 1);
			LineTo(box.right, box.bottom + 1);

			// Reset the pen size to 1,1 and the foreground color to whatever it was.
			PenSize(1, 1);
			RGBForeColor(&saveColor);
		}
	}

	// Now draw the rectangle around the list.
	if (BTST((*hAL)->features, alFDrawRect)) {
		// Want to draw the rectangle around the list.
#if ALIST_USEAPPEARANCEMGR
		if ( appearance && status == noErr )
			status = DrawThemeListBoxFrame(&box, (BTST((*hAL)->flags, alFActive)) ? kThemeStateActive : kThemeStateInactive);
		else
#endif
		{	InsetRect(&box, -1, -1);
			FrameRect(&box);
			InsetRect(&box, 1, 1);
		}
	}

	// Lastly, draw the focus rectangle if we are focused and active.
	if ( BTST((*hAL)->features, alFDrawFocus) && focused && BTST((*hAL)->flags, alFActive) ) {
#if ALIST_USEAPPEARANCEMGR
	  if ( appearance && status == noErr ) {
			status = DrawThemeFocusRect( &box, true );
	  } else
#endif
		{	GetForeColor( &saveColor );
			// use color 8 from Apple's Grayscale Human Interface document
			tempColor.red = tempColor.green = tempColor.blue = 30583;
			RGBForeColor(&tempColor);
			PenSize(2, 2);
			MoveTo(box.left - 2, box.top - 3);
			LineTo(box.right, box.top - 3);
			Line(0, 1);
			Line(1, 0);
			LineTo(box.right + 1, box.bottom);
			MoveTo(box.left - 3, box.top - 2);
			LineTo(box.left - 3, box.bottom);
			Line(1, 0);
			Line(0, 1);
			LineTo(box.right, box.bottom + 1);

			// Reset the pen size to 1,1 and the foreground color to whatever it was.
			PenSize(1, 1);
			RGBForeColor(&saveColor);
		}
	}
} // _ALDrawListBorder

void _ALSetNotepadBackgroundColor(void)
{	RGBColor	notepadColor;

	// Make the background yellow.
	notepadColor.red = notepadColor.green = 65535;
	notepadColor.blue = 39321;
	RGBBackColor(&notepadColor);
}

#pragma mark -

ALIST_API void ALDrawCell(const ALCellPtr theCell, ALHandle hAL)
{	unsigned long		offset;
#if ALIST_HAVE_CELLDATA
	ALDataPtr			pData;
	Boolean			saveDataLock;
#endif
	Rect				entireCellRect, dataPortionCellRect;
	Rect				box;
	CGrafPtr			savePort;
	GDHandle saveDev;
	RgnHandle			saveClip;
	QDDrawingState	saveDrawingState;

	// Sanity check.
	if (theCell == nil || hAL == nil || *hAL == nil)
		return;

	if (BTST((*hAL)->features, alFInhibitRedraw)) {
	  refresh(hAL);
	  return;
	}

	if ( ALIsVisible( theCell, hAL ) ) {
		offset = _ALCalcOffsetFromCell(theCell, &(*hAL)->dataBounds);
		_ALCalcCellRect(&entireCellRect, theCell, true, hAL);
		_ALCalcCellRect(&dataPortionCellRect, theCell, false, hAL);

		GetGWorld(&savePort, &saveDev);
		SetPortWindowPort((*hAL)->winRef);

		// save the clip region
		saveClip = NewRgn();
		GetClip(saveClip);

#if ALIST_HAVE_CELLDATA
		saveDataLock = _ALSetHandleLock((Handle)(*hAL)->hData, true);
		pData = *(*hAL)->hData;
#endif

		// Clip it to the actual visible portion of the cell.
		SectRect(&entireCellRect, &(*hAL)->dispRect, &box);
		ClipRect(&box);

		// save the Quickdraw environment
		SaveQDDrawingState( &saveDrawingState, true );

		// Draw the background.
		if ((*hAL)->drawBackgroundHook != nil)
			CallALDrawBackgroundProc(&entireCellRect, hAL, (*hAL)->drawBackgroundHook);

		// Now get the cell indented for heirarchical lists; Clip it to the actual visible portion of the cell.
		SectRect( &dataPortionCellRect, &(*hAL)->dispRect, &box );
		ClipRect( &box );

		// Draw the cell contents.
		if ((*hAL)->drawCellHook != nil)
#if ALIST_HAVE_CELLDATA
			CallALDrawCellProc(pData[offset], theCell, &dataPortionCellRect, hAL, (*hAL)->drawCellHook);
#else
			CallALDrawCellProc( nil, theCell, &dataPortionCellRect, hAL, (*hAL)->drawCellHook );
#endif

		// Hilite the cell if appropriate.
		if (BTST((*(*hAL)->hSelected)[offset], alSselected))
			if ((*hAL)->hiliteCellHook != nil)
#if ALIST_HAVE_CELLDATA
				CallALHiliteCellProc(pData[offset], theCell, BTST((*hAL)->flags, alFActive) ? true : false,
								BTST((*hAL)->features, alFOutlineHilite) ? true : false,
								&dataPortionCellRect, hAL, (*hAL)->hiliteCellHook);
#else
				CallALHiliteCellProc( nil, theCell, BTST((*hAL)->flags, alFActive) ? true : false,
								BTST((*hAL)->features, alFOutlineHilite) ? true : false,
								&dataPortionCellRect, hAL, (*hAL)->hiliteCellHook);
#endif

#if ALIST_HEIRARCHICAL
		// Draw the heirarchical disclosure triangles.
		if (BTST((*hAL)->features, alFHeirarchical) && theCell->h == (*hAL)->dataBounds.left &&
				BTST((*(*hAL)->hSelected)[offset], alSsuperrow)) {
			SetClip(saveClip);

			_ALDrawDisclosureTriangle(&dataPortionCellRect, BTST((*hAL)->flags, alFActive) != 0,
						(BTST((*(*hAL)->hSelected)[offset], alSexpanded)) ? triEnabledDown : triEnabledRight, hAL );

			ClipRect(&box);
		}
#endif

#if ALIST_HAVE_CELLDATA
		_ALSetHandleLock((Handle)(*hAL)->hData, saveDataLock);
#endif

		if (BTST((*hAL)->features, alFDrawLines)) {
			if ( BTST( (*hAL)->features, alFRowsOnly) ) {
				MoveTo( box.left, box.bottom - 1 );
				LineTo( box.right, box.bottom - 1 );
			} else if ( BTST( (*hAL)->features, alFColumnsOnly ) ) {
				MoveTo( box.right - 1, box.top );
				LineTo( box.right - 1, box.bottom );
			} else {
				box.right++;
				box.bottom++;
				FrameRect(&box);
			}
		}

		// restore the Quickdraw environment
		RestoreQDDrawingState( &saveDrawingState, true );

		// Reset the clip and the port.
		SetClip(saveClip);
		DisposeRgn(saveClip);

		SetGWorld( savePort, saveDev );
	}
} // ALDrawCell

ALIST_API void ALUpdate( RgnHandle inUpdateRgn, ALHandle hAL )
{	ALPtr	pAL;
	Rect		box, lastCellRect, updateRect;
	RgnHandle	saveClip, auxRgn;
	GWorldPtr	saveWorld;
	GDHandle	saveDevice;
	Boolean	saveALLock, doErase;
	ALCell	aCell;
	QDDrawingState	saveState, secondSaveState;

	// Sanity check.
	if (hAL == nil || *hAL == nil)
		return;

	if (BTST((*hAL)->features, alFInhibitRedraw)) {
	  refresh(hAL);
	  return;
	}

	// lock the AL record
	saveALLock = _ALSetHandleLock((Handle) hAL, true);
	pAL = *hAL;

	// set up the port
	GetGWorld(&saveWorld, &saveDevice);
	SetPortWindowPort(pAL->winRef);

	SaveQDDrawingState( &saveState, true );

	// save the clip region
	saveClip = NewRgn();
	GetClip( saveClip );

	// Clip to the insersection between inUpdateRgn and the view rectangle.
	// (inUpdateRgn may be nil; in this case, just clip to the view rectangle)
	auxRgn = NewRgn( );
	if ( inUpdateRgn != nil )
		SectRgn( inUpdateRgn, pAL->viewRgn, auxRgn );
	else
		CopyRgn( pAL->viewRgn, auxRgn );

	SetClip( auxRgn );

	if (!EmptyRgn(auxRgn)) {
		// calculate the rectangle to update
		GetRegionBounds( auxRgn, &box );
		updateRect = box;

		// Find out which cells need to be redrawn and draw them
		// if inUpdateRgn is nil, erase each rectangle before redrawing
		if ( inUpdateRgn == nil || BSET(pAL->flags, alFComingActive) ) {
			doErase = true;
			BCLR(pAL->flags, alFComingActive);
		} else
			doErase = false;
		local_ALDrawCells(doErase, hAL);

		// Erase the portion of the update rectangle below the last line (if any).
		// First, calculate the bottom right most cell's rectangle.
		aCell.h = pAL->visCells.right - 1;
		aCell.v = pAL->visCells.bottom - 1;
		_ALCalcCellRect(&lastCellRect, &aCell, false, hAL);

		// The rectangle to erase starts at the bottom of the last cell visible.
		updateRect.top = lastCellRect.bottom;
		if (BTST(pAL->features, alFDrawLines) && !BTST( pAL->features, alFRowsOnly ) )
			updateRect.top++;	// Leave a pixel for the boundary.

		if ( updateRect.top < updateRect.bottom ) {
			SaveQDDrawingState( &secondSaveState, false );
			box = updateRect;
#if ALIST_USEAPPEARANCEMGR
			if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL)) {
				SetThemePen(kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
				PaintRect(&box);
			} else
#endif
				if ( ALFeatureFlag( alFNotepadBackground, alBitTest, hAL ) ) {
				_ALSetNotepadBackgroundColor( );
				EraseRect( &box );
			} else
				EraseRect(&box);

			RestoreQDDrawingState( &secondSaveState, true );
		}

		// Erase the portion of the update rectangle to the right of the last column (if any).
		updateRect = box;
		// The rectangle to erase starts at the right of the last cell visible.
		updateRect.left = lastCellRect.right;
		if (BTST(pAL->features, alFDrawLines) && !BTST( pAL->features, alFRowsOnly ) )
			updateRect.left++;	// Leave a pixel for the boundary.

		if (updateRect.left < updateRect.right) {
			SaveQDDrawingState( &secondSaveState, false );

			box = updateRect;
#if ALIST_USEAPPEARANCEMGR
			if (BTST((*hAL)->flags, alFHasAppearanceMgr) != 0 && ALFeatureFlag(alFAppearanceBackground, alBitTest, hAL)) {
				SetThemePen(kThemeBrushListViewBackground, (**(**saveDevice).gdPMap).pixelSize, (**(**saveDevice).gdPMap).pixelSize > 1);
				PaintRect(&box);
			} else
#endif
				if ( ALFeatureFlag( alFNotepadBackground, alBitTest, hAL ) ) {
				_ALSetNotepadBackgroundColor( );
				EraseRect( &box );
			} else
				EraseRect(&box);

			RestoreQDDrawingState( &secondSaveState, true );
		}

		// Hilite the selection range
		_ALHiliteSelected(hAL);
	}

	// restore the clip region
	SetClip( saveClip );

	// Check to see if we should draw the border.
	if ( inUpdateRgn != nil ) {
		// Build a region that contains only the list border.
		ALGetViewRect( &box, hAL );
		InsetRect( &box, -1, -1 );
		RectRgn( auxRgn, &box );
		InsetRect( &box, 2, 2 );
		RectRgn( saveClip, &box );
		DiffRgn( auxRgn, saveClip, auxRgn );

		// Test if there is any intersection between the inUpdateRgn and the list border.
		SectRgn( auxRgn, inUpdateRgn, auxRgn );
	} else
		SetEmptyRgn( auxRgn );

	RestoreQDDrawingState( &saveState, true );

	DisposeRgn(saveClip);

	if ( inUpdateRgn == nil || !EmptyRgn( auxRgn ) )
		_ALDrawListBorder( hAL );

	DisposeRgn( auxRgn );

#if 0
	if (pAL->vScroll != nil)
		DrawOneControl(pAL->vScroll);

	if (pAL->hScroll != nil)
		DrawOneControl(pAL->hScroll);
#endif

	// restore the port
	SetGWorld(saveWorld, saveDevice);

	// unlock the AL record
	_ALSetHandleLock((Handle) hAL, saveALLock);
} // ALUpdate
