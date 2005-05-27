/*
 *	ALKeyboard.c
 *
 *	Part of The A List.
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
*/
#include <ctype.h>
#include "AListInternal.h"
#include "LongControls.h"

#if defined( TARGET_API_MAC_CARBON )
	#if !defined( __CONTROLDEFINITIONS__ )
		#include <ControlDefinitions.h>
	#endif
#endif
#if defined(UNIVERSAL_INTERFACES_VERSION) && (UNIVERSAL_INTERFACES_VERSION > 0x0300)
	#if !defined( __TEXTUTILS__ )
		#include <TextUtils.h>	// For NewString
	#endif
#endif

// This performs a case-insensitive string compare.
int _ALStrNCmp( const unsigned char * str1, const unsigned char * str2, int len )
{	register int	i;

	for ( i = 0; i < len && toupper( str1[ i ] )  == toupper( str2[ i ] ) ; i++ )	;

	if ( i == len )
		return 0;
	else if ( toupper( str1[ i ] ) > toupper( str2[ i ] ) )
		return 1;
	else
		return -1;
}

#pragma mark -

ALIST_API void ALKey( SInt16 inCharCode,  EventModifiers inModifiers, unsigned long inKeyTime, ALHandle hAL )
{	ALCell	theCell;
	Boolean	someSelected, selectOne, selectAlso, makeVisible, inhibitRedraw;
#if ALIST_HEIRARCHICAL
	Boolean		listIsHeirarchical;
#endif
	LongRect	bounds;
	Str255	text;
	Point		moveTo;

	if (hAL == nil || *hAL == nil)
		return;

#if ALIST_HEIRARCHICAL
	listIsHeirarchical = BTST( (*hAL)->features, alFHeirarchical );
#endif

	bounds = (**hAL).dataBounds;
	theCell.h = bounds.left;
	theCell.v = bounds.top;
 	selectOne = false;
	selectAlso = false;
	makeVisible = false;
	someSelected = ALGetSelect( true, &theCell, hAL );

	switch ( inCharCode ) {
	case kArrowUp:
		if ( someSelected ) {
			theCell.v--;
#if ALIST_HEIRARCHICAL
			if ( listIsHeirarchical && ALIsRowHidden( theCell.v + 1, hAL ) ) {
				// Previous selection was hidden.  Select the bottom most non-hidden cell.
				theCell.v = bounds.bottom - 1;
				while ( ALIsRowHidden( theCell.v, hAL ) && theCell.v >= bounds.top )
					theCell.v--;
				selectOne = true;
				makeVisible = true;
			} else if ( listIsHeirarchical ) {
				// Make sure that the new cell is not hidden.
				while ( ALIsRowHidden( theCell.v, hAL ) && theCell.v >= bounds.top )
					theCell.v--;
			} // end of heirarchical list.
#endif

			if ( theCell.v >= bounds.top ) {
				if ((inModifiers & shiftKey) && !BTST((*hAL)->features, alFSelOnlyOne))
					selectAlso = true;
				else
					selectOne = true;

				makeVisible = true;
			}
		} else {
			if ( bounds.top != bounds.bottom ) {
				// Select the bottom cell.
				theCell.v = bounds.bottom - 1;
#if ALIST_HEIRARCHICAL
				if ( listIsHeirarchical ) {
					// Make sure that the cell is not hidden.
					while ( ALIsRowHidden( theCell.v, hAL ) && theCell.v >= bounds.top )
						theCell.v--;
				}
#endif
				selectOne = true;

				makeVisible = true;
			}
		}
		break;
	case kArrowDown:
		if ( someSelected ) {
			theCell.v++;
#if ALIST_HEIRARCHICAL
			if ( listIsHeirarchical && ALIsRowHidden( theCell.v - 1, hAL ) ) {
				// Previous selection was hidden.  Select the top cell.
				theCell.v = bounds.top;
				selectOne = true;
				makeVisible = true;
			} else if ( listIsHeirarchical ) {
				// Make sure that the cell is not hidden.
				while ( ALIsRowHidden( theCell.v, hAL ) && theCell.v < bounds.bottom )
					theCell.v++;
			}
#endif

			if ( theCell.v < bounds.bottom ) {
				if ( (inModifiers & shiftKey) && !BTST((*hAL)->features, alFSelOnlyOne) ) {
					// Need to extend the selection.  Find the bottom most selected cell in this column.
					while ( ALGetSelect(false, &theCell, hAL) && theCell.v + 1 < bounds.bottom )
						theCell.v++;
					selectAlso = true;
				} else
					selectOne = true;

				makeVisible = true;
			}
		} else {
			if ( bounds.top != bounds.bottom ) {
				// Select the top cell.
				// By definition - the top cell cannot be hidden in a heirarchical list.
				selectOne = true;

				makeVisible = true;
			}
		}
		break;
	case kArrowLeft:
		if ( someSelected ) {
			theCell.h--;
#if ALIST_HEIRARCHICAL
			// Make sure that the cell is not hidden.
			if ( listIsHeirarchical && ALIsRowHidden( theCell.v, hAL ) ) {
				// We had a selection in a hidden portion of the list.
				// Treat it as if nothing was selected and then select the top right cell.
				theCell.v = bounds.top;
				theCell.h = bounds.right - 1;
				selectOne = true;
				makeVisible = true;
			} else
				// NOTE: This 'else' makes sure that we don't try to extend the selection when
				// the previous selection was hidden.
#endif

			if (theCell.h >= bounds.left) {
				if ((inModifiers & shiftKey) && !BTST((*hAL)->features, alFSelOnlyOne))
					selectAlso = true;
				else
					selectOne = true;

				makeVisible = true;
			}
		} else {
			if ( bounds.left != bounds.right ) {
				// Select the right cell of the top-most row.
				// By definition - the top cell cannot be hidden in a heirarchical list.
				theCell.h = bounds.right - 1;

				selectOne = true;

				makeVisible = true;
			}
		}
		break;
	case kArrowRight:
		if ( someSelected ) {
			theCell.h++;

#if ALIST_HEIRARCHICAL
			// Make sure that the cell is not hidden.
			if ( listIsHeirarchical && ALIsRowHidden( theCell.v, hAL ) ) {
				// We had a selection in a hidden portion of the list.
				// Treat it as if nothing was selected and then select the top right cell.
				theCell.v = bounds.top;
				theCell.h = bounds.left;
				selectOne = true;
				makeVisible = true;
			} else
				// NOTE: This 'else' makes sure that we don't try to extend the selection when
				// the previous selection was hidden.
#endif

			if (theCell.h < bounds.right) {
				if ((inModifiers & shiftKey) && !BTST((*hAL)->features, alFSelOnlyOne)) {
					// Need to extend the selection.  Find the right-most selected cell in this row.
					while (ALGetSelect(false, &theCell, hAL) && theCell.h + 1 < bounds.right)
						theCell.h++;
					selectAlso = true;
				} else
					selectOne = true;

				makeVisible = true;
			}
		} else {
			if ( bounds.left != bounds.right ) {
				// Select the left cell of the top-most row.
				// By definition - the top cell cannot be hidden in a heirarchical list.
				selectOne = true;

				makeVisible = true;
			}
		}
		break;
	}

	if ( makeVisible && _ALCheckInsideBounds( &theCell, &bounds ) && !ALIsVisible( &theCell, hAL ) ) {
		// Make sure theCell is visible.
		moveTo.v = ( (**hAL).dispRect.bottom + (**hAL).dispRect.top ) / 2;
		moveTo.h = ( (**hAL).dispRect.right + (**hAL).dispRect.left ) / 2;
		(void)ALAutoScroll( moveTo, &theCell, hAL );
	}

	if ( inCharCode == kHomeCharCode ) {
/*		If you want the Home key to move the selection, uncomment this.
		theCell.h = bounds.left;
		theCell.v = bounds.top;
		selectOne = true;
*/
		// do the scroll bars
		if ((**hAL).vScroll != nil)
			ALScrollCells(0, LCGetValue((**hAL).vScroll) - LCGetMinimum((**hAL).vScroll), hAL);
		if ((**hAL).hScroll != nil)
			ALScrollCells(LCGetValue((**hAL).hScroll) - LCGetMinimum((**hAL).hScroll), 0, hAL);
	} else if ( inCharCode == kEndCharCode ) {
/*		If you want the End key to move the selection, uncomment this.
		theCell.h = bounds.right - 1;
		theCell.v = bounds.bottom - 1;
		selectOne = true;
*/

		// do the scroll bars
		if ((**hAL).vScroll != nil)
			ALScrollCells(0, LCGetValue((**hAL).vScroll) - LCGetMaximum((**hAL).vScroll), hAL);
		if ((**hAL).hScroll != nil)
			ALScrollCells(LCGetValue((**hAL).hScroll) - LCGetMaximum((**hAL).hScroll), 0, hAL);
	} else if ( inCharCode == kPageUpCharCode ) {
/*		If you want the Page Up key to move the selection, uncomment this.
		theCell.v -= (*hAL)->visCells.bottom - (*hAL)->visCells.top - 1;
		if (theCell.v < bounds.top)
			theCell.v = bounds.top;
		selectOne = true;
*/
		// do the scroll bar
		if ((**hAL).vScroll != nil)
			_ALVertScrollActionProc((**hAL).vScroll, kControlPageUpPart);
	} else if ( inCharCode == kPageDownCharCode ) {
/*		If you want the Page Down key to move the selection, uncomment this.
		theCell.v += (*hAL)->visCells.bottom - (*hAL)->visCells.top - 1;
		if (theCell.v >= bounds.bottom)
			theCell.v = bounds.bottom - 1;
		selectOne = true;
*/
		// do the scroll bar
		if ((**hAL).vScroll != nil)
			_ALVertScrollActionProc((**hAL).vScroll, kControlPageDownPart);
	}

	if ( isalnum( inCharCode ) && (*hAL)->stringSearchHook != nil ) {
		// Keep track of the alphanumeric characters coming in if the stringSearchHook is not nil.

		_ALSetHandleLock( (Handle)hAL, true );
		if ( inKeyTime < (*hAL)->keyTime + LMGetKeyThresh( ) && (*hAL)->keyString != nil  ) {
			// Add it to the current string if it's within LMGetKeyThresh of the last key press.
			BlockMoveData( *(*hAL)->keyString, text, (*(*hAL)->keyString)[0] + 1);
			// Don't let the string get longer than 255 characters.
			if ( text[ 0 ] < 255 )
				text[ ++text[ 0 ] ] = inCharCode;
		} else {
			// Otherwise, start a new string.
			text[ 0 ] = 1;
			text[ 1 ] = inCharCode;
		}

		// Put the text into a StringHandle.
		if ( (*hAL)->keyString != nil )
			_ALForgetHandle( (Handle *)&(*hAL)->keyString );
		(*hAL)->keyString = NewString( text );

		(*hAL)->keyTime = inKeyTime;
		_ALSetHandleLock( (Handle)hAL, false );

		// Now do the StringSearch callback function.
		selectOne = CallALStringSearchProc( (*hAL)->keyString, &theCell, hAL, (*hAL)->stringSearchHook );

#if ALIST_HEIRARCHICAL
		if ( listIsHeirarchical && selectOne )
			// Make sure that the cell is not hidden.
			selectOne = !ALIsRowHidden( theCell.v, hAL );
#endif

		if ( selectOne && !ALIsVisible( &theCell, hAL ) ) {
			// Make sure theCell is visible if we're selecting it.
			moveTo.v = ( (**hAL).dispRect.bottom + (**hAL).dispRect.top ) / 2;
			moveTo.h = ( (**hAL).dispRect.right + (**hAL).dispRect.left ) / 2;
			(void)ALAutoScroll( moveTo, &theCell, hAL);
		}
	}

	inhibitRedraw = BTST( (*hAL)->features, alFInhibitRedraw );
	BCLR( (*hAL)->features, alFInhibitRedraw );
	if ( selectOne && _ALCheckInsideBounds( &theCell, &bounds ) )
		_ALSelectOnlyOne( true, &theCell, hAL );
	else if ( selectAlso )
		ALSetSelect( true, &theCell, hAL );

	if ( inhibitRedraw )
		BSET( (*hAL)->features, alFInhibitRedraw );
}
