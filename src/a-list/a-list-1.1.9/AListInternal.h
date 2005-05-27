/*
 *	AListInternal.h
 *
 *	The A List
 *  Internal (private) interface
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
 *
 */

/*	MacOS Toolbox #includes */

#ifndef __APPEARANCE__
	#include <Appearance.h>
#endif
#ifndef __CONTROLS__
	#include <Controls.h>
#endif
#ifndef __CONTROL_DEFINITIONS__
#if defined( UNIVERSAL_INTERFACES_VERSION ) && ( UNIVERSAL_INTERFACES_VERSION >= 0x0330 )
	#include <ControlDefinitions.h>
#endif
#endif
#ifndef __DRAG__
	#include <Drag.h>
#endif
#ifndef __GESTALT__
	#include <Gestalt.h>
#endif
#ifndef __LOWMEM__
	#include <LowMem.h>
#endif
#ifndef __MIXEDMODE__
	#include <MixedMode.h>
#endif
#ifndef __QDOFFSCREEN__
	#include <QDOffscreen.h>
#endif

#include "AListOptimizations.h"

/*	ANSI #includes */

#ifndef __climits__
	#include <limits.h>
#endif

/*	other #includes */

#ifndef __LONGCOORDINATES__
#ifndef _LongCoords_
	#include "LongCoords.h"
#endif
#endif

/* determine if inline functions are supported */

#if defined(__cplusplus) || defined(__MWERKS__)
#define		INLINE	inline
#else
#define		INLINE
#endif

/* pascal-like macros for testing, setting, clearing and inverting bits */
#define BTST( FLAGS, BIT )	((FLAGS) & (1L << (BIT)))
#define BSET( FLAGS, BIT )	((FLAGS) |= (1L << (BIT)))
#define BCLR( FLAGS, BIT )	((FLAGS) &= ~(1L << (BIT)))
#define BCHG( FLAGS, BIT )	((FLAGS) ^= (1L << (BIT)))

/* pascal-like macros for shifting bits */
#define BSL( A, B )			(((long) (A)) << (B))
#define BSR( A, B )			(((long) (A)) >> (B))

/* other macros */
#define ABS(A) 				((A) > 0 ? (A) : -(A))

/* If we're supposed to use Pascal calling conventions, make ALIST_API == pascal. */
#if	ALIST_USE_PASCAL_CALLING
	#define ALIST_API	pascal
#else
	#define ALIST_API
#endif

/* Make sure ALIST_SHARED_LIBRARY is false if building a non-CFM project. */
#if ( ALIST_SHARED_LIBRARY && ( ! TARGET_RT_MAC_CFM ) )
#error "ALIST_SHARED_LIBRARY must be 0 in a classic 68K project."
#endif


#pragma mark Constants

/* result codes */
enum {
	alAddedNone				=	-1,					/* Could be returned from ALAddRow or ALAddColumn */
	alEmptySelectionErr			=	errAENoUserSelection,	/* selection range is empty */
	alUndefinedSelectorErr		=	paramErr,				/* unknown selector */
	alSkipDragItem				=	129,					/* returned from ExtractFlaver function to not add this item */
#if ALIST_USEAPPEARANCEMGR
	alAppearanceMgrNotInstalled	=	130,					/* could be returned from ALMakeUserPaneControl */
#endif
	alNoDragErr 				= 	128,					/* can be returned internally by _ALDrag */
	alNoDataErr				=	131					/* returned if the function requires the ALIST_HAVE_CELLDATA flag to be 1 */
};

/* control character codes */
enum {
	kBackspace 			=	0x08,
	kTab 				=	0x09,
	kEOL 				=	0x0D,
	kArrowLeft 			=	0x1C,
	kArrowRight 			=	0x1D,
	kArrowUp 			=	0x1E,
	kArrowDown 			=	0x1F,
	kSpace 				=  	0x20,
/*	kHome				=	0x7300,
	kPageUp				=	0x7400,
	kEnd					=	0x7700,
	kPageDown			=	0x7900,
*/	kForwardDelete 		=	0x7F
};

/* values for ALAllocate allocFlags parameter */
enum {
	kAllocClear			=	0x0001,		/* clear handle after allocation */
	kAllocTemp			=	0x0002		/* use temporary memory if available */
};

/* values for ALFeatureFlag action parameter */
enum {
	alBitSet 			=	 1,		// enables the specified feature
	alBitClear 		=	 0,		// disables the specified feature
	alBitTest 			=	-1,		// returns the current setting of the specified feature
	alBitToggle 		=	-2		// toggles the specified feature
};

// bit equates for the flags field in the AL record
enum {
	alFHasColorQD 			=	31,		// Color Quickdraw is available
	alFHasDragManager 		=	30,		// the Drag Manager is available
	alFHasTranslucentDrags	=	29,		// Translucent dragging is available
	alFMouseTracking		=	28,		// set internally during mouse tracking
	alFAnchorIsEnd			=	27,		// anchor offset is selEnd
	alFActive				=	26,		// we're active
	alFComingActive		=	25,		// we're just now coming active
	alFHilited				=	24,		// true if list is highlighted (for Drag & Drop)
	alFCanAcceptDrag		=	23,		// the drag in the list can be accepted
	alFDragCaretVisible		=	22,		// drag caret is currently visible
	alFFocused			=	21,		// true if the list is the focus of keyboard input
#if ALIST_USEAPPEARANCEMGR
	alFHasAppearanceMgr	=	20,		// Appearance Manager is available
#endif
#if ALIST_USECONTROLMGR2
	alFHasControlMgr2		=	19,		// Control Manager 2.0 is available
#endif
	alFHasLiveScrollbars		=	18		// true if the scroll bars were created with the live Appearance variant
};

// bit equates for the selections handle
enum {
	alSselected 	= 1		// true if the cell is selected
#if ALIST_HEIRARCHICAL
	, alSsuperrow	= 2,		// true if the cell (row) is a heirarchical super row - has the triangle
	alSexpanded	= 3		// true if the heirarchical row is 'open' - triangle pointing down
#endif
};

// bit equates for the features field in the AL record
enum {
	alFUseTempMem	=	31,		// use temporary memory for main data structures
	alFVAutoScroll		=	30,		// automatically scroll text vertically when cursor is outside pane
	alFVertScroll		=	29,		// has a vertical scroll bar
	alFHAutoScroll		=	28,		// automatically scroll text horizontally when cursor is outside pane
	alFHorzScroll		=	27,		// has a horizontal scroll bar
	alFDynamicScroll	=	26,		// update list while the scroll bar thumbs are being moved
	alFSelOnlyOne		=	25,		// only one selection at a time
	alFSelExtendDrag	=	24,		// can drag extend selection without Shift key
	alFSelNoDisjoint	=	23,		// turns off multiple selections with Command click
	alFSelNoExtend		=	22,		// turns off extending with Shift key
	alFSelNoRect		=	21,		// turns off extending as a rectangle
	alFSelUseSense		=	20,		// Shift click extending uses sense of first cell
	alFDrawFocus		=	19,		// draws or erases focus rectangle around the list (uses Appearance Manager if present)
	alFInhibitRedraw	=	18,		// don't redraw
	alFDrawOffscreen	=	17,		// draw text offscreen for smoother visual results
	alFInhibitColor		=	16,		// use black and white only
	alFStartDrags		=	15,		// can start a drag
	alFReceiveDrags	=	14,		// can receive drags
	alFDragToSelf		=	13,		// can receive drags from self.
	alFOutlineHilite		=	12,		// frame selection range when pane is inactive
	alFDrawRect		=	11,		// draw a 1 pixel wide black rectangle around the list
	alFDrawLines		=	10,		// draw lines between cells
	alFRowsOnly		=	9,		// the list has one column only
	alFColumnsOnly		=	8,		// the list has one row only
	alFHasGrow		=	7,		// leave room for a grow icon in bottom right
	alFNotepadBackground =	6,		// make the background look like a yellow notepad
#if ALIST_USEAPPEARANCEMGR
	alFAppearanceBackground = 5,		// use the Appearance Manager list background
#endif
	alFHeirarchical		=	4		// True if there should be room in the left-most cell for a heirarchical triangle.
};

// selectors for ALGetInfo/ALSetInfo
enum {
	alRefCon				=	'refc',
	alWindow				=	'wind',
	alClickLoop			=	'clik',
	alClickCellHook			=	'CLIk',
	alDrawCellHook			=	'draw',
	alDrawBackgroundHook	=	'back',
	alHiliteCellHook			=	'hili',
	alUserHandle			=	'user',
	alVertScrollControl		=	'ScrV',
	alVertScrollProc		=	'scrv',
	alHorzScrollControl		=	'ScrH',
	alHorzScrollProc		=	'scrh',
	alCurrentDrag			=	'drag',
	alDisposeCellDataHook	=	'dcda',
	alInputFlavorsHook		=	'Ifla',
	alOutputFlavorsHook		=	'Ofla',
	alSendDataDragHook		=	'sdrg',
	alStringSearchHook		=	'strS'
};


// values for caretLoc parameter
enum {
	kCaretTop			=  1,
	kCaretBottom		= -1,
	kCaretWholeCell	=  0,
	kCaretLeft		=  2,
	kCaretRight		= -2,
	kCaretNotInCell		= -3
};

#if ALIST_HEIRARCHICAL
	// values for the disclosure triangle
	enum {
		triEnabledRight = 0,
		triPressedRight,
		triPressedDown,
		triEnabledDown
	};
#endif

// other miscellaneous constants
enum {
	kAL_CaretWidth = 1,			// width of the caret, in pixels
	kAL_MinFontSize = 1,			// minimum valid font size
	kAL_MaxFontSize = SHRT_MAX,	// maximum valid font size
	kAL_nvalidOffset = -1, 			// used to flag an invalid or nonexistent offset
	kAL_CellMargin = 4,			// width of border area surrounding the cell (in pixels)
	kAL_MaxScrollDelta = 7,		// maximum scroll amount used by standard click loop
	kScrollBarWidth = 15,		// width of a scroll bar in pixels
	kAL_AutoScrollDelay = 10,		// delay before auto-scroll starts (in ticks)
	kALType_Text = 'TEXT',			// The default drag type
	kALType_ListData = 'ALST'		// The default group data type for the clipboard.
};

// forward declarations

#if PRAGMA_STRUCT_ALIGN
#pragma options align=power
#endif

typedef struct ALRec	*ALPtr, **ALHandle;

typedef FourCharCode	ALSelector;

typedef void *			ALData;
#if ALIST_HAVE_CELLDATA
	typedef void *			*ALDataPtr;
	typedef void *			**ALDataHandle;
#endif

typedef LongPt			ALCell, *ALCellPtr;

typedef struct {
	ResType	descriptorType;
	Handle	dataHandle;	
} ALDataDescriptor;

#pragma mark UPPs

// callback prototypes

typedef ALIST_API Boolean (*ALClickLoopProcPtr)(ALHandle hAL);
typedef ALIST_API void (*ALScrollProcPtr)(ALHandle hAL);
typedef ALIST_API Boolean (*ALClickCellProcPtr)(const ALCellPtr theCell, Point mouseLoc, EventModifiers modifiers,
							short numberClicks, ALHandle hAL);

typedef ALIST_API OSErr	(*ALInputFlavorsProcPtr)(short index, Boolean *more, ALDataDescriptor *flavorDesc,
							ALData *cellDataPtr, const ALCellPtr cell, ALHandle hAL);
typedef ALIST_API OSErr	(*ALOutputFlavorsProcPtr)(short index, Boolean *more, Boolean forDrag, ALDataDescriptor *flavorDesc,
							ALData cellData, const ALCellPtr cell, ALHandle hAL);

typedef ALIST_API void	(*ALDisposeCellDataProcPtr)(ALData cellData, const ALCellPtr cell, ALHandle hAL);
typedef ALIST_API void	(*ALDrawCellProcPtr)(ALData cellData, ALCellPtr cell, const Rect *cellRect, ALHandle hAL);
typedef ALIST_API void	(*ALDrawBackgroundProcPtr)(const Rect *cellRect, ALHandle hAL);
typedef ALIST_API void	(*ALHiliteCellProcPtr)(ALData cellData, ALCellPtr cell, Boolean active, Boolean doOutline,
						const Rect *cellRect, ALHandle hAL);

typedef ALIST_API Boolean (*ALSearchProcPtr)(ALData cellData, ALData searchData);
typedef ALIST_API Boolean (*ALStringSearchProcPtr)( StringHandle theString, ALCell *cell, ALHandle hAL );

// UPP proc info
#if ALIST_USE_UPPS

enum {
#if ALIST_USE_PASCAL_CALLING
	uppALClickLoopProcInfo = kPascalStackBased
#else
	uppALClickLoopProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(Boolean)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALScrollProcInfo = kPascalStackBased
#else
	uppALScrollProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALClickCellProcInfo = kPascalStackBased
#else
	uppALClickCellProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(Boolean)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(const ALCellPtr /*theCell*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(Point /*mouseLoc*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(EventModifiers /*modifiers*/)))
		| STACK_ROUTINE_PARAMETER(4,SIZE_CODE(sizeof(short /*numberClicks*/)))
		| STACK_ROUTINE_PARAMETER(5,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALInputFlavorsProcInfo = kPascalStackBased
#else
	uppALInputFlavorsProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(OSErr)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(short /*index*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(Boolean * /*more*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(ALDataDescriptor * /*flavorDesc*/)))
		| STACK_ROUTINE_PARAMETER(4,SIZE_CODE(sizeof(ALData * /*cellDataPtr*/)))
		| STACK_ROUTINE_PARAMETER(5,SIZE_CODE(sizeof(ALCellPtr /*cell*/)))
		| STACK_ROUTINE_PARAMETER(6,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALOutputFlavorsProcInfo = kPascalStackBased
#else
	uppALOutputFlavorsProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(OSErr)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(short /*index*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(Boolean * /*more*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(Boolean /*forDrag*/)))
		| STACK_ROUTINE_PARAMETER(4,SIZE_CODE(sizeof(ALDataDescriptor * /*flavorDesc*/)))
		| STACK_ROUTINE_PARAMETER(5,SIZE_CODE(sizeof(ALData /*cellData*/)))
		| STACK_ROUTINE_PARAMETER(6,SIZE_CODE(sizeof(ALCellPtr /*cell*/)))
		| STACK_ROUTINE_PARAMETER(7,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALDisposeCellDataProcInfo = kPascalStackBased
#else
	uppALDisposeCellDataProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALData /*cellData*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALCellPtr /*cell*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALDrawCellProcInfo = kPascalStackBased
#else
	uppALDrawCellProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALData /*cellData*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALCellPtr /*cell*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(Rect */*cellRect*/)))
		| STACK_ROUTINE_PARAMETER(4,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALDrawBackgroundProcInfo = kPascalStackBased
#else
	uppALDrawBackgroundProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(const Rect */*cellRect*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALHiliteCellProcInfo = kPascalStackBased
#else
	uppALHiliteCellProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALData /*cellData*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALCellPtr /*cell*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(Boolean /*active*/)))
		| STACK_ROUTINE_PARAMETER(4,SIZE_CODE(sizeof(Boolean /*doOutline*/)))
		| STACK_ROUTINE_PARAMETER(5,SIZE_CODE(sizeof(Rect */*cellRect*/)))
		| STACK_ROUTINE_PARAMETER(6,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALSearchProcInfo = kPascalStackBased
#else
	uppALSearchProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(Boolean)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALData /*cellData*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALData /*searchData*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALStringSearchProcInfo = kPascalStackBased
#else
	uppALStringSearchProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(Boolean)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(StringHandle /*theString*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALCell */*cell*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(ALHandle /*hAL*/)))
};

#endif

/*	UPPs, New‰Proc macros & Call‰Proc macros */

/*
	NOTE:
    For compatibility with the Pascal version, Call‰Proc macros take the form:

		CallFooProc(..., userRoutine)

	instead of:

		CallFooProc(userRoutine, ...)

*/

#if ALIST_USE_UPPS

typedef UniversalProcPtr ALClickLoopUPP;
typedef UniversalProcPtr ALScrollUPP;
typedef UniversalProcPtr	ALClickCellUPP;
typedef UniversalProcPtr	ALInputFlavorsUPP;
typedef UniversalProcPtr	ALOutputFlavorsUPP;
typedef UniversalProcPtr ALDisposeCellDataUPP;
typedef UniversalProcPtr ALDrawCellUPP;
typedef UniversalProcPtr ALDrawBackgroundUPP;
typedef UniversalProcPtr ALHiliteCellUPP;
typedef UniversalProcPtr ALSearchUPP;
typedef UniversalProcPtr ALStringSearchUPP;

#define NewALClickLoopProc(userRoutine) \
	(ALClickLoopUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALClickLoopProcInfo, GetCurrentArchitecture())
#define NewALScrollProc(userRoutine) \
	(ALScrollUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALScrollProcInfo, GetCurrentArchitecture())
#define NewALClickCellProc(userRoutine) \
	(ALClickCellUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALClickCellProcInfo, GetCurrentArchitecture())
#define NewALInputFlavorsProc(userRoutine) \
	(ALInputFlavorsUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALInputFlavorsProcInfo, GetCurrentArchitecture())
#define NewALOutputFlavorsProc(userRoutine) \
	(ALOutputFlavorsUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALOutputFlavorsProcInfo, GetCurrentArchitecture())
#define NewALDisposeCellDataProc(userRoutine) \
	(ALDisposeCellDataUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALDisposeCellDataProcInfo, GetCurrentArchitecture())
#define NewALDrawCellProc(userRoutine) \
	(ALDrawCellUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALDrawCellProcInfo, GetCurrentArchitecture())
#define NewALDrawBackgroundProc(userRoutine) \
	(ALDrawBackgroundUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALDrawBackgroundProcInfo, GetCurrentArchitecture())
#define NewALHiliteCellProc(userRoutine) \
	(ALHiliteCellUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALHiliteCellProcInfo, GetCurrentArchitecture())
#define NewALSearchProc(userRoutine) \
	(ALSearchUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALSearchProcInfo, GetCurrentArchitecture())
#define NewALStringSearchProc(userRoutine) \
	(ALStringSearchUPP) NewRoutineDescriptor((ProcPtr) (userRoutine), uppALStringSearchProcInfo, GetCurrentArchitecture())

#define CallALClickLoopProc(hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALClickLoopProcInfo, (hAL))
#define CallALScrollProc(hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALScrollProcInfo, (hAL))
#define CallALClickCellProc(cell, mouseLoc, modifiers, numberClicks, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALClickCellProcInfo, (cell), (mouseLoc), (modifiers), (numberClicks), (hAL))
#define CallALInputFlavorsProc(index, more, flavorDesc, cellDataPtr, cell, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALInputFlavorsProcInfo, (index), (more), (flavorDesc), (cellDataPtr), (cell), (hAL))
#define CallALOutputFlavorsProc(index, more, forDrag, flavorDesc, cellData, cell, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALOutputFlavorsProcInfo, (index), (more), (forDrag), (flavorDesc), (cellData), (cell), (hAL))
#define CallALDisposeCellDataProc(cellData, cell, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALDisposeCellDataProcInfo, (cellData), (cell), (hAL))
#define CallALDrawCellProc(pData, cell, cellRect, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALDrawCellProcInfo, (pData), (cell), (cellRect), (hAL))
#define CallALDrawBackgroundProc(cellRect, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALDrawBackgroundProcInfo, (cellRect), (hAL))
#define CallALHiliteCellProc(pData, cell, active, doOutline, cellRect, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALHiliteCellProcInfo, (pData), (cell), (active), (doOutline), (cellRect), (hAL))
#define CallALSearchProc(cellData, searchData, userRoutine) \
	CallUniversalProc((userRoutine), uppALSearchProcInfo, (cellData), (searchData))
#define CallALStringSearchProc(theString, cell, hAL, userRoutine) \
	CallUniversalProc((userRoutine), uppALStringSearchProcInfo, (theString), (cell), (hAL))

#define DisposeALClickLoopUPP(userRoutine)		DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALScrollUPP(userRoutine)			DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALClickCellUPP(userRoutine)		DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALInputFlavorsUPP(userRoutine)	DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALOutputFlavorsUPP(userRoutine)	DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALDisposeCellDataUPP(userRoutine)	DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALDrawCellUPP(userRoutine)		DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALDrawBackgroundUPP(userRoutine)	DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALHiliteCellUPP(userRoutine)		DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALSearchUPP(userRoutine)		DisposeRoutineDescriptor( (userRoutine) )
#define DisposeALStringSearchUPP(userRoutine)	DisposeRoutineDescriptor( (userRoutine) )

#else

typedef ALClickLoopProcPtr		ALClickLoopUPP;
typedef ALScrollProcPtr			ALScrollUPP;
typedef ALClickCellProcPtr		ALClickCellUPP;
typedef ALInputFlavorsProcPtr		ALInputFlavorsUPP;
typedef ALOutputFlavorsProcPtr	ALOutputFlavorsUPP;
typedef ALDisposeCellDataProcPtr	ALDisposeCellDataUPP;
typedef ALDrawCellProcPtr		ALDrawCellUPP;
typedef ALDrawBackgroundProcPtr	ALDrawBackgroundUPP;
typedef ALHiliteCellProcPtr		ALHiliteCellUPP;
typedef ALSearchProcPtr			ALSearchUPP;
typedef ALStringSearchProcPtr	ALStringSearchUPP;

#define NewALClickLoopProc(userRoutine) ((ALClickLoopUPP) (userRoutine))
#define NewALScrollProc(userRoutine) ((ALScrollUPP) (userRoutine))
#define NewALClickCellProc(userRoutine) ((ALClickCellUPP) (userRoutine))
#define NewALInputFlavorsProc(userRoutine) ((ALInputFlavorsUPP) (userRoutine))
#define NewALOutputFlavorsProc(userRoutine) ((ALOutputFlavorsUPP) (userRoutine))
#define NewALDisposeCellDataProc(userRoutine) ((ALDisposeCellDataUPP) (userRoutine))
#define NewALDrawCellProc(userRoutine) ((ALDrawCellUPP) (userRoutine))
#define NewALDrawBackgroundProc(userRoutine) ((ALDrawBackgroundUPP) (userRoutine))
#define NewALHiliteCellProc(userRoutine) ((ALHiliteCellUPP) (userRoutine))
#define NewALSearchProc(userRoutine) ((ALSearchUPP) (userRoutine))
#define NewALStringSearchProc(userRoutine) ((ALStringSearchUPP) (userRoutine))

#define CallALClickLoopProc(hAL, userRoutine) \
	(*(userRoutine))((hAL))
#define CallALScrollProc(hAL, userRoutine) \
	(*(userRoutine))((hAL))
#define CallALClickCellProc(theCell, mouseLoc, modifiers, numberClicks, hAL, userRoutine) \
	(*(userRoutine))((theCell), (mouseLoc), (modifiers), (numberClicks), (hAL))
#define CallALInputFlavorsProc(index, more, flavorDesc, cellDataPtr, cell, hAL, userRoutine) \
	(*(userRoutine))((index), (more), (flavorDesc), (cellDataPtr), (cell), (hAL))
#define CallALOutputFlavorsProc(index, more, forDrag, flavorDesc, cellData, cell, hAL, userRoutine) \
	(*(userRoutine))((index), (more), (forDrag), (flavorDesc), (cellData), (cell), (hAL))
#define CallALDisposeCellDataProc(cellData, cell, hAL, userRoutine) \
	(*(userRoutine))((cellData), (cell), (hAL))
#define CallALDrawCellProc(pData, cell, cellRect, hAL, userRoutine) \
	(*(userRoutine))((pData), (cell), (cellRect), (hAL))
#define CallALDrawBackgroundProc(cellRect, hAL, userRoutine) \
	(*(userRoutine))((cellRect), (hAL))
#define CallALHiliteCellProc(pData, cell, active, doOutline, cellRect, hAL, userRoutine) \
	(*(userRoutine))((pData), (cell), (active), (doOutline), (cellRect), (hAL))
#define CallALSearchProc(cellData, searchData, userRoutine) \
	(*(userRoutine))((cellData), (searchData))
#define CallALStringSearchProc(theString, cell, hAL, userRoutine) \
	(*(userRoutine))((theString), (cell), (hAL))

#define DisposeALClickLoopUPP(userRoutine)
#define DisposeALScrollUPP(userRoutine)
#define DisposeALClickCellUPP(userRoutine)
#define DisposeALInputFlavorsUPP(userRoutine)
#define DisposeALOutputFlavorsUPP(userRoutine)
#define DisposeALDisposeCellDataUPP(userRoutine)
#define DisposeALDrawCellUPP(userRoutine)
#define DisposeALDrawBackgroundUPP(userRoutine)
#define DisposeALHiliteCellUPP(userRoutine)
#define DisposeALSearchUPP(userRoutine)
#define DisposeALStringSearchUPP(userRoutine)

#endif

#pragma mark Typedefs

typedef unsigned char ALSelection, *ALSelectionPtr, **ALSelectionHandle;

#if ALIST_HEIRARCHICAL
	typedef unsigned short ALRowLevel, *ALRowLevelPtr, **ALRowLevelHandle;
#endif

typedef struct ALFieldDescriptor {
	short fOffset;
	short fLength;
} ALFieldDescriptor;

typedef struct ALLookupTable {
	ALSelector selector;
	ALFieldDescriptor desc;
} ALLookupTable;

typedef struct	ALRec {
	WindowPtr		winRef;				// the window that this list is in.
	GWorldPtr			offscreenPort;			// offscreen graphics world 
	RgnHandle			viewRgn;				// handle to the view region 

#if ALIST_HAVE_CELLDATA
	ALDataHandle		hData;				// handle to the data
#endif
	unsigned long		dataLength;			// length of data (total number of cells)
	LongRect			dataBounds;			// boundary of cells allocated

	ALSelectionHandle	hSelected;				// which cells are selected
#if ALIST_HEIRARCHICAL
	ALRowLevelHandle	hLevels;				// what heirarchical level is the row at?
#endif

	Rect				dispRect;				// display rectangle, all drawing is clipped to this (port coordinates)
	LongRect			visCells;				// boundary of visible cells rectangle
	Point				cellSize;				// size of cells

	unsigned long		flags;				// 32 bits of miscellaneous flags (private)
	unsigned long		features;				// 32 bit of feature flags (public)

	unsigned long		keyTime;				// time of most recent alpha-numeric key press.
	StringHandle		keyString;			// the string of keys pressed.
	ALStringSearchUPP	stringSearchHook;		// string search callback

	unsigned long		clickTime;				// time of most recent click, in ticks
	Point				clickLoc;				// position of most recent click
	ALCell			lastClickCell;			// cell location of most recent click
	ALClickLoopUPP	clickLoop;				// click loop callback
	ALClickCellUPP		clickCellHook;			// click cell callback

	unsigned short		clickCount;			// multiple click count

	ControlHandle		vScroll;				// vertical scroll bar
	ALScrollUPP		scrollVertProc;			// scroll vertical callback 
	ControlHandle		hScroll;				// horizontal scroll bar
	ALScrollUPP		scrollHorzProc;		// scroll horizontal callback 
        int                     scrolls_visible;

	void				*refCon;				// reference value for client use 
	Handle			*userHandle;			// handle for client use

	DragReference		currentDrag;			// reference of drag being tracked by _ALDrag
	short			dragCaretPos;			// is drag caret on left, right, top or bottom of cell
	unsigned long		caretTime;			// time of most recent caret drawing, in ticks 
	ALCell			dragCaretLoc;			// cell location of caret displayed during a drag 
	ALDisposeCellDataUPP	disposeCellDataHook;		// hook for disposing of data in a cell (during a Cut, or Delete operation)
	ALInputFlavorsUPP		inputFlavorsHook;		// take flavorDesc and put into cell data for paste/receive drag
	ALOutputFlavorsUPP		outputFlavorsHook;		// put cell data into the flavorDesc for copy/drag
	DragSendDataUPP		sendDataDragHook;		// send drag flavor(s) data hook

	ALDrawCellUPP			drawCellHook;			// hook for drawing cell
	ALDrawBackgroundUPP	drawBackgroundHook;	// hook for drawing the cell background
	ALHiliteCellUPP			hiliteCellHook;			// hook for hiliting a cell
} ALRec;

#if PRAGMA_STRUCT_ALIGN
#pragma options align=reset
#endif

// Function prototypes.

#pragma mark Function Prototypes

#ifdef __cplusplus
extern "C" {
#endif

// Routines in ALBirthDeath.c
ALIST_API short	ALVersion( void );
ALIST_API OSErr	ALNew(const WindowPtr wr, const Rect *dispRect, const LongRect *dataBounds, Point cellSize,
						unsigned long features, ALHandle *hAL);
ALIST_API void		ALDispose(ALHandle hAL);
ALIST_API void		ALSetViewRect(const Rect *viewRect, ALHandle hAL);
ALIST_API void		ALGetViewRect(Rect *viewRect, ALHandle hAL);


// Routines in ALCellData.c
ALIST_API OSErr	ALClearCell(const ALCellPtr theCell, ALHandle hAL);
ALIST_API OSErr	ALGetCell(ALData *dataPtr, const ALCellPtr theCell, ALHandle hAL);
ALIST_API void		ALGetCellRect(Rect *cellRect, const ALCellPtr theCell, ALHandle hAL);
ALIST_API void		ALSetCellSize(Point cSize, ALHandle hAL);
ALIST_API void		ALGetCellSize(Point *cSize, ALHandle hAL);
ALIST_API Boolean	ALNextCell(Boolean hNext, Boolean vNext, ALCell *theCell, ALHandle hAL);
ALIST_API OSErr	ALSetCell(const ALData data, const ALCellPtr theCell, ALHandle hAL);
ALIST_API Boolean	ALSearch(const ALData searchData, ALSearchUPP searchProc, ALCellPtr theCell, ALHandle hAL);
ALIST_API long		ALAddColumn(long count, long afterColNum, ALHandle hAL);
ALIST_API void		ALDelColumn(long count, long startColNum, ALHandle hAL);
ALIST_API long		ALAddRow(long count, long afterRowNum, ALHandle hAL);
ALIST_API void		ALDelRow(long count, long startRowNum, ALHandle hAL);
ALIST_API long		ALGetNumberRows(ALHandle hAL);
ALIST_API long		ALGetNumberColumns(ALHandle hAL);
ALIST_API Boolean	ALIsVisible(const ALCellPtr theCell, ALHandle hAL);


// Routines in ALDrawing.c
ALIST_API void		ALDrawCell(const ALCellPtr theCell, ALHandle hAL);
ALIST_API void		ALUpdate(RgnHandle updateRgn, ALHandle hAL);


// Routines in ALEditing.c
ALIST_API Boolean	ALCanUndo(Boolean *isRedo, ALHandle hAL);
ALIST_API void		ALUndo(ALHandle hAL);
ALIST_API void		ALCut(ALHandle hAL);
ALIST_API void		ALCopy(ALHandle hAL);
ALIST_API Boolean	ALCanPaste(ALHandle hAL);
ALIST_API void		ALPaste(ALHandle hAL);
ALIST_API void		ALDelete(ALHandle hAL);


// Routines in ALHeirarchical.c
#if ALIST_HEIRARCHICAL
	ALIST_API OSErr	ALExpandRow(long rowNum, Boolean expandChildren, ALHandle hAL);
	ALIST_API OSErr	ALCollapseRow(long rowNum, Boolean collapseChildren, ALHandle hAL);
	ALIST_API Boolean	ALIsRowExpanded( long inSuperRowNum, ALHandle hAL );
	ALIST_API Boolean	ALIsRowHidden( long inAnyRowNum, ALHandle hAL );
	ALIST_API long		ALSuperRow( long subRow, ALHandle hAL);
	ALIST_API long		ALAddRowUnder( long count, long superRow, ALHandle hAL);
#endif

// Routines in ALKeyboard.c
ALIST_API void ALKey( SInt16 inCharCode,  EventModifiers inModifiers, unsigned long inKeyTime, ALHandle hAL );


// Routines in ALMouse.c
ALIST_API void		ALGetCellFromItemRef(ItemReference theItem, ALCell *theCell);
ALIST_API Boolean	ALCanAcceptDrag(DragReference theDrag, ALHandle hAL);
  ALIST_API Boolean	ALClick(Point mouseLoc0, Point mouseLoc, EventModifiers modifiers, unsigned long clickTime, ALHandle hAL);
ALIST_API void		ALLastClick(ALCellPtr theCell, ALHandle hAL);
ALIST_API OSErr	ALReceiveDrag(DragReference theDrag, ALHandle hAL);
ALIST_API OSErr	ALTrackDrag(DragTrackingMessage theMessage, DragReference theDrag, ALHandle hAL);


// Routines in ALScrolling.c
ALIST_API Boolean	ALAutoScroll(Point moveTo, const ALCellPtr whichCell, ALHandle hAL);
ALIST_API void		ALScrollCells(long dCols, long dRows, ALHandle hAL);
ALIST_API void		ALScrollPixels(long hOffset, long vOffset, ALHandle hAL);


// Routines in ALSelecting.c
ALIST_API short		ALGetCellAndEdge(Point thePoint, ALCellPtr cell, ALHandle hAL);
ALIST_API RgnHandle	ALGetCellHiliteRgn(const ALCellPtr theCell, ALHandle hAL);
ALIST_API long			ALGetNumberSelected(ALHandle hAL);
ALIST_API RgnHandle	ALGetSelectedHiliteRgn(ALHandle hAL);
ALIST_API Boolean		ALGetSelect(Boolean next, ALCell *theCell, ALHandle hAL);
ALIST_API void			ALSetSelect(Boolean setIt, ALCellPtr theCell, ALHandle hAL);
ALIST_API void			ALSetSelectNone(Boolean redrawIfChange, ALHandle hAL);
ALIST_API void			ALSetSelectAll(Boolean redrawIfChange, ALHandle hAL);


// Routines in ALSelectors.c
ALIST_API OSErr			ALGetInfo(ALSelector selector, void *info, ALHandle hAL);
ALIST_API OSErr			ALSetInfo(ALSelector selector, const void *info, ALHandle hAL);
ALIST_API short			ALFeatureFlag(unsigned long feature, short action, ALHandle hAL);
ALIST_API void				ALActivate(Boolean isActive, ALHandle hAL);
ALIST_API ControlPartCode	ALSetFocus(ControlPartCode focusPart, ALHandle hAL);
ALIST_API ControlPartCode	ALPartFocused(ALHandle hAL);


#if ALIST_USEAPPEARANCEMGR
	// Routines in ALUserPane.c
	ALIST_API OSErr	ALMakeUserPaneControl(ALHandle hAL, Boolean canFocus, ControlHandle *newCntl);
#endif


// Private Routines:

#if (ALIST_SHARED_LIBRARY == 1)
#pragma internal on
#endif

// Make these functions internal to this code segment only if we're generating a shared library.

#if !( defined( TARGET_API_MAC_CARBON ) && ( TARGET_API_MAC_CARBON == 1 ) )
	#define	GetRegionBounds( r, b )			_ALGetRegionBounds( (r), (b) )
	#define	GetPortPixMap( port )			_ALGetPortPixMap( port )
	#define	GetPortVisibleRegion( port, rgn )	_ALGetPortVisibleRegion( (port), (rgn) );

	Rect *_ALGetRegionBounds( const RgnHandle inRgn, Rect *outBox );
	PixMapHandle _ALGetPortPixMap( const CGrafPtr port );
	RgnHandle _ALGetPortVisibleRegion( const CGrafPtr port, RgnHandle rgn );
#endif

// Routines in ALBirthDeath.c
void	_ALSetStandardHooks(ALHandle hAL);
void _ALRemoveStandardHookUser( void );

// Routines in ALCellData.c
unsigned long	_ALCalcOffsetFromCell(const ALCellPtr theCell, const LongRect *bounds);
void			_ALCalcCellFromOffset(ALCellPtr theCell, unsigned long offset, const LongRect *bounds);
void			_ALCalcVisibleCells(ALHandle hAL);
Boolean		_ALCheckInsideBounds(const ALCellPtr theCell, const LongRect *bounds);

// Routines in ALDrawing.c
void _ALCalcCellRect(Rect *cellRect, const ALCellPtr theCell, Boolean includeHeirarchical, ALHandle hAL);
void	_ALDrawCaret(ALCellPtr caretLoc, short caretPos, ALHandle hAL);
#if ALIST_HEIRARCHICAL
	void _ALDrawDisclosureTriangle( const Rect *cellRect, Boolean active, short triangleState, ALHandle hAL );
#endif
void _ALDrawListBorder(ALHandle hAL);
void _ALSetNotepadBackgroundColor(void);

// Routines in ALKeyboard.c
// This performs a case-insensitive string compare.
int	_ALStrNCmp( const unsigned char * str1, const unsigned char * str2, int len );

// Routines in ALMouse.c
Boolean		_ALIsOptionDrag(DragReference drag);
OSErr		_ALMakeDragImage(DragReference theDrag, const ALCellPtr imageCell, GWorldPtr *imageGWorld,
								RgnHandle *imageRgn, ALHandle hAL);
OSErr		_ALDrag(Point mouseLoc, EventModifiers modifiers, unsigned long clickTime, ALHandle hAL);
OSErr		_ALExtractFlavor(DragReference theDrag, ItemReference theItem, ALData *cellData, ALHandle hAL);
pascal void	_ALHorzScrollActionProc(ControlHandle ctlRef, ControlPartCode partCode);
pascal void	_ALVertScrollActionProc(ControlHandle ctlRef, ControlPartCode partCode);
void			_ALUpdateDragCaret(ALCellPtr dragLoc, short dragPos, ALHandle hAL);
ItemReference	_ALGetItemRefFromCell(const ALCellPtr theCell);

// Routines in ALSelecting.c
void		_ALHiliteSelected(ALHandle hAL);
Boolean	_ALCellIsSelected(const ALCellPtr theCell, ALHandle hAL);
void		_ALSelectRect(Boolean turnOn, Boolean redrawIfChange, const LongRect *selectRect, ALHandle hAL);
void		_ALSelectOnlyOne(Boolean redrawIfChange, const ALCellPtr theCell, ALHandle hAL);

// Routines in ALUtilities.c
void		_ALForgetHandle(Handle *h);
Boolean	_ALSetHandleLock(Handle h, Boolean lock);
void		_ALBlockClr(void *block, long blockSize);
Boolean	_ALBlockCmp(const void *block1, const void *block2, long blockSize);
void		_ALReorder(long *a, long *b);
OSErr	_ALAllocate(long blockSize, short allocFlags, Handle *h);

#define SetPortWindowPort(p) SetPort(GetWindowPort(p))

#if (ALIST_SHARED_LIBRARY == 1)
#pragma internal reset
#endif

#ifdef __cplusplus
}
#endif
