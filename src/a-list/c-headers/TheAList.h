/*
 *	TheAList.h
 *
 *	The A List
 *	External C/C++ interface
 *
 *  Copyright (c) 1997-2000 Kyle Hammond
 *	All Rights Reserved
 *
 */

#if !defined( __TheAList_h_ )
#define __TheAList_h_

/*	MacOS Toolbox #includes */

#ifndef WX_CARBON

#if !defined( __CONTROLS__ )
	#include <Controls.h>
#endif

#if defined(UNIVERSAL_INTERFACES_VERSION) && (UNIVERSAL_INTERFACES_VERSION >= 0x0330)
	#if !defined( __CONTROLDEFINITIONS__ )
		#include <ControlDefinitions.h>
	#endif
#endif

#if !defined( __DRAG__ )
	#include <Drag.h>
#endif

#if !defined( __ERRORS__ )
	#include <Errors.h>
#endif

#if !defined( __MIXEDMODE__ )
	#include <MixedMode.h>
#endif

#endif
/*	other #includes */

#if !defined( __LONGCOORDINATES__ )
	#if !defined( _LongCoords_ )
		#include "LongCoords.h"
	#endif
#endif

#include "AListOptimizations.h"

#if	ALIST_USE_PASCAL_CALLING
	#define ALIST_API	pascal
#else
	#define ALIST_API
#endif

#pragma mark Constants

/* result codes */
enum {
	alAddedNone				=	-1,					/* Could be returned from ALAddRow or ALAddCol */
	alEmptySelectionErr			=	errAENoUserSelection,	/* selection range is empty */
	alUndefinedSelectorErr		=	paramErr,				/* unknown selector */
	alSkipDragItem				=	129,					/* returned from ExtractFlavor function to not add this item */
#if ALIST_USEAPPEARANCEMGR
	alAppearanceMgrNotInstalled	=	130,					/* may be returned from ALMakeUserPaneControl */
#endif
	alNoDataErr				=	131					/* returned if the function requires the */
													/* ALIST_HAVE_CELLDATA flag to be 1 */
};

/* Useful control character codes. */
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

/* values for ALFeatureFlag action parameter */
enum {
	alBitSet 			=	 1,		/* enables the specified feature */
	alBitClear 		=	 0,		/* disables the specified feature */
	alBitTest 			=	-1,		/* returns the current setting of the specified feature */
	alBitToggle 		=	-2		/* toggles the specified feature */
};

/* Bit equates for the features field in the AL record */
enum {
	alFUseTempMem	=	31,		/* use temporary memory for main data structures */
	alFVAutoScroll		=	30,		/* automatically scroll text vertically when cursor is outside pane */
	alFVertScroll		=	29,		/* has a vertical scroll bar */
	alFHAutoScroll		=	28,		/* automatically scroll text horizontally when cursor is outside pane */
	alFHorzScroll		=	27,		/* has a horizontal scroll bar */
	alFDynamicScroll	=	26,		/* update list while the scroll bar thumbs are being moved */
	alFSelOnlyOne		=	25,		/* only one selection at a time */
	alFSelExtendDrag	=	24,		/* can drag extend selection without Shift key */
	alFSelNoDisjoint	=	23,		/* turns off multiple selections with Command click */
	alFSelNoExtend		=	22,		/* turns off extending with Shift key */
	alFSelNoRect		=	21,		/* turns off extending as a rectangle */
	alFSelUseSense		=	20,		/* Shift click extending uses sense of first cell */
	alFDrawFocus		=	19,		/* draws or erases focus rectangle around the list (uses Appearance Manager if present) */
	alFInhibitRedraw	=	18,		/* don't redraw */
	alFDrawOffscreen	=	17,		/* draw text offscreen for smoother visual results */
	alFInhibitColor		=	16,		/* use black and white only */
	alFStartDrags		=	15,		/* can start a drag */
	alFReceiveDrags	=	14,		/* can receive drags */
	alFDragToSelf		=	13,		/* can receive drags from self. */
	alFOutlineHilite		=	12,		/* frame selection range when pane is inactive */
	alFDrawRect		=	11,		/* draw a 1 pixel wide black rectangle around the list */
	alFDrawLines		=	10,		/* draw lines between cells */
	alFRowsOnly		=	9,		/* the list has one column only */
	alFColumnsOnly		=	8,		/* the list has one row only */
	alFHasGrow		=	7,		/* leave room for a grow icon in bottom right */
	alFNotepadBackground =	6,		/* make the background look like a yellow notepad */
#if ALIST_USEAPPEARANCEMGR
	alFAppearanceBackground = 5,		/* use the Appearance Manager list background */
#endif
	alFHeirarchical		=	4		/* True if there should be room in the left-most cell for a heirarchical triangle. */
};

/* Use with ALNew only. */
enum {
	alDoUseTempMem	= 1L << alFUseTempMem,
	alDoVAutoScroll	= 1L << alFVAutoScroll,
	alDoVertScroll		= 1L << alFVertScroll,
	alDoHAutoScroll	= 1L << alFHAutoScroll,
	alDoHorzScroll		= 1L << alFHorzScroll,
	alDoDynamicScroll 	= 1L << alFDynamicScroll,
	alDoSelOnlyOne		= 1L << alFSelOnlyOne,
	alDoSelExtendDrag 	= 1L << alFSelExtendDrag,
	alDoSelNoDisjoint	= 1L << alFSelNoDisjoint,
	alDoSelNoExtend	= 1L << alFSelNoExtend,
	alDoSelNoRect		= 1L << alFSelNoRect,
	alDoSelUseSense	= 1L << alFSelUseSense,
	alDoDrawFocus		= 1L << alFDrawFocus,
	alDoInhibitRedraw	= 1L << alFInhibitRedraw,
	alDoDrawOffscreen	= 1L << alFDrawOffscreen,
	alDoInibitColor		= 1L << alFInhibitColor,
	alDoStartDrags		= 1L << alFStartDrags,
	alDoReceiveDrags	= 1L << alFReceiveDrags,
	alDoDragToSelf		= 1L << alFDragToSelf,
	alDoDrags			= alDoStartDrags + alDoReceiveDrags,
	alDoOutlineHilite	= 1L << alFOutlineHilite,
	alDoDrawRect		= 1L << alFDrawRect,
	alDoDrawLines		= 1L << alFDrawLines,
	alDoRowsOnly		= 1L << alFRowsOnly,
	alDoColumnsOnly	= 1L << alFColumnsOnly,
	alDoHasGrow		= 1L << alFHasGrow,
	alDoNotepadBg		= 1L << alFNotepadBackground,
#if ALIST_USEAPPEARANCEMGR
	alDoAppearanceBg	= 1L << alFAppearanceBackground,
#endif
	alDoHeirarchical	= 1L << alFHeirarchical
};

/* selectors for ALGetInfo/ALSetInfo */
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

/* other miscellaneous constants */
enum {
	kAL_CellMargin = 4,			/* width of border area surrounding the cell (in pixels) */
	kAL_MaxScrollDelta = 7,		/* maximum scroll amount used by standard click loop */
	kScrollBarWidth = 16,		/* width of a scroll bar in pixels */
	kAL_AutoScrollDelay = 10,		/* delay before auto-scroll starts (in ticks) */
	kAL_TypeText = 'TEXT',			/* The default drag type */
	kAL_TypeListData = 'ALST'		/* The default group data type for the clipboard. */
};

/* values for caret location parameter (ALGetCellAndEdge) */
enum {
	kCaretTop			=  1,
	kCaretBottom		= -1,
	kCaretWholeCell	=  0,
	kCaretLeft		=  2,
	kCaretRight		= -2,
	kCaretNotInCell		= -3
};

/* forward declarations */

#if PRAGMA_STRUCT_ALIGN
#pragma options align=power
#endif

#pragma mark Typedefs

typedef struct OpaqueALReference *ALReference;

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

/* callback prototypes */

typedef ALIST_API Boolean (*ALClickLoopProcPtr)(ALReference hAL);
typedef ALIST_API void (*ALScrollProcPtr)(ALReference hAL);
typedef ALIST_API Boolean (*ALClickCellProcPtr)(const ALCellPtr theCell, Point mouseLoc, EventModifiers modifiers,
							short numberClicks, ALReference hAL);

typedef ALIST_API OSErr	(*ALInputFlavorsProcPtr)(short index, Boolean *more, ALDataDescriptor *flavorDesc, ALData *cellDataPtr,
							const ALCellPtr cell, ALReference hAL);
typedef ALIST_API OSErr	(*ALOutputFlavorsProcPtr)(short index, Boolean *more, Boolean forDrag, ALDataDescriptor *flavorDesc,
							ALData cellData, const ALCellPtr cell, ALReference hAL);

typedef ALIST_API void	(*ALDisposeCellDataProcPtr)(ALData cellData, const ALCellPtr cell, ALReference hAL);
typedef ALIST_API void	(*ALDrawCellProcPtr)(ALData cellData, ALCellPtr cell, const Rect *cellRect, ALReference hAL);
typedef ALIST_API void	(*ALDrawBackgroundProcPtr)(const Rect *cellRect, ALReference hAL);
typedef ALIST_API void	(*ALHiliteCellProcPtr)(ALData cellData, ALCellPtr cell, Boolean active, Boolean doOutline,
						const Rect *cellRect, ALReference hAL);

typedef ALIST_API Boolean (*ALSearchProcPtr)(ALData cellData, ALData searchData);
typedef ALIST_API Boolean (*ALStringSearchProcPtr)( StringHandle theString, ALCell *cell, ALReference hAL );

/* UPP proc info */
#if ALIST_USE_UPPS

enum {
#if ALIST_USE_PASCAL_CALLING
	uppALClickLoopProcInfo = kPascalStackBased
#else
	uppALClickLoopProcInfo = kCStackBased
#endif
		| RESULT_SIZE(SIZE_CODE(sizeof(Boolean)))
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALReference /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALScrollProcInfo = kPascalStackBased
#else
	uppALScrollProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALReference /*hAL*/)))
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
		| STACK_ROUTINE_PARAMETER(5,SIZE_CODE(sizeof(ALReference /*hAL*/)))
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
		| STACK_ROUTINE_PARAMETER(6,SIZE_CODE(sizeof(ALReference /*hAL*/)))
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
		| STACK_ROUTINE_PARAMETER(7,SIZE_CODE(sizeof(ALReference /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALDisposeCellDataProcInfo = kPascalStackBased
#else
	uppALDisposeCellDataProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(ALData /*cellData*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALCellPtr /*cell*/)))
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(ALReference /*hAL*/)))
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
		| STACK_ROUTINE_PARAMETER(4,SIZE_CODE(sizeof(ALReference /*hAL*/)))
};
enum {
#if ALIST_USE_PASCAL_CALLING
	uppALDrawBackgroundProcInfo = kPascalStackBased
#else
	uppALDrawBackgroundProcInfo = kCStackBased
#endif
		| STACK_ROUTINE_PARAMETER(1,SIZE_CODE(sizeof(const Rect */*cellRect*/)))
		| STACK_ROUTINE_PARAMETER(2,SIZE_CODE(sizeof(ALReference /*hAL*/)))
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
		| STACK_ROUTINE_PARAMETER(6,SIZE_CODE(sizeof(ALReference /*hAL*/)))
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
		| STACK_ROUTINE_PARAMETER(3,SIZE_CODE(sizeof(ALReference /*hAL*/)))
};

#endif

/*	UPPs, New‰Proc macros & Call‰Proc macros


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

#if PRAGMA_STRUCT_ALIGN
#pragma options align=reset
#endif

/* Function prototypes. */

#pragma mark Function Prototypes

#ifdef __cplusplus
extern "C" {
#endif

/* Routines for creating and disposing of a list. */
ALIST_API short	ALVersion( void );
ALIST_API OSErr	ALNew(const WindowPtr wr, const Rect *viewRect, const LongRect *dataBounds, Point cellSize,
						unsigned long features, ALReference *hAL);
ALIST_API void		ALDispose(ALReference hAL);
ALIST_API void		ALSetViewRect(const Rect *viewRect, ALReference hAL);
ALIST_API void		ALGetViewRect(Rect *viewRect, ALReference hAL);
ALIST_API void          ALShow(int on, ALReference hAL);

/* Routines for getting and setting data in a list. */
ALIST_API OSErr	ALClearCell(const ALCellPtr theCell, ALReference hAL);
ALIST_API OSErr	ALGetCell(ALData *dataPtr, const ALCellPtr theCell, ALReference hAL);
ALIST_API void		ALGetCellRect(Rect *cellRect, const ALCellPtr theCell, ALReference hAL);
ALIST_API void		ALSetCellSize(Point cSize, ALReference hAL);
ALIST_API void		ALGetCellSize(Point *cSize, ALReference hAL);
ALIST_API Boolean	ALNextCell(Boolean hNext, Boolean vNext, ALCell *theCell, ALReference hAL);
ALIST_API OSErr	ALSetCell(const ALData data, const ALCellPtr theCell, ALReference hAL);
ALIST_API Boolean	ALSearch(const ALData searchData, ALSearchUPP searchProc, ALCellPtr theCell, ALReference hAL);
ALIST_API long		ALAddColumn(long count, long afterColNum, ALReference hAL);
ALIST_API void		ALDelColumn(long count, long startColNum, ALReference hAL);
ALIST_API long		ALAddRow(long count, long afterRowNum, ALReference hAL);
ALIST_API void		ALDelRow(long count, long startRowNum, ALReference hAL);
ALIST_API long		ALGetNumberRows(ALReference hAL);
ALIST_API long		ALGetNumberColumns(ALReference hAL);
ALIST_API Boolean	ALIsVisible(const ALCellPtr theCell, ALReference hAL);


/* Routines for drawing lists. */
ALIST_API void		ALDrawCell(const ALCellPtr theCell, ALReference hAL);
ALIST_API void		ALUpdate(RgnHandle updateRgn, ALReference hAL);


/* Routines for editing support. */
ALIST_API Boolean	ALCanUndo(Boolean *isRedo, ALReference hAL);
ALIST_API void		ALUndo(ALReference hAL);
ALIST_API void		ALCut(ALReference hAL);
ALIST_API void		ALCopy(ALReference hAL);
ALIST_API Boolean	ALCanPaste(ALReference hAL);
ALIST_API void		ALPaste(ALReference hAL);
ALIST_API void		ALDelete(ALReference hAL);


/* Routines for supporting heirarchical lists.

	These are only available if The A List was compiled with ALIST_HEIRARCHICAL set to one.
	See AListOptimizations.h for details.
*/
#if ALIST_HEIRARCHICAL
	ALIST_API OSErr	ALExpandRow(long rowNum, Boolean expandChildren, ALReference hAL);
	ALIST_API OSErr	ALCollapseRow(long rowNum, Boolean collapseChildren, ALReference hAL);
	ALIST_API Boolean	ALIsRowExpanded(long inSuperRowNum, ALReference hAL);
	ALIST_API Boolean	ALIsRowHidden( long inAnyRowNum, ALReference hAL );
	ALIST_API long		ALSuperRow( long subRow, ALReference hAL );
	ALIST_API long		ALAddRowUnder( long count, long superRow, ALReference hAL);
#endif


/* Routines for keyboard support. */
ALIST_API void		ALKey( SInt16 inCharCode,  EventModifiers inModifiers, unsigned long inKeyTime, ALReference hAL );


/* Routines for mouse support. */
ALIST_API void		ALGetCellFromItemRef(ItemReference theItem, ALCell *theCell);
ALIST_API Boolean	ALCanAcceptDrag(DragReference theDrag, ALReference hAL);
  ALIST_API Boolean	ALClick(Point mouseLoc0, Point mouseLoc, EventModifiers modifiers, unsigned long clickTime, ALReference hAL);
ALIST_API void		ALLastClick( ALCell *theCell, ALReference hAL );
ALIST_API OSErr	ALReceiveDrag(DragReference theDrag, ALReference hAL);
ALIST_API OSErr	ALTrackDrag(DragTrackingMessage theMessage, DragReference theDrag, ALReference hAL);


/* Routines for scrolling. */
ALIST_API Boolean	ALAutoScroll(Point moveTo, const ALCellPtr whichCell, ALReference hAL);
ALIST_API void		ALScrollCells(long dCols, long dRows, ALReference hAL);
ALIST_API void		ALScrollPixels(long hOffset, long vOffset, ALReference hAL);


/* Routines for selecting cells. */
ALIST_API short		ALGetCellAndEdge(Point thePoint, ALCellPtr cell, ALReference hAL);
ALIST_API RgnHandle	ALGetCellHiliteRgn(const ALCellPtr theCell, ALReference hAL);
ALIST_API long			ALGetNumberSelected(ALReference hAL);
ALIST_API RgnHandle	ALGetSelectedHiliteRgn(ALReference hAL);
ALIST_API Boolean		ALGetSelect(Boolean next, ALCell *theCell, ALReference hAL);
ALIST_API void			ALSetSelect(Boolean setIt, ALCellPtr theCell, ALReference hAL);
ALIST_API void			ALSetSelectNone(Boolean redrawIfChange, ALReference hAL);
ALIST_API void			ALSetSelectAll(Boolean redrawIfChange, ALReference hAL);


/* Routines for list features. */
ALIST_API OSErr			ALGetInfo(ALSelector selector, void *info, ALReference hAL);
ALIST_API OSErr			ALSetInfo(ALSelector selector, const void *info, ALReference hAL);
ALIST_API short			ALFeatureFlag(unsigned long feature, short action, ALReference hAL);
ALIST_API void				ALActivate(Boolean isActive, ALReference hAL);
ALIST_API ControlPartCode	ALSetFocus(ControlPartCode focusPart, ALReference hAL);
ALIST_API ControlPartCode	ALPartFocused(ALReference hAL);


/* Routine for creating and disposing of an Appearance user control that handles The A List.

	This is only available if The A List was compiled with ALIST_USEAPPEARANCEMGR set to one.
	See AListOptimizations.h for details.

*/
#if ALIST_USEAPPEARANCEMGR
	ALIST_API OSErr	ALMakeUserPaneControl( ALReference hAL, Boolean canFocus, ControlHandle *newCntl );
#endif


#ifdef __cplusplus
}
#endif

#endif /* __TheAList_h_ */
