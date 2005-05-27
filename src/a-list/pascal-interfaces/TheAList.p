{}
{ *	TheAList.p}
{ *}
{ *	The A List}
{ *	External interface for CodeWarrior Pascal.}
{ *}
{ *  Copyright (c) 1997-2000 Kyle Hammond}
{ *	All Rights Reserved}
{ *}
{}

unit TheAList;
interface

	uses
		Drag, LongCoords;


	const
{ result codes }
		alAddedNone = -1;					{ Could be returned from ALAddRow or ALAddCol }
		alEmptySelectionErr = errAENoUserSelection;	{ selection range is empty }
		alNotHandledErr = errAEEventNotHandled;	{ please use default handling }
		alUndefinedSelectorErr = paramErr;				{ unknown selector }
		alSkipDragItem = 129;					{ returned from ExtractFlaver function to not add this item }
		alAppearanceMgrNotInstalled = 130;		{ could be returned from ALMakeUserPaneControl }
		alNoDataErr = 131;							{ returned if the function requires the }
															{		ALIST_HAVE_CELLDATA flag to be 1 }

{ control character codes }
		kBackspace = $08;
		kTab = $09;
		kEOL = $0D;
		kArrowLeft = $1C;
		kArrowRight = $1D;
		kArrowUp = $1E;
		kArrowDown = $1F;
		kSpace = $20;
	{	kHome = $7300;
		kPageUp = $7400;
		kForwardDelete = $7F;
		kEnd = $7700;
		kPageDown = $7900;
	}

{ values for ALAllocate allocFlags parameter }
		kAllocClear = $0001;		{ clear handle after allocation }
		kAllocTemp = $0002;		{ use temporary memory if available }

{ values for ALFeatureFlag action parameter }
		alBitSet = 1;		{ enables the specified feature }
		alBitClear = 0;		{ disables the specified feature }
		alBitTest = -1;		{ returns the current setting of the specified feature }
		alBitToggle = -2;		{ toggles the specified feature }

{ Bit equates for the features field in the AL record }
		alFUseTempMem = 31;		{ use temporary memory for main data structures }
		alFVAutoScroll = 30;		{ automatically scroll text vertically when cursor is outside pane }
		alFVertScroll = 29;		{ has a vertical scroll bar }
		alFHAutoScroll = 28;		{ automatically scroll text horizontally when cursor is outside pane }
		alFHorzScroll = 27;		{ has a horizontal scroll bar }
		alFDynamicScroll = 26;		{ update list while the scroll bar thumbs are being moved }
		alFSelOnlyOne = 25;		{ only one selection at a time }
		alFSelExtendDrag = 24;		{ can drag extend selection without Shift key }
		alFSelNoDisjoint = 23;		{ turns off multiple selections with Command click }
		alFSelNoExtend = 22;		{ turns off extending with Shift key }
		alFSelNoRect = 21;		{ turns off extending as a rectangle }
		alFSelUseSense = 20;		{ Shift click extending uses sense of first cell }
		alFDrawFocus = 19;		{ draws or erases focus rectangle around the list (uses Appearance Manager if present) }
		alFInhibitRedraw = 18;		{ don't redraw }
		alFDrawOffscreen = 17;		{ draw text offscreen for smoother visual results }
		alFInhibitColor = 16;		{ use black and white only }
		alFStartDrags = 15;		{ can start a drag }
		alFReceiveDrags = 14;		{ can receive drags }
		alFDragToSelf = 13;		{ can receive drags from self. }
		alFOutlineHilite = 12;		{ frame selection range when pane is inactive }
		alFDrawRect = 11;		{ draw a 1 pixel wide black rectangle around the list }
		alFDrawLines = 10;		{ draw lines between cells }
		alFRowsOnly = 9;		{ the list has one column only }
		alFColumnsOnly = 8;		{ the list has one row only }
		alFHasGrow = 7;		{ leave room for a grow icon in bottom right }
		alFNotepadBackground = 6;		{ make the background look like a yellow notepad }
		alFAppearanceBackground = 5;	{ use the Appearance Manager list background }
		alFHeirarchical = 4;		{ True if there should be room in the left-most cell for a heirarchical triangle. }

{ Use with ALNew only. }
		alDoUseTempMem = $80000000;
		alDoVAutoScroll = $40000000;
		alDoVertScroll = $20000000;
		alDoHAutoScroll = $10000000;
		alDoHorzScroll = $08000000;
		alDoDynamicScroll = $04000000;
		alDoSelOnlyOne = $02000000;
		alDoSelExtendDrag = $01000000;
		alDoSelNoDisjoint = $00800000;
		alDoSelNoExtend = $00400000;
		alDoSelNoRect = $00200000;
		alDoSelUseSense = $00100000;
		alDoDrawFocus = $00080000;
		alDoInhibitRedraw = $00040000;
		alDoDrawOffscreen = $00020000;
		alDoInibitColor = $00010000;
		alDoStartDrags = $00008000;
		alDoReceiveDrags = $00004000;
		alDoDragToSelf = $00002000;
		alDoDrags = alDoStartDrags + alDoReceiveDrags;
		alDoOutlineHilite = $00001000;
		alDoDrawRect = $00000800;
		alDoDrawLines = $00000400;
		alDoRowsOnly = $00000200;
		alDoColumnsOnly = $00000100;
		alDoHasGrow = $00000080;
		alDoNotepadBg = $00000040;
		alDoAppearanceBg = $00000020;
		alDoHeirarchical = $00000010;

{ selectors for ALGetInfo/ALSetInfo }
		alRefCon = 'refc';
		alWindow = 'wind';
		alClickLoop = 'clik';
		alClickCell = 'CLIk';
		alDrawCellHook = 'draw';
		alDrawBackgroundHook = 'back';
		alHiliteCellHook = 'hili';
		alUserHandle = 'user';
		alVertScrollControl = 'ScrV';
		alVertScrollProc = 'scrv';
		alHorzScrollControl = 'ScrH';
		alHorzScrollProc = 'scrh';
		alCurrentDrag = 'drag';
		alDisposeCellDataHook = 'dcda';
		alInputFlavorsHook = 'Ifla';
		alOutputFlavorsHook = 'Ofla';
		alSendDataDragHook = 'sdrg';
		alStringSearchHook = 'strS';

{ other miscellaneous constants }
		kAL_CellMargin = 4;				{ width of border area surrounding the cell (in pixels) }
		kAL_MaxScrollDelta = 7;			{ maximum scroll amount used by standard click loop }
		kScrollBarWidth = 16;			{ width of a scroll bar in pixels }
		kAL_AutoScrollDelay = 10;		{ delay before auto-scroll starts (in ticks) }
		kAL_TypeText = 'TEXT';			{ The default drag type }
		kAL_TypeListData = 'ALST';		{ The default group data type for the clipboard. }

{ values for caret location parameter (ALGetCellAndEdge) }
		kCaretTop = 1;
		kCaretBottom = -1;
		kCaretWholeCell = 0;
		kCaretLeft = 2;
		kCaretRight = -2;
		kCaretNotInCell = -3;

{ forward declarations }

	type
		OpaqueALReference = packed record
			end;
		ALReference = ^OpaqueALReference;

		ALSelector = FourCharCode;

{ ALData is an opaque type }
		OpaqueALData = packed record
			end;
		ALData = ^OpaqueALData;
		ALDataPtr = ^ALData;
		ALDataHandle = ^ALDataPtr;

		ALCell = LongPt;
		ALCellPtr = ^LongPt;

{ callback prototypes }

{$IFC PROCTYPE}
		ALClickLoopProcPtr = FUNCTION (hAL: ALReference): BOOLEAN;
{$ELSEC}
		ALClickLoopProcPtr = ProcPtr;
{$ENDC}
		ALClickLoopUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALScrollProcPtr = PROCEDURE (hAL: ALReference);
{$ELSEC}
		ALScrollProcPtr = ProcPtr;
{$ENDC}
		ALScrollUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALClickCellProcPtr = FUNCTION ({CONST}VAR cell: ALCell; mouseLoc: Point; modifiers: EventModifiers; numberClicks: INTEGER; hAL: ALReference): BOOLEAN;
{$ELSEC}
		ALClickCellProcPtr = ProcPtr;
{$ENDC}
		ALClickCellUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALInputFlavorsProcPtr = FUNCTION (index: INTEGER; VAR more: BOOLEAN; VAR flavorDesc: AEDesc; cellDataPtr: ALDataPtr; {CONST}VAR cell: ALCell; hAL: ALReference): OSErr;
{$ELSEC}
		ALInputFlavorsProcPtr = ProcPtr;
{$ENDC}
		ALInputFlavorsUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALOutputFlavorsProcPtr = FUNCTION (index: INTEGER; VAR more: BOOLEAN; forDrag: BOOLEAN; VAR flavorDesc: AEDesc; cellData: ALData; {CONST}VAR cell: ALCell; hAL: ALReference): OSErr;
{$ELSEC}
		ALOutputFlavorsProcPtr = ProcPtr;
{$ENDC}
		ALOutputFlavorsUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALDisposeCellDataProcPtr = PROCEDURE (cellData: ALData; {CONST}VAR cell: ALCell; hAL: ALReference);
{$ELSEC}
		ALDisposeCellDataProcPtr = ProcPtr;
		ALDisposeCellDataUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALDrawCellProcPtr = PROCEDURE (cellData: ALData; {CONST}VAR cell: ALCell; {CONST}VAR cellRect: Rect; hAL: ALReference);
{$ELSEC}
		ALDrawCellProcPtr = ProcPtr;
{$ENDC}
		ALDrawCellUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALDrawBackgroundProcPtr = PROCEDURE ({CONST}VAR cellRect: Rect; hAL: ALReference);
{$ELSEC}
		ALDrawBackgroundProcPtr = ProcPtr;
{$ENDC}
		ALDrawBackgroundUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALHiliteCellProcPtr = PROCEDURE (cellData: ALData; {CONST}VAR cell: ALCell; active: BOOLEAN; doOutline: BOOLEAN; {CONST}VAR cellRect: Rect; hAL: ALReference);
{$ELSEC}
		ALHiliteCellProcPtr = ProcPtr;
{$ENDC}
		ALHiliteCellUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALSearchProcPtr = FUNCTION (cellData: ALData; searchData: ALData): BOOLEAN;
{$ELSEC}
		ALSearchProcPtr = ProcPtr;
{$ENDC}
		ALSearchUPP = UniversalProcPtr;

{$IFC PROCTYPE}
		ALStringSearchProcPtr = FUNCTION (theString: StringHandle; VAR cell: ALCell; hAL: ALReference): BOOLEAN;
{$ELSEC}
		ALStringSearchProcPtr = ProcPtr;
{$ENDC}
		ALStringSearchUPP = UniversalProcPtr;

{ UPP proc info }
	const
		uppALClickLoopProcInfo = 208;
		uppALScrollProcInfo = 192;
		uppALClickCellProcInfo = 60368;
		uppALInputFlavorsProcInfo = 262048;
		uppALOutputFlavorsProcInfo = 1046432;
		uppALDisposeCellDataProcInfo = 4032;
		uppALDrawCellProcInfo = 16320;
		uppALDrawBackgroundProcInfo = 960;
		uppALHiliteCellProcInfo = 251840;
		uppALSearchProcInfo = 976;
		uppALStringSearchProcInfo = 4048;

{ NewProc macros }
	function NewALClickLoopProc (userRoutine: ALClickLoopProcPtr): ALClickLoopUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALScrollProc (userRoutine: ALScrollProcPtr): ALScrollUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALClickCellProc (userRoutine: ALClickCellProcPtr): ALClickCellUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALInputFlavorsProc (userRoutine: ALInputFlavorsProcPtr): ALInputFlavorsUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALOutputFlavorsProc (userRoutine: ALOutputFlavorsProcPtr): ALOutputFlavorsUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALDisposeCellDataProc (userRoutine: ALDisposeCellDataProcPtr): ALDisposeCellDataUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALDrawCellProc (userRoutine: ALDrawCellProcPtr): ALDrawCellUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALDrawBackgroundProc (userRoutine: ALDrawBackgroundProcPtr): ALDrawBackgroundUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALHiliteCellProc (userRoutine: ALHiliteCellProcPtr): ALHiliteCellUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALSearchProc (userRoutine: ALSearchProcPtr): ALSearchUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

	function NewALStringSearchProc (userRoutine: ALStringSearchProcPtr): ALStringSearchUPP;
	{$IFC NOT GENERATINGCFM}
	inline
		$2E9F;
	{$ENDC}

{ CallProc macros }

	function CallALClickLoopProc (hAL: ALReference;
									userRoutine: ALClickLoopUPP): BOOLEAN;
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	procedure CallALScrollProc (hAL: ALReference;
									userRoutine: ALScrollUPP);
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	function CallALClickCellProc ({CONST}VAR cell: ALCell;
									mouseLoc: Point;
									modifiers: EventModifiers;
									numberClicks: INTEGER;
									hAL: ALReference;
									userRoutine: ALClickCellUPP): BOOLEAN;
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	function CallALInputFlavorsProc (index: INTEGER;
									var more: BOOLEAN;
									var flavorDesc: AEDesc;
									cellDataPtr: ALDataPtr;
									{CONST}VAR cell: ALCell;
									hAL: ALReference;
									userRoutine: ALInputFlavorsUPP): OSErr;
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	function CallALOutputFlavorsProc (index: INTEGER;
									var more: BOOLEAN;
									forDrag: BOOLEAN;
									var flavorDesc: AEDesc;
									cellData: ALData;
									{CONST}VAR cell: ALCell;
									hAL: ALReference;
									userRoutine: ALOutputFlavorsUPP): OSErr;
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	procedure CallALDisposeCellDataProc (cellData: ALData;
									{CONST}VAR cell: ALCell;
									hAL: ALReference;
									userRoutine: ALDisposeCellDataUPP);
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	procedure CallALDrawCellProc (cellData: ALData;
									{CONST}VAR cell: ALCell;
									{CONST}VAR cellRect: Rect;
									hAL: ALReference;
									userRoutine: ALDrawCellUPP);
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	procedure CallALDrawBackgroundProc ({CONST}VAR cellRect: Rect;
									hAL: ALReference;
									userRoutine: ALDrawBackgroundUPP);
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	procedure CallALHiliteCellProc (cellData: ALData;
									{CONST}VAR cell: ALCell;
									active: BOOLEAN;
									doOutline: BOOLEAN;
									{CONST}VAR cellRect: Rect;
									hAL: ALReference;
									userRoutine: ALHiliteCellUPP);
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	function CallALSearchProc (cellData: ALData;
									searchData: ALData;
									userRoutine: ALSearchUPP): BOOLEAN;
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

	function CallALStringSearchProc (theString: StringHandle;
									var cell: ALCell;
									hAL: ALReference;
									userRoutine: ALStringSearchUPP): BOOLEAN;
	{$IFC NOT GENERATINGCFM}
	inline
		$205F, $4E90;
	{$ENDC}

{ Function prototypes. }

{ Routines for creating and disposing of a list. }
	function ALVersion: INTEGER;
	function ALNew (wr: WindowPtr;
									{CONST}VAR viewRect: Rect;
									{CONST}VAR dataBounds: LongRect;
									cellSize: Point;
									features: LONGINT;
									var hAL: ALReference): OSErr;
	procedure ALDispose (hAL: ALReference);
	procedure ALSetViewRect ({CONST}VAR viewRect: Rect;
									hAL: ALReference);
	procedure ALGetViewRect (var viewRect: Rect;
									hAL: ALReference);


{ Routines for getting and setting data in a list. }
	function ALClearCell ({CONST}VAR theCell: ALCell;
									hAL: ALReference): OSErr;
	function ALGetCell (var dataPtr: ALData;
									{CONST}VAR theCell: ALCell;
									hAL: ALReference): OSErr;
	procedure ALGetCellRect (var cellRect: Rect;
									{CONST}VAR theCell: ALCell;
									hAL: ALReference);
	procedure ALSetCellSize (cSize: Point;
									hAL: ALReference);
	function ALNextCell (hNext: BOOLEAN;
									vNext: BOOLEAN;
									var theCell: ALCell;
									hAL: ALReference): BOOLEAN;
	function ALSetCell (data: ALData;
									{CONST}VAR theCell: ALCell;
									hAL: ALReference): OSErr;
	function ALSearch (searchData: ALData;
									searchProc: ALSearchUPP;
									{CONST}VAR theCell: ALCell;
									hAL: ALReference): BOOLEAN;
	function ALAddColumn (count: LONGINT;
									afterColNum: LONGINT;
									hAL: ALReference): LONGINT;
	procedure ALDelColumn (count: LONGINT;
									startColNum: LONGINT;
									hAL: ALReference);
	function ALAddRow (count: LONGINT;
									afterRowNum: LONGINT;
									hAL: ALReference): LONGINT;
	procedure ALDelRow (count: LONGINT;
									startRowNum: LONGINT;
									hAL: ALReference);
	function ALGetNumberRows (hAL: ALReference): LONGINT;
	function ALGetNumberColumns (hAL: ALReference): LONGINT;
	function ALIsVisible ({CONST}VAR theCell: ALCell;
									hAL: ALReference): BOOLEAN;


{ Routines for drawing lists. }
	procedure ALDrawCell ({CONST}VAR theCell: ALCell;
									hAL: ALReference);
	procedure ALUpdate (updateRgn: RgnHandle;
									hAL: ALReference);


{ Routines for editing support. }
	function ALCanUndo (var isRedo: BOOLEAN;
									hAL: ALReference): BOOLEAN;
	procedure ALUndo (hAL: ALReference);
	procedure ALCut (hAL: ALReference);
	procedure ALCopy (hAL: ALReference);
	function ALCanPaste (hAL: ALReference): BOOLEAN;
	procedure ALPaste (hAL: ALReference);
	procedure ALDelete (hAL: ALReference);


{ Routines for supporting heirarchical lists. }
	function ALExpandRow (rowNum: Longint;
									expandChildren: Boolean;
									hAL: ALReference): OSErr;
	function ALCollapseRow (rowNum: Longint;
									collapseChildren: Boolean;
									hAL: ALReference): OSErr;
	function ALIsRowExpanded (rowNum: Longint;
									hAL: ALReference): Boolean;
	function ALSuperRow (subRow: Longint;
									hAL: ALReference): Longint;
	function ALAddRowUnder (count: Longint;
									superRow: Longint;
									hAL: ALReference): Longint;


{ Routines for keyboard support. }
	procedure ALKey (keyCode: SInt16;
									modifiers: EventModifiers;
									keyTime: UInt32;
									hAL: ALReference);


{ Routines for mouse support. }
	procedure ALGetCellFromItemRef (theItem: ItemReference;
									var theCell: ALCell);
	function ALCanAcceptDrag (theDrag: DragReference;
									hAL: ALReference): BOOLEAN;
	function ALClick (mouseLoc: Point;
									modifiers: EventModifiers;
									clickTime: LONGINT;
									hAL: ALReference): BOOLEAN;
	procedure ALLastClick (var theCell: ALCell;
									hAL: ALReference);
	function ALReceiveDrag (theDrag: DragReference;
									hAL: ALReference): OSErr;
	function ALTrackDrag (theMessage: DragTrackingMessage;
									theDrag: DragReference;
									hAL: ALReference): OSErr;


{ Routines for scrolling. }
	function ALAutoScroll (moveTo: Point;
									whichCell: ALCellPtr;
									hAL: ALReference): BOOLEAN;
	procedure ALScrollCells (dCols: LONGINT;
									dRows: LONGINT;
									hAL: ALReference);
	procedure ALScrollPixels (hOffset: LONGINT;
									vOffset: LONGINT;
									hAL: ALReference);


{ Routines for selecting cells. }
	function ALGetCellAndEdge (thePoint: Point;
									var cell: ALCell;
									hAL: ALReference): INTEGER;
	function ALGetCellHiliteRgn ({CONST}VAR theCell: ALCell;
									hAL: ALReference): RgnHandle;
	function ALGetNumberSelected (hAL: ALReference): LONGINT;
	function ALGetSelectedHiliteRgn (hAL: ALReference): RgnHandle;
	function ALGetSelect (next: BOOLEAN;
									var theCell: ALCell;
									hAL: ALReference): BOOLEAN;
	procedure ALSetSelect (setIt: BOOLEAN;
									{CONST}VAR theCell: ALCell;
									hAL: ALReference);
	procedure ALSetSelectNone (redrawIfChange: BOOLEAN;
									hAL: ALReference);
	procedure ALSetSelectAll (redrawIfChange: BOOLEAN;
									hAL: ALReference);


{ Routines for list features. }
	function ALGetInfo (selector: ALSelector;
									info: univ Ptr;
									hAL: ALReference): OSErr;
	function ALSetInfo (selector: ALSelector;
									info: univ Ptr;
									hAL: ALReference): OSErr;
	function ALFeatureFlag (feature: LONGINT;
									action: INTEGER;
									hAL: ALReference): INTEGER;
	procedure ALActivate (isActive: BOOLEAN;
									hAL: ALReference);
	function ALSetFocus (focusPart: ControlPartCode;
									hAL: ALReference): ControlPartCode;
	function ALPartFocused (hAL: ALReference): ControlPartCode;


{ Routine for creating and disposing of an Appearance user control that handles The A List. }
	function ALMakeUserPaneControl (hAL: ALReference;
									canFocus: Boolean;
									var newCntl: ControlHandle): OSErr;


implementation


{ NewProc macro implementations for CFM. If someone knows how to turn these into macros I'll be happy... }
{$IFC GENERATINGCFM}
	function NewALClickLoopProc (userRoutine: ALClickLoopProcPtr): ALClickLoopUPP;
	begin
		NewALClickLoopProc := ALClickLoopUPP(NewRoutineDescriptor(userRoutine, uppALClickLoopProcInfo, GetCurrentISA));
	end;

	function NewALScrollProc (userRoutine: ALScrollProcPtr): ALScrollUPP;
	begin
		NewALScrollProc := ALScrollUPP(NewRoutineDescriptor(userRoutine, uppALScrollProcInfo, GetCurrentISA));
	end;

	function NewALClickCellProc (userRoutine: ALClickCellProcPtr): ALClickCellUPP;
	begin
		NewALClickCellProc := ALClickCellUPP(NewRoutineDescriptor(userRoutine, uppALClickCellProcInfo, GetCurrentISA));
	end;

	function NewALInputFlavorsProc (userRoutine: ALInputFlavorsProcPtr): ALInputFlavorsUPP;
	begin
		NewALInputFlavorsProc := ALInputFlavorsUPP(NewRoutineDescriptor(userRoutine, uppALInputFlavorsProcInfo, GetCurrentISA));
	end;

	function NewALOutputFlavorsProc (userRoutine: ALOutputFlavorsProcPtr): ALOutputFlavorsUPP;
	begin
		NewALOutputFlavorsProc := ALOutputFlavorsUPP(NewRoutineDescriptor(userRoutine, uppALOutputFlavorsProcInfo, GetCurrentISA));
	end;

	function NewALDisposeCellDataProc (userRoutine: ALDisposeCellDataProcPtr): ALDisposeCellDataUPP;
	begin
		NewALDisposeCellDataProc := ALDisposeCellDataUPP(NewRoutineDescriptor(userRoutine, uppALDisposeCellDataProcInfo, GetCurrentISA));
	end;

	function NewALDrawCellProc (userRoutine: ALDrawCellProcPtr): ALDrawCellUPP;
	begin
		NewALDrawCellProc := ALDrawCellUPP(NewRoutineDescriptor(userRoutine, uppALDrawCellProcInfo, GetCurrentISA));
	end;

	function NewALDrawBackgroundProc (userRoutine: ALDrawBackgroundProcPtr): ALDrawBackgroundUPP;
	begin
		NewALDrawBackgroundProc := ALDrawBackgroundUPP(NewRoutineDescriptor(userRoutine, uppALDrawBackgroundProcInfo, GetCurrentISA));
	end;

	function NewALHiliteCellProc (userRoutine: ALHiliteCellProcPtr): ALHiliteCellUPP;
	begin
		NewALHiliteCellProc := ALHiliteCellUPP(NewRoutineDescriptor(userRoutine, uppALHiliteCellProcInfo, GetCurrentISA));
	end;

	function NewALSearchProc (userRoutine: ALSearchProcPtr): ALSearchUPP;
	begin
		NewALSearchProc := ALSearchUPP(NewRoutineDescriptor(userRoutine, uppALSearchProcInfo, GetCurrentISA));
	end;

	function NewALStringSearchProc (userRoutine: ALStringSearchProcPtr): ALStringSearchUPP;
	begin
		NewALStringSearchProc := ALStringSearchUPP(NewRoutineDescriptor(userRoutine, uppALStringSearchProcInfo, GetCurrentISA));
	end;

{ CallProc macro implementations. Same as above. }

	function CallALClickLoopProc (hAL: univ ALReference;
									userRoutine: ALClickLoopUPP): BOOLEAN;
	begin
		CallALClickLoopProc := CallUniversalProc(userRoutine, uppALClickLoopProcInfo, hAL) <> 0;
	end;

	procedure CallALScrollProc (hAL: ALReference;
									userRoutine: ALScrollUPP);
	var junk: LongInt;
	begin
		junk := CallUniversalProc(userRoutine, uppALScrollProcInfo, hAL);
	end;

	function CallALClickCellProc ({CONST}VAR cell: ALCell;
									mouseLoc: Point;
									modifiers: EventModifiers;
									numberClicks: INTEGER;
									hAL: ALReference;
									userRoutine: ALClickCellUPP): BOOLEAN;
	begin
		CallALClickCellProc := CallUniversalProc(userRoutine, uppALClickCellProcInfo, cell, mouseLoc, modifiers, numberClicks, hAL) <> 0;
	end;

	function CallALInputFlavorsProc (index: INTEGER;
									var more: BOOLEAN;
									var flavorDesc: AEDesc;
									cellDataPtr: ALDataPtr;
									{CONST}VAR cell: ALCell;
									hAL: ALReference;
									userRoutine: ALInputFlavorsUPP): OSErr;
	begin
		CallALInputFlavorsProc := CallUniversalProc(userRoutine, uppALInputFlavorsProcInfo, index, more, flavorDesc, cellDataPtr, cell, hAL);
	end;

	function CallALOutputFlavorsProc (index: INTEGER;
									var more: BOOLEAN;
									forDrag: BOOLEAN;
									var flavorDesc: AEDesc;
									cellData: ALData;
									{CONST}VAR cell: ALCell;
									hAL: ALReference;
									userRoutine: ALOutputFlavorsUPP): OSErr;
	begin
		CallALOutputFlavorsProc := CallUniversalProc(userRoutine, uppALOutputFlavorsProcInfo, index, more, forDrag, flavorDesc, cellData, cell, hAL);
	end;

	procedure CallALDisposeCellDataProc (cellData: ALData;
									{CONST}VAR cell: ALCell;
									hAL: ALReference;
									userRoutine: ALDisposeCellDataUPP);
	var junk: LongInt;
	begin
		junk := CallUniversalProc(userRoutine, uppALDisposeCellDataProcInfo, cellData, cell, hAL);
	end;

	procedure CallALDrawCellProc (cellData: ALData;
									{CONST}VAR cell: ALCell;
									{CONST}VAR cellRect: Rect;
									hAL: ALReference;
									userRoutine: ALDrawCellUPP);
	var junk: LongInt;
	begin
		junk := CallUniversalProc(userRoutine, uppALDrawCellProcInfo, cellData, cell, cellRect, hAL);
	end;

	procedure CallALDrawBackgroundProc ({CONST}VAR cellRect: Rect;
									hAL: ALReference;
									userRoutine: ALDrawBackgroundUPP);
	var junk: LongInt;
	begin
		junk := CallUniversalProc(userRoutine, uppALDrawBackgroundProcInfo, cellRect, hAL);
	end;

	procedure CallALHiliteCellProc (cellData: ALData;
									{CONST}VAR cell: ALCell;
									active: BOOLEAN;
									doOutline: BOOLEAN;
									{CONST}VAR cellRect: Rect;
									hAL: ALReference;
									userRoutine: ALHiliteCellUPP);
	var junk: LongInt;
	begin
		junk := CallUniversalProc(userRoutine, uppALHiliteCellProcInfo, cellData, cell, active, doOutline, cellRect, hAL);
	end;

	function CallALSearchProc (cellData: ALData;
									searchData: ALData;
									userRoutine: ALSearchUPP): BOOLEAN;
	begin
		CallALSearchProc := CallUniversalProc(userRoutine, uppALSearchProcInfo, cellData, searchData) <> 0;
	end;

	function CallALStringSearchProc (theString: StringHandle;
									var cell: ALCell;
									hAL: ALReference;
									userRoutine: ALStringSearchUPP): BOOLEAN;
	begin
		CallALStringSearchProc := CallUniversalProc(userRoutine, uppALStringSearchProcInfo, theString, cell, hAL) <> 0;
	end;
{$ENDC}
	
end.