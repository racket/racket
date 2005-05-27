//	DragManagerAdditions.c

//#define SystemSevenFiveOrLater  1
//#define CGLUESUPPORTED          0
//#define OLDROUTINENAMES         0
//#define OLDROUTINELOCATIONS     0
//#define STRICT_WINDOWS          1

#if !defined(UNIVERSAL_INTERFACES_VERSION) || (UNIVERSAL_INTERFACES_VERSION < 0x0300)

#if GENERATINGCFM

    //
    // If we're not generating CFM, then assume the
    // 68K inlines in the headers apply instead.
    //

#include "DragManagerAdditions.h"
    // if missing, see Appendix A1, Technote 1043

pascal OSErr SetDragImage ( DragReference   theDragRef,
                            PixMapHandle    imagePixMap,
                            RgnHandle       imageRgn,
                            Point           imageOffsetPt,
                            DragImageFlags  theImageFlags   )
{
    enum
    {
        uppSetDragImageInfo = kD0DispatchedPascalStackBased
            | RESULT_SIZE (SIZE_CODE (sizeof(OSErr)))
            | DISPATCHED_STACK_ROUTINE_SELECTOR_SIZE
                  (SIZE_CODE (sizeof (unsigned long)))
            | DISPATCHED_STACK_ROUTINE_PARAMETER
                  (1, SIZE_CODE (sizeof (theDragRef)))
            | DISPATCHED_STACK_ROUTINE_PARAMETER
                  (2, SIZE_CODE (sizeof (imagePixMap)))
            | DISPATCHED_STACK_ROUTINE_PARAMETER
                  (3, SIZE_CODE (sizeof (imageRgn)))
            | DISPATCHED_STACK_ROUTINE_PARAMETER
                  (4, SIZE_CODE (sizeof (imageOffsetPt)))
            | DISPATCHED_STACK_ROUTINE_PARAMETER
                  (5, SIZE_CODE (sizeof (theImageFlags)))
    };

    return CallUniversalProc (
        GetToolTrapAddress (_DragDispatch),
        uppSetDragImageInfo, 0x27L, theDragRef, imagePixMap,
        imageRgn, imageOffsetPt, theImageFlags);
}

pascal OSErr GetDragHiliteColor (WindowPtr window, RGBColor *color)
{
    enum
    {
        uppGetDragHiliteColorInfo =
            kD0DispatchedPascalStackBased
                | RESULT_SIZE (SIZE_CODE (sizeof(OSErr)))
                | DISPATCHED_STACK_ROUTINE_SELECTOR_SIZE
                    (SIZE_CODE (sizeof (unsigned long)))
                | DISPATCHED_STACK_ROUTINE_PARAMETER
                    (1, SIZE_CODE (sizeof (window)))
                | DISPATCHED_STACK_ROUTINE_PARAMETER
                    (2, SIZE_CODE (sizeof (color)))
    };

    return CallUniversalProc (
        GetToolTrapAddress (_DragDispatch),
            uppGetDragHiliteColorInfo, 0x26L, window, color);
}

#endif // GENERATINGCFM

#endif	// old Universal Interfaces version