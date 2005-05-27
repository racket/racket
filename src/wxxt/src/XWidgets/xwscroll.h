/*
 * File: <scroll.h>
 *
 *
 * Declarations and typedefs for the Free Widget Foundation
 * Scrolling Widget Interface Policy.
 */

#ifndef _XFWF_SCROLL_H
#define _XFWF_SCROLL_H 1

/*
 * The XfwfSReason typedef defines all the possible types
 * of scroll messages.  XfwfSNotify indicates a _notify_
 * message, and all others indicate a _command_ message.
 */
typedef enum _XfwfSReason
{
    XfwfSNotify,        /* Widget has changed position or size */

    XfwfSMove,          /* Request to move widget */
    XfwfSDrag,          /* widget is being moved continuously */

    XfwfSZoom,       	/* Request to Zoom (resize visible area) */
    XfwfSStretch,       /* widget is being zoomed continuously */

    XfwfSUp,            /* User request to ``move up one unit'' */
    XfwfSLeft,          /* User request to ``move left one unit'' */
    XfwfSDown, XfwfSRight,      /* similar */

    XfwfSPageUp,        /* User request to ``move up one page'' */
    XfwfSPageLeft, XfwfSPageDown, XfwfSPageRight,       /* similar */

    XfwfSZoomIn,        /* User invoked ``Zoom In'' */
    XfwfSZoomOut,       /* User invoked ``Zoom Out'' */

    XfwfSTop,		/* User invoked ``Scroll to top'' */
    XfwfSBottom, XfwfSLeftSide, XfwfSRightSide, /* similar */

    XfwfSZoomInFull, XfwfSZoomOutFull  /* similar, but wrt zoom state */

} XfwfSReason;

/* =

 * #define's for the 'flags' field:
 */
typedef unsigned short XfwfSFlags;
#define XFWF_VPOS       0x1     /* vpos set */
#define XFWF_VSIZE      0x2     /* vsize set */
#define XFWF_HPOS       0x4     /* hpos set */
#define XFWF_HSIZE      0x8     /* hsize set */

/*
 * The XfwfScrollInfo structure defines the scroll message passed
 * between scrolling widgets:
 */
typedef struct _XfwfScrollInfo
{
        XfwfSReason     reason;         /* see above */
        XfwfSFlags      flags;          /* defines which fields are relevant */
        float           vpos;           /* "top" position, [0..1] */
        float           vsize;          /* total visible vertical size [0..1] */
        float           hpos;           /* "left" position */
        float           hsize;          /* total visible horizontal size */
	Position	gx, gy;		/* position computed by ScrollWin::scroll_callback */
} XfwfScrollInfo;


/*
 * Application convenience functions:
 *
 * XfwfConnectScrollingWidgets(Widget, Widget) attaches two
 * widgets, which must have the scrollCallback and scrollResponse
 * resources.
 */

extern void XfwfConnectScrollingWidgets( /* Widget w1, Widget w2 */);


/*
 * Widget convenience functions:
 *
 * =

 * XcwfCvtStringToScrollReason(char *s) converts 's' to one
 * of the XfwfS* enumerated values.  The comparison is case-insensitive,
 * and the XfwfS prefix may be present but is not necessary.
 */

extern XfwfSReason XfwfCvtStringToScrollReason(/* char * */);

#endif /* _XFWF_SCROLL_H */
