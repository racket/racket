/*
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xmu/CharSet.h>
#include <xwscroll.h>

void XfwfConnectScrollingWidgets(w1, w2)
    Widget w1, w2;
{
    XtCallbackProc response_cb_1 = NULL, response_cb_2 = NULL;

    /* Error checking omitted */
    XtVaGetValues(w1, "scrollResponse", &response_cb_1, NULL);
    XtVaGetValues(w2, "scrollResponse", &response_cb_2, NULL);

    XtAddCallback(w1, "scrollCallback", response_cb_2, (XtPointer)w2);

    XtAddCallback(w2, "scrollCallback", response_cb_1, (XtPointer)w1);
}

XfwfSReason XfwfCvtStringToScrollReason(s)
    String s;
{
    if (XmuCompareISOLatin1(s, "Notify") == 0) return XfwfSNotify;
    if (XmuCompareISOLatin1(s, "Move") == 0) return XfwfSMove;
    if (XmuCompareISOLatin1(s, "Drag") == 0) return XfwfSDrag;
    if (XmuCompareISOLatin1(s, "Zoom") == 0) return XfwfSZoom;
    if (XmuCompareISOLatin1(s, "Stretch") == 0) return XfwfSStretch;
    if (XmuCompareISOLatin1(s, "Up") == 0) return XfwfSUp;
    if (XmuCompareISOLatin1(s, "Down") == 0) return XfwfSDown;
    if (XmuCompareISOLatin1(s, "Left") == 0) return XfwfSLeft;
    if (XmuCompareISOLatin1(s, "Right") == 0) return XfwfSRight;
    if (XmuCompareISOLatin1(s, "PageUp") == 0) return XfwfSPageUp;
    if (XmuCompareISOLatin1(s, "PageDown") == 0) return XfwfSPageDown;
    if (XmuCompareISOLatin1(s, "PageLeft") == 0) return XfwfSPageLeft;
    if (XmuCompareISOLatin1(s, "PageRight") == 0) return XfwfSPageRight;
    if (XmuCompareISOLatin1(s, "ZoomIn") == 0) return XfwfSZoomIn;
    if (XmuCompareISOLatin1(s, "ZoomOut") == 0) return XfwfSZoomOut;
    if (XmuCompareISOLatin1(s, "Top") == 0) return XfwfSTop;
    if (XmuCompareISOLatin1(s, "Bottom") == 0) return XfwfSBottom;
    if (XmuCompareISOLatin1(s, "LeftSide") == 0) return XfwfSLeftSide;
    if (XmuCompareISOLatin1(s, "RightSide") == 0) return XfwfSRightSide;
    if (XmuCompareISOLatin1(s, "ZoomInFull") == 0) return XfwfSZoomInFull;
    if (XmuCompareISOLatin1(s, "ZoomOutFull") == 0) return XfwfSZoomOutFull;
    return XfwfSNotify;				/* Should be: error */
}

