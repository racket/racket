/** zyz3d.c
 *
 *  DESCRIPTION: 
 *      This module contains functions for creating nifty 3D borders
 *      around controls like zYzGauge.
 *
 *  HISTORY:
 *      3/14/91     cjp     put in this comment
 *      6/19/92     cjp     touched it a bit
 *
 ** cjp */
// COPYRIGHT:
//
//   (C) Copyright Microsoft Corp. 1992.  All rights reserved.
//
//   You have a royalty-free right to use, modify, reproduce and
//   distribute the Sample Files (and/or any modified version) in
//   any way you find useful, provided that you agree that
//   Microsoft has no warranty obligations or liability for any
//   Sample Application Files which are modified.
//


/* get the includes we need */
#include <windows.h>
#include "zyz3d.h"


/** void FAR PASCAL Draw3DFaceFrame(HDC hdc, LPRECT rc, WORD wWidth)
 *
 *  DESCRIPTION: 
 *      This function draws a flat frame with the current button-face
 *      color.
 *
 *  ARGUMENTS:
 *      HDC hdc     :   The DC to draw into.
 *
 *      LPRECT rc   :   The containing rect for the new frame.
 *
 *      WORD wWidth :   The width of the frame to draw.
 *
 *  RETURN (void FAR PASCAL):
 *      The frame will have been drawn into the DC.
 *
 *  NOTES:
 *
 ** cjp */

void FAR PASCAL Draw3DFaceFrame(HDC hdc, LPRECT rc, WORD wWidth)
{
    RECT    rc1;
    DWORD   rgbOld;

    /* don't go through a bunch of work if we don't have to */
    if (!wWidth)
        return;

    /* set up color to be button-face color--so it may not be gray */
    rgbOld = SetBkColor(hdc, GetSysColor(COLOR_BTNFACE));

    /* perform CopyRect w/o bloody windows style overhead */
    rc1 = *rc;

    /* top */
    rc1.top = rc->top;
    rc1.left = rc->left;
    rc1.bottom = rc->top + wWidth;
    rc1.right = rc->right;

    /* blast it out */
    ExtTextOut(hdc, rc1.left, rc1.top, ETO_OPAQUE, &rc1, NULL, 0, NULL);

    /* right */
    rc1.left = rc->right - wWidth;
    rc1.bottom = rc->bottom;

    /* blast this part now */
    ExtTextOut(hdc, rc1.left, rc1.top, ETO_OPAQUE, &rc1, NULL, 0, NULL);

    /* left */
    rc1.left = rc->left;
    rc1.right = rc->left + wWidth;

    /* and another part */
    ExtTextOut(hdc, rc1.left, rc1.top, ETO_OPAQUE, &rc1, NULL, 0, NULL);

    /* bottom */
    rc1.right = rc->right;
    rc1.top = rc->bottom - wWidth;

    /* finish it off */
    ExtTextOut(hdc, rc1.left, rc1.top, ETO_OPAQUE, &rc1, NULL, 0, NULL);

    /* restore the old bk color */
    SetBkColor(hdc, rgbOld);
} /* Draw3DFaceFrame() */


/** void FAR PASCAL Draw3DRect(HDC, LPRECT, WORD, WORD)
 *
 *  DESCRIPTION: 
 *      Draws a 3D rectangle that is shaded.  wFlags can be used to
 *      control how the rectangle looks.
 *
 *  ARGUMENTS:
 *      HDC hdc             :   Handle to the device context that will be
 *                              used to display the rectangle.
 *
 *      RECT rect           :   A rectangle describing the dimensions of
 *                              the rectangle in device coordinates.
 *
 *      WORD wShadowWidth   :   Width of the shadow in device coordinates.
 *
 *      WORD wFlags         :   The following flags may be passed to describe
 *                              the style of the rectangle:
 *
 *                              DRAW3D_IN   :   The shadow is drawn such that
 *                              the box appears to be sunk in to the screen.
 *                              This is default if 0 is passed.
 *
 *                              DRAW3D_OUT  :   The shadow is drawn such that
 *                              the box appears to be sticking out of the
 *                              screen.
 *
 *  RETURN (void FAR PASCAL):
 *      The 3D looking rectangle will have been drawn into the DC.
 *
 *  NOTES:
 *
 ** cjp */

void FAR PASCAL Draw3DRect(HDC hdc, LPRECT lpRect,
                               WORD wShadowWidth, WORD wFlags)
{
    /* sanity check--don't work if you don't have to! */
    if (!wShadowWidth || !RectVisible(hdc, lpRect))
        return;

    /* draw the top line */
    Draw3DLine(hdc, lpRect->left, lpRect->top,
                    lpRect->right - lpRect->left,
                    wShadowWidth, DRAW3D_TOPLINE | wFlags);

    /* right line */
    Draw3DLine(hdc, lpRect->right, lpRect->top,
                    lpRect->bottom - lpRect->top,
                    wShadowWidth, DRAW3D_RIGHTLINE | wFlags);

    /* bottom line */
    Draw3DLine(hdc, lpRect->left, lpRect->bottom,
                    lpRect->right - lpRect->left,
                    wShadowWidth, DRAW3D_BOTTOMLINE | wFlags);

    /* left line */
    Draw3DLine(hdc, lpRect->left, lpRect->top,
                    lpRect->bottom - lpRect->top,
                    wShadowWidth, DRAW3D_LEFTLINE | wFlags);
} /* Draw3DRect() */


/** void FAR PASCAL Draw3DLine(HDC hdc, WORD x, WORD y, WORD nLen,
 *
 *  DESCRIPTION: 
 *      Draws a 3D line that can be used to make a 3D box.
 *
 *  ARGUMENTS:
 *      HDC hdc             :   Handle to the device context that will be
 *                              used to display the 3D line.
 *
 *      WORD x, y           :   Coordinates of the beginning of the line.
 *                              These coordinates are in device units and
 *                              represent the _outside_ most point. Horiz-
 *                              ontal lines are drawn from left to right and
 *                              vertical lines are drawn from top to bottom.
 *
 *      WORD wShadowWidth   :   Width of the shadow in device coordinates.
 *
 *      WORD  wFlags        :   The following flags may be passed to
 *                              describe the style of the 3D line:
 *
 *                              DRAW3D_IN   :   The shadow is drawn such that
 *                              the box appears to be sunk in to the screen.
 *                              This is default if 0 is passed.
 *
 *                              DRAW3D_OUT  :   The shadow is drawn such that
 *                              the box appears to be sticking out of the
 *                              screen.
 *
 *                              DRAW3D_TOPLINE, _BOTTOMLINE, _LEFTLINE, and
 *                              _RIGHTLINE  :   Specifies that a "top",
 *                              "Bottom", "Left", or"Right" line is to be
 *                              drawn.
 *
 *  RETURN (void FAR PASCAL):
 *      The line will have been drawn into the DC.
 *
 *  NOTES:
 *
 ** cjp */

void FAR PASCAL Draw3DLine(HDC hdc, WORD x, WORD y, WORD nLen,
                               WORD wShadowWidth, WORD wFlags) 
{
    HBRUSH  hOldBrush;
    HPEN    hOldPen;
    BOOL    fDark;
    POINT   Point[ 4 ];         /* define a polgon with 4 points    */

    /* if width is zero, don't do nothin'! */
    if (!wShadowWidth)
        return;

    /* define shape of polygon--origin is always the same */
    Point[0].x = x;
    Point[0].y = y;

    /*  To do this we'll simply draw a polygon with four sides, using 
     *  the appropriate brush.  I dare you to ask me why this isn't a
     *  switch/case!
     */
    if (wFlags & DRAW3D_TOPLINE)
    {
        /* across to right */
        Point[1].x = x + nLen - (wShadowWidth == 1 ? 1 : 0);
        Point[1].y = y;

        /* down/left */
        Point[2].x = x + nLen - wShadowWidth;
        Point[2].y = y + wShadowWidth;

        /* accross to left */
        Point[3].x = x + wShadowWidth;
        Point[3].y = y + wShadowWidth;

        /* select 'dark' brush if 'in'--'light' for 'out' */
        fDark = (wFlags & DRAW3D_IN) ? TRUE : FALSE;
    }

    /* possibly the bottom? */
    else if (wFlags & DRAW3D_BOTTOMLINE)
    {
        /* across to right */
        Point[1].x = x + nLen;
        Point[1].y = y;

        /* up/left */
        Point[2].x = x + nLen - wShadowWidth;
        Point[2].y = y - wShadowWidth;

        /* accross to left */
        Point[3].x = x + wShadowWidth;
        Point[3].y = y - wShadowWidth;

        /* select 'light' brush if 'in' */
        fDark = (wFlags & DRAW3D_IN) ? FALSE : TRUE;
    }

    /* ok, it's gotta be left? */
    else if (wFlags & DRAW3D_LEFTLINE)
    {
        /* down */
        Point[1].x = x;
        Point[1].y = y + nLen - (wShadowWidth == 1 ? 1 : 0);

        /* up/right */
        Point[2].x = x + wShadowWidth;
        Point[2].y = y + nLen - wShadowWidth;

        /* down */
        Point[3].x = x + wShadowWidth;
        Point[3].y = y + wShadowWidth;

        /* select 'dark' brush if 'in'--'light' for 'out' */
        fDark = (wFlags & DRAW3D_IN) ? TRUE : FALSE;
    }
    
    /* well maybe it's for the right side? */
    else if (wFlags & DRAW3D_RIGHTLINE)
    {
        /* down */
        Point[1].x = x;
        Point[1].y = y + nLen;

        /* up/left */
        Point[2].x = x - wShadowWidth;
        Point[2].y = y + nLen - wShadowWidth;

        /* up */
        Point[3].x = x - wShadowWidth;
        Point[3].y = y + wShadowWidth;

        /* select 'light' brush if 'in' */
        fDark = (wFlags & DRAW3D_IN) ? FALSE : TRUE;
    }

    /* bad drugs? */
    else return;

    /* select NULL_PEN for no borders */
    hOldPen = SelectObject(hdc, GetStockObject(NULL_PEN));

    /* select the appropriate color for the fill */
    if (fDark)
        hOldBrush = SelectObject(hdc, GetStockObject(GRAY_BRUSH));
    else
        hOldBrush = SelectObject(hdc, GetStockObject(WHITE_BRUSH));

    /* finally, draw the dern thing */
    Polygon(hdc, (LPPOINT)&Point, 4);

    /* restore what we killed */
    SelectObject(hdc, hOldBrush);
    SelectObject(hdc, hOldPen);
} /* Draw3DLine() */

/** EOF: zyz3d.c **/
