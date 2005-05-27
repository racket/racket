/*
 * Copyright (C) 1989-95 GROUPE BULL
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * GROUPE BULL BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of GROUPE BULL shall not be
 * used in advertising or otherwise to promote the sale, use or other dealings
 * in this Software without prior written authorization from GROUPE BULL.
 */

/*****************************************************************************\
*  CrIFrP.c:                                                                  *
*                                                                             *
*  XPM library                                                                *
*  Create the XImage related to the given Pixmap.                             *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

/*
 * The code related to FOR_MSW has been added by
 * HeDu (hedu@cul-ipn.uni-kiel.de) 4/94
 */

#include "xpmP.h"

#ifndef FOR_MSW

void
xpmCreateImageFromPixmap(display, pixmap, ximage_return, width, height)
    Display *display;
    Pixmap pixmap;
    XImage **ximage_return;
    unsigned int *width;
    unsigned int *height;
{
    unsigned int dum;
    int dummy;
    Window win;

    if (*width == 0 && *height == 0)
	XGetGeometry(display, pixmap, &win, &dummy, &dummy,
		     width, height, &dum, &dum);

    *ximage_return = XGetImage(display, pixmap, 0, 0, *width, *height,
			       AllPlanes, ZPixmap);
}

#endif /* FOR_MSW */
