/*
 * Copyright (C) 1989-94 GROUPE BULL
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
* scan.c:                                                                     *
*                                                                             *
*  XPM library                                                                *
*  Scanning utility for XPM file format                                       *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

/*
 * The code related to FOR_MSW has been added by
 * HeDu (hedu@cul-ipn.uni-kiel.de) 4/94
 */

#include "xpm34p.h"
#if defined(FOR_MAC) && defined(__cplusplus)
extern char *strdup(char *);
#endif
#define MAXPRINTABLE 92			/* number of printable ascii chars
					 * minus \ and " for string compat
					 * and ? to avoid ANSI trigraphs. */

static char *printable =
" .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZ\
ASDFGHJKLPIUYTREWQ!~^/()_`'][{}|";

/*
 * printable begin with a space, so in most case, due to my algorithm, when
 * the number of different colors is less than MAXPRINTABLE, it will give a
 * char follow by "nothing" (a space) in the readable xpm file
 */


typedef struct {
    Pixel *pixels;
    unsigned int *pixelindex;
    unsigned int size;
    unsigned int ncolors;
    unsigned int mask_pixel;		/* whether there is or not */
}      PixelsMap;

LFUNC(storePixel, int, (Pixel pixel, PixelsMap *pmap,
			unsigned int *index_return));

LFUNC(storeMaskPixel, int, (Pixel pixel, PixelsMap *pmap,
			    unsigned int *index_return));

LFUNC(MSWGetImagePixels, int, (Display *d, XImage *image, unsigned int width,
			       unsigned int height, PixelsMap *pmap));

LFUNC(ScanTransparentColor, int, (XpmColor *color, unsigned int cpp,
				  XpmAttributes *attributes));

LFUNC(ScanOtherColors, int, (Display *display, XpmColor *colors, int ncolors,
			     Pixel *pixels, unsigned int mask,
			     unsigned int cpp, XpmAttributes *attributes));

/*
 * This function stores the given pixel in the given arrays which are grown
 * if not large enough.
 */
static int
storePixel(Pixel pixel, PixelsMap *pmap, unsigned int *index_return)
{
    unsigned int i;
    Pixel *p;
    unsigned int ncolors;

    if (*index_return) {		/* this is a transparent pixel! */
	*index_return = 0;
	return 0;
    }
    ncolors = pmap->ncolors;
    p = pmap->pixels + pmap->mask_pixel;
    for (i = pmap->mask_pixel; i < ncolors; i++, p++)
	if (*p == pixel)
	    break;
    if (i == ncolors) {
	if (ncolors >= pmap->size) {
	    pmap->size *= 2;
	    p = (Pixel *) XpmReallocA(pmap->pixels, sizeof(Pixel) * pmap->size);
	    if (!p)
		return (1);
	    pmap->pixels = p;

	}
	(pmap->pixels)[ncolors] = pixel;
	pmap->ncolors++;
    }
    *index_return = i;
    return 0;
}

static int
storeMaskPixel(Pixel pixel, PixelsMap *pmap, unsigned int *index_return)
{
    if (!pixel) {
	if (!pmap->ncolors) {
	    pmap->ncolors = 1;
	    (pmap->pixels)[0] = 0;
	    pmap->mask_pixel = 1;
	}
	*index_return = 1;
    } else
	*index_return = 0;
    return 0;
}

/* function call in case of error, frees only locally allocated variables */
#undef RETURN
#define RETURN(status) \
{ \
    if (pmap.pixelindex) XpmFree(pmap.pixelindex); \
    if (pmap.pixels) XpmFree(pmap.pixels); \
    if (colorTable) xpmFreeColorTable(colorTable, pmap.ncolors); \
    return(status); \
}

/*
 * This function scans the given image and stores the found informations in
 * the given XpmImage structure.
 */
int
XpmCreateXpmImageFromImage(Display *display, XImage *image, XImage *shapeimage,
			   XpmImage *xpmimage, XpmAttributes *attributes)
{
    /* variables stored in the XpmAttributes structure */
    unsigned int cpp;

    /* variables to return */
    PixelsMap pmap;
    XpmColor *colorTable = NULL;
    int ErrorStatus;

    /* calculation variables */
    unsigned int width = 0;
    unsigned int height = 0;
    unsigned int cppm;			/* minimum chars per pixel */
    unsigned int c;
    unsigned int offset;

    /* initialize pmap */
    pmap.pixels = NULL;
    pmap.pixelindex = NULL;
    pmap.size = 256;			/* should be enough most of the time */
    pmap.ncolors = 0;
    pmap.mask_pixel = 0;

    /*
     * get geometry
     */
    if (image) {
	width = image->width;
	height = image->height;
    } else if (shapeimage) {
	width = shapeimage->width;
	height = shapeimage->height;
    }

    /*
     * retrieve information from the XpmAttributes
     */
    if (attributes && (attributes->valuemask & XpmCharsPerPixel
/* 3.2 backward compatibility code */
		       || attributes->valuemask & XpmInfos))
/* end 3.2 bc */
	cpp = attributes->cpp;
    else
	cpp = 0;

    pmap.pixelindex =
	(unsigned int *) XpmCallocA(width * height, sizeof(unsigned int));
    if (!pmap.pixelindex)
	RETURN(XpmNoMemory);

    pmap.pixels = (Pixel *) XpmMallocA(sizeof(Pixel) * pmap.size);
    if (!pmap.pixels)
	RETURN(XpmNoMemory);

    /*
     * scan shape mask if any
     */
    if (shapeimage) {
	ErrorStatus = MSWGetImagePixels(display, shapeimage, width, height,
					&pmap);
	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);
    }

    /*
     * scan the image data
     * 
     * In case depth is 1 or bits_per_pixel is 4, 6, 8, 24 or 32 use optimized
     * functions, otherwise use slower but sure general one.
     * 
     */

    if (image) {
	ErrorStatus = MSWGetImagePixels(display, image, width, height, &pmap);

	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);
    }

    /*
     * get rgb values and a string of char, and possibly a name for each
     * color
     */

    colorTable = (XpmColor *) XpmCalloc(pmap.ncolors, sizeof(XpmColor));
    if (!colorTable)
	RETURN(XpmNoMemory);

    /* compute the minimal cpp */
    for (cppm = 1, c = MAXPRINTABLE; pmap.ncolors > c; cppm++)
	c *= MAXPRINTABLE;
    if (cpp < cppm)
	cpp = cppm;

    if (pmap.mask_pixel) {
	ErrorStatus = ScanTransparentColor(colorTable, cpp, attributes);
	if (ErrorStatus != XpmSuccess)
	    RETURN(ErrorStatus);
	offset = 1;
    } else
	offset = 0;

    ErrorStatus = ScanOtherColors(display, colorTable + offset,
				  pmap.ncolors - offset, pmap.pixels + offset,
				  pmap.mask_pixel, cpp, attributes);
    if (ErrorStatus != XpmSuccess)
	RETURN(ErrorStatus);

    /*
     * store found informations in the XpmImage structure
     */
    xpmimage->width = width;
    xpmimage->height = height;
    xpmimage->cpp = cpp;
    xpmimage->ncolors = pmap.ncolors;
    xpmimage->colorTable = colorTable;
    xpmimage->data = pmap.pixelindex;

    XpmFree(pmap.pixels);
    return (XpmSuccess);
}

static int
ScanTransparentColor(XpmColor *color, unsigned int cpp, XpmAttributes *attributes)
{
    char *s;
    unsigned int b;

    /* first get a character string */
    if (!(s = color->string = (char *) XpmMallocA(cpp + 1)))
	return (XpmNoMemory);
    *s++ = printable[0]; 
    for (b = 1; b < cpp; b++, s++) {
      *s = printable[0];
    }
    *s = '\0';

    /* then retreive related info from the attributes if any */
    if (attributes && attributes->mask_pixel != XpmUndefPixel && (
/* 3.2 backward compatibility code */
	attributes->valuemask & XpmInfos ||
/* end 3.2 bc */
	attributes->valuemask & XpmColorTable)) {

	unsigned int key;
	char **defaults = (char **) color;
	char **mask_defaults;

/* 3.2 backward compatibility code */
	if (attributes->valuemask & XpmInfos)
	    mask_defaults = (char **)
		((XpmColor **) attributes->colorTable)[attributes->mask_pixel];
	else
/* end 3.2 bc */
	    mask_defaults = (char **) (
		attributes->colorTable + attributes->mask_pixel);
	for (key = 1; key <= NKEYS; key++) {
	    if (s = mask_defaults[key]) {
		defaults[key] = (char *) strdup(s);
		if (!defaults[key])
		    return (XpmNoMemory);
	    }
	}
    } else {
	color->c_color = (char *) strdup(TRANSPARENT_COLOR);
	if (!color->c_color)
	    return (XpmNoMemory);
    }
    return (XpmSuccess);
}

static int
ScanOtherColors(Display *display, XpmColor *colors, int ncolors, Pixel *pixels,
  unsigned int mask, unsigned int cpp, XpmAttributes *attributes)
{
    /* variables stored in the XpmAttributes structure */
    Colormap colormap;
    char *rgb_fname;

    xpmRgbName *rgbn = NULL; 
    int rgbn_max = 0;
    unsigned int i, j, c, i2;
    XpmColor *color;
    XColor *xcolors = NULL, *xcolor;
    char *s;
    XpmColor *colorTable, **oldColorTable = NULL;
    unsigned int ancolors = 0;
    Pixel *apixels;
    unsigned int mask_pixel;
    int found;

    /* retrieve information from the XpmAttributes */
    if (attributes && (attributes->valuemask & XpmColormap))
	colormap = attributes->colormap;
    else
	colormap = XDefaultColormap(display, XDefaultScreen(display));
    if (attributes && (attributes->valuemask & XpmRgbFilename))
	rgb_fname = attributes->rgb_fname;
    else
	rgb_fname = NULL;

    /* first get character strings and rgb values */
    xcolors = (XColor *) XpmMallocA(sizeof(XColor) * ncolors);
    if (!xcolors)
	return (XpmNoMemory);

    for (i = 0, i2 = (mask ? i + 1 : i), color = colors, xcolor = xcolors;
	 i < ncolors; i++, i2++, color++, xcolor++, pixels++) {

	if (!(s = color->string = (char *) XpmMallocA(cpp + 1))) {
	    XpmFree(xcolors);
	    return (XpmNoMemory);
	}
	{
	  int i3 = i2;
	  c = i3 % MAXPRINTABLE;
	  *s++ = printable[c];
	  for (j = 1; j < cpp; j++, s++) {
	    i3 = (i3 - c) / MAXPRINTABLE;
	    c = i3 % MAXPRINTABLE;
	    *s = printable[c];
	  }
	}
	*s = '\0';

	xcolor->pixel = *pixels;
    }
#if defined(wx_msw) || defined(FOR_MAC)
    XQueryColors(display, (Colormap *)colormap, xcolors, ncolors);
#else
    XQueryColors(display, (Colormap)colormap, xcolors, ncolors);
#endif

    /* FOR_MSW: rgb names and values are hardcoded in rgbtab.h */
    rgbn_max = xpmReadRgbNames(NULL, NULL);

    if (attributes && attributes->valuemask & XpmColorTable) {
	colorTable = attributes->colorTable;
	ancolors = attributes->ncolors;
	apixels = attributes->pixels;
	mask_pixel = attributes->mask_pixel;
    }
/* 3.2 backward compatibility code */
    else if (attributes && attributes->valuemask & XpmInfos) {
	oldColorTable = (XpmColor **) attributes->colorTable;
	ancolors = attributes->ncolors;
	apixels = attributes->pixels;
	mask_pixel = attributes->mask_pixel;
    }
/* end 3.2 bc */

    for (i = 0, color = colors, xcolor = xcolors; i < ncolors;
						  i++, color++, xcolor++) {

	/* look for related info from the attributes if any */
	found = False;
	if (ancolors) {
	    unsigned int offset = 0;

	    for (j = 0; j < ancolors; j++) {
		if (j == mask_pixel) {
		    offset = 1;
		    continue;
		}
		if (apixels[j - offset] == xcolor->pixel)
		    break;
	    }
	    if (j != ancolors) {
		unsigned int key;
		char **defaults = (char **) color;
		char **adefaults;

/* 3.2 backward compatibility code */
		if (oldColorTable)
		    adefaults = (char **) oldColorTable[j];
		else
/* end 3.2 bc */
		    adefaults = (char **) (colorTable + j);

		found = True;
		for (key = 1; key <= NKEYS; key++) {
		    if (s = adefaults[key])
			defaults[key] = (char *) strdup(s);
		}
	    }
	}
	if (!found) {
	    /* if nothing found look for a color name */
	    {
		/* at last store the rgb value */
		char buf[BUFSIZ];
		sprintf(buf, "#%02x%02x%02x",
			xcolor->red, xcolor->green, xcolor->blue);
		color->c_color = (char *) strdup(buf);
	    }
	    if (!color->c_color) {
		XpmFree(xcolors);
		xpmFreeRgbNames(rgbn, rgbn_max);
		return (XpmNoMemory);
	    }
	}
    }

    XpmFree(xcolors);
    xpmFreeRgbNames(rgbn, rgbn_max);
    return (XpmSuccess);
}

static int
MSWGetImagePixels(Display *display, XImage *image, unsigned int width, unsigned int height, PixelsMap *pmap)
{
    unsigned int *iptr;
    unsigned int x, y;
    Pixel pixel;

    iptr = pmap->pixelindex;

    for (y = 0; y < height; y++) {
	for (x = 0; x < width; x++, iptr++) {
	    /* bitmap must be selected !!! */
		RGBColor	macpixel;
		long r, g, b;
		GetCPixel(x, y, &macpixel); 
                r = ((macpixel.red >> 8) & 0xFF);
		g = ((macpixel.green >> 8) & 0xFF);
		b = ((macpixel.blue >> 8) & 0xFF);
		pixel = (r << 16) | (g << 8) | b;
	    if (storePixel(pixel, pmap, iptr))
		return (XpmNoMemory);
	}
    }
    return (XpmSuccess);
}
