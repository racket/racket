/* $Id: plctrl.c,v 1.3 2005/03/18 20:32:41 eli Exp $

	Misc. control routines, like begin, end, exit, change graphics/text
	mode, change color.  Includes some spillage from plcore.c.  If you
	don't know where it should go, put it here.  
*/

#define DEBUG

#define NEED_PLDEBUG
#include "plplotP.h"
#ifdef macintosh
#include "mac.h"
/* for plMacLibOpen prototype; used in plLibOpen */
#endif

#ifdef DJGPP			/* dos386/djgpp */
#ifdef __unix
#undef __unix
#endif
#endif

#ifdef __unix
#include <sys/types.h>
#include <sys/stat.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <errno.h>
#endif

/* Static functions */

/* Used by any external init code to suggest a path */
MZ_DLLEXPORT
char* plplotLibDir = 0;

static void
color_set(PLINT i, U_CHAR r, U_CHAR g, U_CHAR b, char *name );

static void
strcat_delim(char *dirspec);

static int
(*exit_handler) (char *errormsg);

static void
plcmap0_def(int imin, int imax);

static void
plcmap1_def(void);

static PLFLT
value(double n1, double n2, double hue);

/* An additional hardwired location for lib files. */
/* I have no plans to change these again, ever. */

#if defined(DJGPP)
#ifndef PLLIBDEV
#define PLLIBDEV "c:/plplot/lib"
#endif

#elif defined(MSDOS)
#ifndef PLLIBDEV
#define PLLIBDEV "c:\\plplot\\lib"
#endif

#else

/* Anything else is assumed to be Unix */

#ifndef PLLIBDEV
#define PLLIBDEV "/usr/local/plplot/lib"
#endif

#endif

/*--------------------------------------------------------------------------*\
 *  Routines that deal with colors & color maps.
\*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*\
 * plcol0()
 *
 * Set color, map 0.  Argument is integer between 0 and plsc->ncol0.
\*--------------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plcol0(PLINT icol0)
{
    if (plsc->level < 1) {
	plabort("plcol0: Please call plinit first");
	return;
    }
    if (icol0 < 0 || icol0 >= plsc->ncol0) {
	char buffer[256];
	sprintf(buffer, "plcol0: Invalid color map entry: %d", (int) icol0);
	plabort(buffer);
	return;
    }

    plsc->icol0 = icol0;
    plsc->curcolor.r = plsc->cmap0[icol0].r;
    plsc->curcolor.g = plsc->cmap0[icol0].g;
    plsc->curcolor.b = plsc->cmap0[icol0].b;

    plsc->curcmap = 0;
    plP_state(PLSTATE_COLOR0);
}

/*--------------------------------------------------------------------------*\
 * plcol1()
 *
 * Set color, map 1.  Argument is a float between 0. and 1.
\*--------------------------------------------------------------------------*/

void
c_plcol1(PLFLT col1)
{
    PLINT icol1;

    if (plsc->level < 1) {
	plabort("plcol1: Please call plinit first");
	return;
    }
    if (col1 < 0 || col1 > 1) {
	char buffer[256];
	sprintf(buffer, "plcol1: Invalid color map position: %f", (PLFLT) col1);
	plabort(buffer);
	return;
    }

    icol1 = col1 * plsc->ncol1;
    icol1 = MIN(icol1, plsc->ncol1-1);

    plsc->icol1 = icol1;
    plsc->curcolor.r = plsc->cmap1[plsc->icol1].r;
    plsc->curcolor.g = plsc->cmap1[plsc->icol1].g;
    plsc->curcolor.b = plsc->cmap1[plsc->icol1].b;

    plsc->curcmap = 1;
    plP_state(PLSTATE_COLOR1);
}

/*--------------------------------------------------------------------------*\
 * plscolbg()
 *
 * Set the background color (cmap0[0]) by 8 bit RGB value
\*--------------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plscolbg(PLINT r, PLINT g, PLINT b)
{
    plscol0(0, r, g, b);
}

/*--------------------------------------------------------------------------*\
 * plgcolbg()
 *
 * Returns the background color (cmap0[0]) by 8 bit RGB value
\*--------------------------------------------------------------------------*/

void
c_plgcolbg(PLINT *r, PLINT *g, PLINT *b)
{
    plgcol0(0, r, g, b);
}

/*--------------------------------------------------------------------------*\
 * plscol0()
 *
 * Set a given color from color map 0 by 8 bit RGB value
 * Does not result in any additional cells to be allocated.
\*--------------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plscol0(PLINT icol0, PLINT r, PLINT g, PLINT b)
{
    if (plsc->cmap0 == NULL)
	plscmap0n(0);

    if (icol0 < 0 || icol0 >= plsc->ncol0) {
	char buffer[256];
	sprintf(buffer, "plscol0: Illegal color table value: %d", (int) icol0);
	plabort(buffer);
	return;
    }
    if ((r < 0 || r > 255) || (g < 0 || g > 255) || (b < 0 || b > 255)) {
	char buffer[256];
	sprintf(buffer, "plscol0: Invalid RGB color: %d, %d, %d",
		(int) r, (int) g, (int) b);
	plabort(buffer);
	return;
    }

    plsc->cmap0[icol0].r = r;
    plsc->cmap0[icol0].g = g;
    plsc->cmap0[icol0].b = b;

    if (plsc->level > 0)
	plP_state(PLSTATE_CMAP0);
}

/*--------------------------------------------------------------------------*\
 * plgcol0()
 *
 * Returns 8 bit RGB values for given color from color map 0
 * Values are negative if an invalid color id is given
\*--------------------------------------------------------------------------*/

void
c_plgcol0(PLINT icol0, PLINT *r, PLINT *g, PLINT *b)
{
    if (plsc->cmap0 == NULL)
	plscmap0n(0);

    *r = -1;
    *g = -1;
    *b = -1;

    if (icol0 < 0 || icol0 > plsc->ncol0) {
	char buffer[256];
	sprintf(buffer, "plgcol0: Invalid color index: %d", (int) icol0);
	plabort(buffer);
	return;
    }

    *r = plsc->cmap0[icol0].r;
    *g = plsc->cmap0[icol0].g;
    *b = plsc->cmap0[icol0].b;

    return;
}

/*--------------------------------------------------------------------------*\
 * plscmap0()
 *
 * Set color map 0 colors by 8 bit RGB values.  This sets the entire color
 * map -- only as many colors as specified will be allocated.
\*--------------------------------------------------------------------------*/

void
c_plscmap0(PLINT *r, PLINT *g, PLINT *b, PLINT ncol0)
{
    int i;

    plscmap0n(ncol0);

    for (i = 0; i < plsc->ncol0; i++) {
	if ((r[i] < 0 || r[i] > 255) ||
	    (g[i] < 0 || g[i] > 255) ||
	    (b[i] < 0 || b[i] > 255)) {

	    char buffer[256];
	    sprintf(buffer, "plscmap0: Invalid RGB color: %d, %d, %d",
		    (int) r[i], (int) g[i], (int) b[i]);
	    plabort(buffer);
	    return;
	}

	plsc->cmap0[i].r = r[i];
	plsc->cmap0[i].g = g[i];
	plsc->cmap0[i].b = b[i];
    }

    if (plsc->level > 0)
	plP_state(PLSTATE_CMAP0);
}

/*--------------------------------------------------------------------------*\
 * plscmap1()
 *
 * Set color map 1 colors by 8 bit RGB values
 * This also sets the number of colors.
\*--------------------------------------------------------------------------*/

void
c_plscmap1(PLINT *r, PLINT *g, PLINT *b, PLINT ncol1)
{
    int i;

    plscmap1n(ncol1);

    for (i = 0; i < plsc->ncol1; i++) {
	if ((r[i] < 0 || r[i] > 255) ||
	    (g[i] < 0 || g[i] > 255) ||
	    (b[i] < 0 || b[i] > 255)) {

	    char buffer[256];
	    sprintf(buffer, "plscmap1: Invalid RGB color: %d, %d, %d",
		    (int) r[i], (int) g[i], (int) b[i]);
	    plabort(buffer);
	    return;
	}
	plsc->cmap1[i].r = r[i];
	plsc->cmap1[i].g = g[i];
	plsc->cmap1[i].b = b[i];
    }

    if (plsc->level > 0)
	plP_state(PLSTATE_CMAP1);
}

/*--------------------------------------------------------------------------*\
 * plscmap1l()
 *
 * Set color map 1 colors using a piece-wise linear relationship between
 * position in the color map (from 0 to 1) and position in HLS or RGB color
 * space.  May be called at any time.
 *
 * The idea here is to specify a number of control points that specify the
 * mapping between HLS (or RGB or CMY) and palette 1 value.  Between these
 * points, linear interpolation is used.  By mapping position in the color
 * map to function value, this gives a smooth variation of color with
 * intensity.  Any number of control points may be specified, located at
 * arbitrary positions (intensities), although typically 2 - 4 are enough.
 * Another way of stating this is that we are traversing a given number of
 * lines through HLS (or RGB) space as we move through cmap 1 entries.  The
 * control points at the minimum and maximum intensity (0 and 1) must
 * always be specified.  By adding more control points you can get more
 * variation.  One good technique for plotting functions that vary about
 * some expected average is to use an additional 2 control points in the
 * center (intensity ~= 0.5) that are the same color as the background
 * (typically white for paper output, black for crt), and same hue as the
 * boundary control points.  This allows the highs and lows to be very
 * easily distinguished.
 *
 * Each control point must specify the position in cmap 1 as well as three
 * coordinates in HLS or RGB space.  The first point MUST correspond to
 * position = 0, and the last to position = 1.  
 *
 * The hue is interpolated around the "front" of the color wheel
 * (red<->green<->blue<->red) unless the "rev" flag is set, in which case
 * interpolation proceeds around the back (reverse) side.  Specifying
 * rev=NULL is equivalent to setting rev[]=0 for every control point.
 *
 * Bounds on RGB coordinates:
 *	R,G,B		[0, 1]		magnitude
 *
 * Bounds on HLS coordinates:
 *	hue		[0, 360]	degrees
 *	lightness	[0, 1]		magnitude
 *	saturation	[0, 1]		magnitude
 *
 * The inputs are:
 *	itype		0: HLS, 1: RGB
 *	npts		number of control points
 *	pos[]		position for each control point
 *	coord1[]	first coordinate for each control point
 *	coord2[]	second coordinate for each control point
 *	coord3[]	third coordinate for each control point 
 *	rev[]		reverse flag for each control point
\*--------------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plscmap1l(PLINT itype, PLINT npts, PLFLT *pos,
	    PLFLT *coord1, PLFLT *coord2, PLFLT *coord3, PLINT *rev)
{
    int n;
    PLFLT h, l, s, r, g, b;

    if (npts < 2) {
	plabort("plscmap1l: Must specify at least two control points");
	return;
    }

    if ( (pos[0] != 0) || (pos[npts-1] != 1)) {
	plabort("plscmap1l: First, last control points must lie on boundary");
	return;
    }

    if ( npts > PL_MAX_CMAP1CP ) {
	plabort("plscmap1l: exceeded maximum number of control points");
	return;
    }

/* Allocate if not done yet */

    if (plsc->cmap1 == NULL)
	plscmap1n(0);

/* Save control points */

    plsc->ncp1 = npts;

    for (n = 0; n < npts; n++) {

	if (itype == 0) {
	    h = coord1[n];
	    l = coord2[n];
	    s = coord3[n];
	}
	else {
	    r = coord1[n];
	    g = coord2[n];
	    b = coord3[n];
	    plRGB_HLS(r, g, b, &h, &l, &s);
	}

	plsc->cmap1cp[n].h = h;
	plsc->cmap1cp[n].l = l;
	plsc->cmap1cp[n].s = s;
	plsc->cmap1cp[n].p = pos[n];

	if (rev == NULL)
	    plsc->cmap1cp[n].rev = 0;
	else
	    plsc->cmap1cp[n].rev = rev[n];
    }

/* Calculate and set color map */

    plcmap1_calc();
}

/*--------------------------------------------------------------------------*\
 * plcmap1_calc()
 *
 * Bin up cmap 1 space and assign colors to make inverse mapping easy.
 * Always do interpolation in HLS space.
\*--------------------------------------------------------------------------*/

void
plcmap1_calc(void)
{
    int i, n;
    PLFLT delta, dp, dh, dl, ds;
    PLFLT h, l, s, p, r, g, b;

/* Loop over all control point pairs */

    for (n = 0; n < plsc->ncp1-1; n++) {

	if ( plsc->cmap1cp[n].p == plsc->cmap1cp[n+1].p )
	    continue;

    /* Differences in p, h, l, s between ctrl pts */

	dp = plsc->cmap1cp[n+1].p - plsc->cmap1cp[n].p;
	dh = plsc->cmap1cp[n+1].h - plsc->cmap1cp[n].h;
	dl = plsc->cmap1cp[n+1].l - plsc->cmap1cp[n].l;
	ds = plsc->cmap1cp[n+1].s - plsc->cmap1cp[n].s;

    /* Adjust dh if we are to go around "the back side" */

	if (plsc->cmap1cp[n].rev)
	    dh = (dh > 0) ? dh-360 : dh+360;

    /* Loop over all color cells.  Only interested in cells located (in */
    /* cmap1 space)  between n_th and n+1_th control points */

	for (i = 0; i < plsc->ncol1; i++) {
	    p = (double) i / (plsc->ncol1 - 1.0);
	    if ( (p < plsc->cmap1cp[n].p) ||
		 (p > plsc->cmap1cp[n+1].p) )
		continue;

	/* Interpolate based on position of color cell in cmap1 space */

	    delta = (p - plsc->cmap1cp[n].p) / dp;

	/* Linearly interpolate to get color cell h, l, s values */

	    h = plsc->cmap1cp[n].h + dh * delta;
	    l = plsc->cmap1cp[n].l + dl * delta;
	    s = plsc->cmap1cp[n].s + ds * delta;

	    while (h >= 360.)
		h -= 360.;

	    while (h < 0.)
		h += 360.;

	    plHLS_RGB(h, l, s, &r, &g, &b);

	    plsc->cmap1[i].r = MAX(0, MIN(255, (int) (256. * r)));
	    plsc->cmap1[i].g = MAX(0, MIN(255, (int) (256. * g)));
	    plsc->cmap1[i].b = MAX(0, MIN(255, (int) (256. * b)));
	}
    }

    if (plsc->level > 0)
	plP_state(PLSTATE_CMAP1);
}

/*--------------------------------------------------------------------------*\
 * plscmap0n()
 *
 * Set number of colors in cmap 0, (re-)allocate cmap 0, and fill with
 * default values for those colors not previously allocated (and less
 * than index 15, after that you just get grey).
 *
 * The driver is not guaranteed to support all of these.
\*--------------------------------------------------------------------------*/

void
c_plscmap0n(PLINT ncol0)
{
    int ncol, size, imin, imax;

/* No change */

    if (ncol0 > 0 && plsc->ncol0 == ncol0)
	return;

/* Handle all possible startup conditions */

    if (plsc->ncol0 <= 0 && ncol0 <= 0)
	ncol = 16;
    else if (ncol0 <= 0)
	ncol = plsc->ncol0;
    else
        ncol = ncol0;

    imax = ncol-1;
    size = ncol * sizeof(PLColor);

/* Allocate the space */

    if (plsc->cmap0 == NULL) {
	plsc->cmap0 = (PLColor *) calloc(1, size);
	imin = 0;
    }
    else {
	plsc->cmap0 = (PLColor *) realloc(plsc->cmap0, size);
	imin = plsc->ncol0;
    }

/* Fill in default entries */

    plsc->ncol0 = ncol;
    plcmap0_def(imin, imax);
}

/*--------------------------------------------------------------------------*\
 * plscmap1n()
 *
 * Set number of colors in cmap 1, (re-)allocate cmap 1, and set default
 * values if this is the first allocation.
 *
 * Note that the driver is allowed to disregard this number.
 * In particular, most use fewer than we use internally.
\*--------------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plscmap1n(PLINT ncol1)
{
    int ncol, size;

/* No change */

    if (ncol1 > 0 && plsc->ncol1 == ncol1)
	return;

/* Handle all possible startup conditions */

    if (plsc->ncol1 <= 0 && ncol1 <= 0)
	ncol = 128;
    else if (ncol1 <= 0)
	ncol = plsc->ncol1;
    else
        ncol = ncol1;

    size = ncol * sizeof(PLColor);

/* Allocate the space */

    if (plsc->ncol1 > 0) 
	plsc->cmap1 = (PLColor *) realloc(plsc->cmap1, size);
    else 
	plsc->cmap1 = (PLColor *) calloc(ncol, sizeof(PLColor));

/* Fill in default entries */

    plsc->ncol1 = ncol;
    if (plsc->ncp1 == 0)
	plcmap1_def();
    else
	plcmap1_calc();
}

/*--------------------------------------------------------------------------*\
 * color_set()
 *
 * Initializes color table entry by RGB values.
\*--------------------------------------------------------------------------*/

static void
color_set(PLINT i, U_CHAR r, U_CHAR g, U_CHAR b, char *name )
{
    plsc->cmap0[i].r = r;
    plsc->cmap0[i].g = g;
    plsc->cmap0[i].b = b;
    plsc->cmap0[i].name = name;
}

/*--------------------------------------------------------------------------*\
 * plcmap0_def()
 *
 * Initializes specified color map 0 color entry to its default.
 *
 * Initial RGB values for color map 0 taken from X11R6 
 * (XFree86-3.3.6) X-windows 
 * rgb.txt file, and may not accurately represent the described colors on 
 * all systems.
\*--------------------------------------------------------------------------*/

#define color_def(i, r, g, b, n) \
if (i >= imin && i <= imax) color_set(i, r, g, b, n);

static void
plcmap0_def(int imin, int imax)
{
    int i;

    color_def(0,    0,   0,   0, "black" );	/* black */
    color_def(1,  255,   0,   0, "red");	/* red */
    color_def(2,  255, 255,   0, "yellow" );	/* yellow */
    color_def(3,    0, 255,   0, "green" );	/* green */
    color_def(4,  127, 255, 212, "aquamarine" );	/* aquamarine */
    color_def(5,  255, 192, 203, "pink" );	/* pink */
    color_def(6,  245, 222, 179, "wheat" );	/* wheat */
    color_def(7,  190, 190, 190, "grey" );	/* grey */
    color_def(8,  165,  42,  42, "brown" );	/* brown */
    color_def(9,    0,   0, 255, "blue" );	/* blue */
    color_def(10, 138,  43, 226, "BlueViolet" );	/* Blue Violet */
    color_def(11,   0, 255, 255, "cyan" );	/* cyan */
    color_def(12,  64, 224, 208, "turquoise" );	/* turquoise */
    color_def(13, 255,   0, 255, "magenta" );	/* magenta */
    color_def(14, 250, 128, 114, "salmon" );	/* salmon */
    color_def(15, 255, 255, 255, "white" );	/* white */

/* Any others are just arbitrarily set */

    for (i = 16; i <= imax; i++)
	color_def(i, 255, 0, 0, "red"); 	/* red */
}

/*--------------------------------------------------------------------------*\
 * plcmap1_def()
 *
 * Initializes color map 1.
 *
 * The default initialization uses 6 control points in HLS space, the inner
 * ones being very close to one of the vertices of the HLS double cone.  The
 * vertex used (black or white) is chosen to be the closer to the background
 * color.  The 6 points were chosen over the older 4 points in order to make 
 * weaker structures more easily visible, and give more control through the
 * palette editor.  If you don't like these settings.. change them!
\*--------------------------------------------------------------------------*/

static void
plcmap1_def(void)
{
    PLFLT i[6], h[6], l[6], s[6], midpt = 0., vertex = 0.;

/* Positions of control points */

    i[0] = 0;		/* left boundary */
    i[1] = 0.44;	/* a little left of center */
    i[2] = 0.50;	/* at center */
    i[3] = 0.50;	/* at center */
    i[4] = 0.56;	/* a little right of center */
    i[5] = 1;		/* right boundary */

/* For center control points, pick black or white, whichever is closer to bg */
/* Be carefult to pick just short of top or bottom else hue info is lost */

    if (plsc->cmap0 != NULL)
	vertex = ((PLFLT) plsc->cmap0[0].r +
		  (PLFLT) plsc->cmap0[0].g +
		  (PLFLT) plsc->cmap0[0].b) / 3. / 255.;

    if (vertex < 0.5) {
	vertex = 0.01;
	midpt  = 0.10;
    } else {
	vertex = 0.99;
	midpt  = 0.90;
    }

/* Set hue */

    h[0] = 260;		/* low: blue-violet */
    h[1] = 260;		/* only change as we go over vertex */
    h[2] = 260;		/* only change as we go over vertex */
    h[3] = 0;		/* high: red */
    h[4] = 0;		/* high: red */
    h[5] = 0;		/* keep fixed */

/* Set lightness */

    l[0] = 0.5;		/* low */
    l[1] = midpt;	/* midpoint value */
    l[2] = vertex;	/* bg */
    l[3] = vertex;	/* bg */
    l[4] = midpt;	/* midpoint value */
    l[5] = 0.5;		/* high */

/* Set saturation -- keep at maximum */

    s[0] = 1;
    s[1] = 1;
    s[2] = 1;
    s[3] = 1;
    s[4] = 1;
    s[5] = 1;

    c_plscmap1l(0, 6, i, h, l, s, NULL);
}

/*--------------------------------------------------------------------------*\
 * plscolor()
 *
 * Used to globally turn color output on/off
\*--------------------------------------------------------------------------*/

void
c_plscolor(PLINT color)
{
    plsc->colorset = 1;
    plsc->color = color;
}

/*--------------------------------------------------------------------------*\
 * plrgb()
 *
 * Set line color by red, green, blue from  0. to 1.
 * Do NOT use this.  Only retained for backward compatibility
\*--------------------------------------------------------------------------*/

void
c_plrgb(PLFLT r, PLFLT g, PLFLT b)
{
    if (plsc->level < 1) {
	plabort("plrgb: Please call plinit first");
	return;
    }

    plsc->icol0 = PL_RGB_COLOR;
    plsc->curcolor.r = MAX(0, MIN(255, (int) (256. * r)));
    plsc->curcolor.g = MAX(0, MIN(255, (int) (256. * g)));
    plsc->curcolor.b = MAX(0, MIN(255, (int) (256. * b)));

    plsc->curcmap = 0;
    plP_state(PLSTATE_COLOR0);
}

/*--------------------------------------------------------------------------*\
 * plrgb1()
 *
 * Set line color by 8 bit RGB values.
 * Do NOT use this.  Only retained for backward compatibility
\*--------------------------------------------------------------------------*/

void
c_plrgb1(PLINT r, PLINT g, PLINT b)
{
    if (plsc->level < 1) {
	plabort("plrgb1: Please call plinit first");
	return;
    }
    if ((r < 0 || r > 255) || (g < 0 || g > 255) || (b < 0 || b > 255)) {
	plabort("plrgb1: Invalid color");
	return;
    }

    plsc->icol0 = PL_RGB_COLOR;
    plsc->curcolor.r = r;
    plsc->curcolor.g = g;
    plsc->curcolor.b = b;

    plsc->curcmap = 0;
    plP_state(PLSTATE_COLOR0);
}

/*--------------------------------------------------------------------------*\
 * void plhls()
 *
 * Set current color by hue, lightness, and saturation.
 * Convert hls color coordinates to rgb, then call plrgb.
 * Do NOT use this.  Only retained for backward compatibility
\*--------------------------------------------------------------------------*/

void
c_plhls(PLFLT h, PLFLT l, PLFLT s)
{
    PLFLT r, g, b;

    plHLS_RGB(h, l, s, &r, &g, &b);
    plrgb(r, g, b);
}

/*--------------------------------------------------------------------------*\
 * void value()
 *
 * Auxiliary function used by plHLS_RGB().
\*--------------------------------------------------------------------------*/

static PLFLT
value(double n1, double n2, double hue)
{
    PLFLT val;

    while (hue >= 360.)
	hue -= 360.;
    while (hue < 0.)
	hue += 360.;

    if (hue < 60.)
	val = n1 + (n2 - n1) * hue / 60.;
    else if (hue < 180.)
	val = n2;
    else if (hue < 240.)
	val = n1 + (n2 - n1) * (240. - hue) / 60.;
    else
	val = n1;

    return (val);
}

/*--------------------------------------------------------------------------*\
 * void plHLS_RGB()
 *
 * Convert HLS color to RGB color.
 * Bounds on HLS (input):
 *	hue		[0., 360.]	degrees
 *	lightness	[0., 1.]	magnitude
 *	saturation	[0., 1.]	magnitude
 *
 * Hue is always mapped onto the interval [0., 360.] regardless of input.
 * Bounds on RGB (output) is always [0., 1.].  Convert to RGB color values
 * by multiplying by 2**nbits (nbits typically 8).
\*--------------------------------------------------------------------------*/

void
plHLS_RGB(PLFLT h, PLFLT l, PLFLT s, PLFLT *p_r, PLFLT *p_g, PLFLT *p_b)
{
    PLFLT m1, m2;

    if (l <= .5)
	m2 = l * (s + 1.);
    else
	m2 = l + s - l * s;

    m1 = 2 * l - m2;

    *p_r = value(m1, m2, h + 120.);
    *p_g = value(m1, m2, h);
    *p_b = value(m1, m2, h - 120.);
}

/*--------------------------------------------------------------------------*\
 * void plRGB_HLS()
 *
 * Convert RGB color to HLS color.
 * Bounds on RGB (input) is always [0., 1.].  
 * Bounds on HLS (output):
 *	hue		[0., 360.]	degrees
 *	lightness	[0., 1.]	magnitude
 *	saturation	[0., 1.]	magnitude
\*--------------------------------------------------------------------------*/

void
plRGB_HLS(PLFLT r, PLFLT g, PLFLT b, PLFLT *p_h, PLFLT *p_l, PLFLT *p_s)
{
    PLFLT h, l, s, d, rc, gc, bc, rgb_min, rgb_max;

    rgb_min = MIN( r, MIN( g, b ));
    rgb_max = MAX( r, MAX( g, b ));

    l = (rgb_min+rgb_max) / 2.0;

    if (rgb_min == rgb_max) {
	s = 0;
	h = 0;
    } 
    else {
	d = rgb_max - rgb_min;
	if (l < 0.5)
	    s = 0.5 * d / l;
	else 
	    s = 0.5* d / (1.-l);

	rc = (rgb_max-r) / d;
	gc = (rgb_max-g) / d;
	bc = (rgb_max-b) / d;

	if (r == rgb_max)
	    h = bc-gc;
	else if (g == rgb_max)
	    h = rc-bc+2;
	else
	    h = gc-rc-2;

	h = h*60;
	if (h <  0)
	    h = h+360;
	else if (h >= 360)
	    h = h-360;
    }
    *p_h = h;
    *p_l = l;
    *p_s = s;
}

/*--------------------------------------------------------------------------*\
 * A grab-bag of various control routines.
\*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*\
 * void plwarn()
 *
 * A handy way to issue warnings, if need be.
\*--------------------------------------------------------------------------*/

void
plwarn(char *errormsg)
{
    int was_gfx = 0;

    if (plsc->graphx == 1) {
	was_gfx = 1;
	pltext();
    }

    fprintf(stderr, "\n*** PLPLOT WARNING ***\n");
    if (*errormsg != '\0')
	fprintf(stderr, "%s\n", errormsg);

    if (was_gfx == 1)
	plgra();
}

/*--------------------------------------------------------------------------*\
 * void plabort()
 *
 * Much the same as plwarn(), but appends ", aborting operation" to the
 * error message.  Helps to keep source code uncluttered and provides a
 * convention for error aborts.
\*--------------------------------------------------------------------------*/

void
plabort(char *errormsg)
{
    if (plsc->errcode != NULL)
	*(plsc->errcode) = 1;

    if (plsc->errmsg != NULL) {
	sprintf(plsc->errmsg, "\n*** PLPLOT ERROR ***\n");
	if (*errormsg != '\0')
	    sprintf(plsc->errmsg, "%s, aborting operation\n", errormsg);

    } else {
	int was_gfx = 0;

	if (plsc->graphx == 1) {
	    was_gfx = 1;
	    pltext();
	}

	fprintf(stderr, "\n*** PLPLOT ERROR ***\n");
	if (*errormsg != '\0')
	    fprintf(stderr, "%s, aborting operation\n", errormsg);

	if (was_gfx == 1)
	    plgra();
    }
}

/*--------------------------------------------------------------------------*\
 * void plexit()
 *
 * In case of an abort this routine is called.  It just prints out an error
 * message and tries to clean up as much as possible.  It's best to turn
 * off pause and then restore previous setting before returning.
 *
 * If cleanup needs to be done in the main program, the user should write
 * his/her own exit handler and pass it in via plsexit().  This function
 * should should either call plend() before exiting, or simply return.
\*--------------------------------------------------------------------------*/

void
plexit(char *errormsg)
{
    int status = 1;

    if (exit_handler != NULL)
	status = (*exit_handler)(errormsg);

    plsc->nopause = 1;
    if (*errormsg != '\0') {
	fprintf(stderr, "\n*** PLPLOT ERROR ***\n");
	fprintf(stderr, "%s\n", errormsg);
    }
    plend();

    fprintf(stderr, "Program aborted\n");
    exit(status);
}

/*--------------------------------------------------------------------------*\
 * void plsexit()
 *
 * Sets an optional user exit handler.
\*--------------------------------------------------------------------------*/

void
plsexit(int (*handler) (char *))
{
    exit_handler = handler;
}

/*--------------------------------------------------------------------------*\
 * void plgra()
 *
 * Switches to graphics screen.  
 *
 * Here and in pltext() it's a good idea to return silently if plinit()
 * hasn't yet been called, since plwarn() calls pltext() and plgra(), and
 * plwarn() may be called at any time.
\*--------------------------------------------------------------------------*/

void
c_plgra(void)
{
    if (plsc->level > 0)
	plP_esc(PLESC_GRAPH, NULL);
}

void
c_plxormod(PLINT mode, PLINT *status)	/* xor mode */
{
  static int ostate = 0;

  if (!plsc->dev_xor) {
    *status = 0;
    return;
  }

  if (plsc->level > 0) {
    plP_esc(PLESC_XORMOD, &mode);
    if (mode) {
      ostate = plsc->plbuf_write;
      plsc->plbuf_write = 0;
    } else
      plsc->plbuf_write = ostate;
  } 
  *status = 1;
}

/*--------------------------------------------------------------------------*\
 * void pltext()
 *
 * Switches to text screen.
\*--------------------------------------------------------------------------*/

void
c_pltext(void)
{
    if (plsc->level > 0)
	plP_esc(PLESC_TEXT, NULL);
}

/*--------------------------------------------------------------------------*\
 * void pl_cmd()
 *
 * Front-end to driver escape function.
 * In principle this can be used to pass just about anything directly
 * to the driver.
\*--------------------------------------------------------------------------*/

void
pl_cmd(PLINT op, void *ptr)
{
    plP_esc(op, ptr);
}

/*--------------------------------------------------------------------------*\
 * char *plFindCommand
 *
 * Looks for the specified executable file.  Search path:
 *	PLPLOT_BIN_ENV = $(PLPLOT_BIN)
 *	current directory
 *	PLPLOT_HOME_ENV/bin = $(PLPLOT_HOME)/bin
 *	BIN_DIR
 *
 * The caller must free the returned pointer (points to malloc'ed memory)
 * when finished with it.
\*--------------------------------------------------------------------------*/

char *
plFindCommand(char *fn)
{
    char *fs = NULL, *dn;

/* PLPLOT_BIN_ENV = $(PLPLOT_BIN) */

#if defined(PLPLOT_BIN_ENV)
    if ((dn = getenv(PLPLOT_BIN_ENV)) != NULL) {
        plGetName(dn, "", fn, &fs);
        if ( ! plFindName(fs))
            return fs;
        fprintf(stderr, PLPLOT_BIN_ENV"=\"%s\"\n", dn); /* what IS set? */
    }
#endif  /* PLPLOT_BIN_ENV */

/* Current directory */

    plGetName(".", "", fn, &fs);
    if ( ! plFindName(fs))
	return fs;

/* PLPLOT_HOME_ENV/bin = $(PLPLOT_HOME)/bin */

#if defined(PLPLOT_HOME_ENV)
    if ((dn = getenv(PLPLOT_HOME_ENV)) != NULL) {
        plGetName(dn, "bin", fn, &fs);
        if ( ! plFindName(fs))
            return fs;
        fprintf(stderr, PLPLOT_HOME_ENV"=\"%s\"\n",dn); /* what IS set? */
    }
#endif  /* PLPLOT_HOME_ENV */

/* BIN_DIR */

#if defined (BIN_DIR)
    plGetName(BIN_DIR, "", fn, &fs);
    if ( ! plFindName(fs))
	return fs;
#endif

/* Crapped out */

    free_mem(fs);
    fprintf(stderr, "plFindCommand: cannot locate command: %s\n", fn);
#if defined (BIN_DIR)
    fprintf(stderr, "bin dir=\"" BIN_DIR "\"\n" );      /* what WAS set? */
#endif  /* BIN_DIR */
    return NULL;
}

/*--------------------------------------------------------------------------*\
 * FILE *plLibOpen(fn)
 *
 * Return file pointer to lib file.
 * Locations checked:
 *	PLPLOT_LIB_ENV = $(PLPLOT_LIB)
 *	current directory
 *	PLPLOT_HOME_ENV/lib = $(PLPLOT_HOME)/lib
 *	DATA_DIR
 *	PLLIBDEV
\*--------------------------------------------------------------------------*/

FILE *
plLibOpen(char *fn)
{
    FILE *ret = NULL;
    
    PDFstrm *pdfs = plLibOpenPdfstrm(fn);
    if (pdfs == NULL) {
        return NULL;
    }
    if (pdfs->file != NULL) {
        ret = pdfs->file;
	pdfs->file = NULL;
    }
    pdf_close(pdfs);
    return ret;
}

PDFstrm *
plLibOpenPdfstrm(char *fn)
{
    PDFstrm *file;
    char *fs = NULL, *dn = NULL;

/****	search PLPLOT_LIB_ENV = $(PLPLOT_LIB)	****/

#if defined(PLPLOT_LIB_ENV)
    if ((dn = getenv(PLPLOT_LIB_ENV)) != NULL) {
        plGetName(dn, "", fn, &fs);

        if ((file = pdf_fopen(fs, "rb")) != NULL)
            goto done;

        fprintf(stderr, PLPLOT_LIB_ENV"=\"%s\"\n", dn); /* what IS set? */
    }
#endif  /* PLPLOT_LIB_ENV */

/****	search current directory	****/

    if ((file = pdf_fopen(fn, "rb")) != NULL)
        goto done;

/****	search PLPLOT_HOME_ENV/lib = $(PLPLOT_HOME)/lib	****/

#if defined (PLPLOT_HOME_ENV)
    if ((dn = getenv(PLPLOT_HOME_ENV)) != NULL) {
        plGetName(dn, "lib", fn, &fs);

        if ((file = pdf_fopen(fs, "rb")) != NULL)
            goto done;
        fprintf(stderr, PLPLOT_HOME_ENV"=\"%s\"\n",dn); /* what IS set? */
    }
#endif  /* PLPLOT_HOME_ENV/lib */

/**** 	search installed location	****/

#if defined (DATA_DIR)
    plGetName(DATA_DIR, "", fn, &fs);

    if ((file = pdf_fopen(fs, "rb")) != NULL)
        goto done;
#endif  /* DATA_DIR */

/**** 	search hardwired location	****/

#ifdef PLLIBDEV
    plGetName(PLLIBDEV, "", fn, &fs);

    if ((file = pdf_fopen(fs, "rb")) != NULL)
	goto done;
#endif	/* PLLIBDEV */

#ifdef macintosh
    file = plMacLibOpen(fn);
    if (file != NULL)
        goto done;
#endif /* macintosh */

    if (plplotLibDir != NULL) {
	plGetName(plplotLibDir, "", fn, &fs);
	if ((file = pdf_fopen(fs, "rb")) != NULL)
	    goto done;

    }
    
/**** 	not found, give up 	****/

    return NULL;

 done:
    free_mem(fs);
    return (file);
}

/*--------------------------------------------------------------------------*\
 * int plFindName
 *
 * Authors: Paul Dubois (LLNL), others?
 * This function is in the public domain.
 *
 * Given a pathname, determine if it is a symbolic link.  If so, continue
 * searching to the ultimate terminus - there may be more than one link.
 * Use the error value to determine when the terminus is reached, and to
 * determine if the pathname really exists.  Then stat it to determine
 * whether it's executable.  Return 0 for an executable, errno otherwise.
 * Note that 'p' _must_ have at least one '/' character - it does by
 * construction in this program.  The contents of the array pointed to by
 * 'p' are changed to the actual pathname if findname is successful.
 *
 * This function is only defined under Unix for now.
\*--------------------------------------------------------------------------*/

#ifdef do_not_do // changed, this is unnessisary in this build anyway
int 
plFindName(char *p)
{
    int n;
    char buf[1024], *cp;
    extern int errno;
    struct stat sbuf;

    pldebug("plFindName", "Trying to find %s\n", p);
    while ((n = readlink(p, buf, 1024)) > 0) {
	pldebug("plFindName", "Readlink read %d chars at: %s\n", n, p);
	if (buf[0] == '/') {
	/* Link is an absolute path */

	    strncpy(p, buf, n);
	    p[n] = '\0';
	    pldebug("plFindName", "Link is absolute: %s\n", p);
	}
	else {
	/* Link is relative to its directory; make it absolute */

	    cp = 1 + strrchr(p, '/');
	    strncpy(cp, buf, n);
	    cp[n] = '\0';
	    pldebug("plFindName",
		    "Link is relative: %s\n\tTotal path:%s\n", cp, p);
	}
    }

/* This macro not defined on the NEC SX-3 */

#ifdef SX
#define S_ISREG(mode)   (mode & S_IFREG)
#endif

/* SGI machines return ENXIO instead of EINVAL Dubois 11/92 */

    if (errno == EINVAL || errno == ENXIO) {
	pldebug("plFindName", "%s may be the one...\n", p);
	if ((stat(p, &sbuf) == 0) && S_ISREG(sbuf.st_mode)) {
	    pldebug("plFindName", "%s is a regular file\n", p);
	    return (access(p, X_OK));
	}
    }
    pldebug("plFindName", "%s found but is not executable\n", p);
    return (errno ? errno : -1);
}

#else
int 
plFindName(char *p)
{
    return 1;
}
#endif

/*--------------------------------------------------------------------------*\
 * void plGetName()
 *
 * Gets search name for file by concatenating the dir, subdir, and file
 * name, allocating memory as needed.  The appropriate delimiter is added
 * after the dir specification as necessary.  The caller is responsible
 * for freeing the malloc'ed memory.
\*--------------------------------------------------------------------------*/

void
plGetName(char *dir, char *subdir, char *filename, char **filespec)
{
    int lfilespec;

/* Malloc space for filespec */

    free_mem(*filespec);
    lfilespec = 10;
    lfilespec = strlen(dir) + strlen(subdir) + strlen(filename) + 10;
    *filespec = (char *) malloc(lfilespec);

    strcpy(*filespec, dir);

    if (*subdir != '\0') {
	strcat_delim(*filespec);
	strcat(*filespec, subdir);
    }
    if (*filename != '\0') {
	strcat_delim(*filespec);
	strcat(*filespec, filename);
    }
}

/*--------------------------------------------------------------------------*\
 * void strcat_delim()
 *
 * Append path name deliminator if necessary (does not add one if one's
 * there already, or if dealing with a colon-terminated device name).
\*--------------------------------------------------------------------------*/

static void
strcat_delim(char *dirspec)
{
    int ldirspec = strlen(dirspec);
#if defined (MSDOS)
    if (dirspec[ldirspec-1] != '\\')
	strcat(dirspec, "\\");
#elif defined (macintosh)
    if (dirspec[ldirspec-1] != ':')
        strcat(dirspec, ":");
#else           /* unix is the default */
    if (dirspec[ldirspec-1] != '/')
	strcat(dirspec, "/");
#endif
}

/*--------------------------------------------------------------------------*\
 * plcol_interp()
 *
 * Initializes device cmap 1 entry by interpolation from pls->cmap1
 * entries.  Returned PLColor is supposed to represent the i_th color
 * out of a total of ncol colors in the current color scheme.
\*--------------------------------------------------------------------------*/

void
plcol_interp(PLStream *pls, PLColor *newcolor, int i, int ncol)
{
    PLFLT x, delta;
    int il, ir;

    x = (double) (i * (pls->ncol1-1)) / (double) (ncol-1);
    il = x;
    ir = il + 1;
    delta = x - il;

    if (ir > pls->ncol1 || il < 0)
	fprintf(stderr, "Invalid color\n");

    else if (ir == pls->ncol1 || (delta == 0.)) {
	newcolor->r = pls->cmap1[il].r;
	newcolor->g = pls->cmap1[il].g;
	newcolor->b = pls->cmap1[il].b;
    }
    else {
	newcolor->r = (1.-delta) * pls->cmap1[il].r + delta * pls->cmap1[ir].r;
	newcolor->g = (1.-delta) * pls->cmap1[il].g + delta * pls->cmap1[ir].g;
	newcolor->b = (1.-delta) * pls->cmap1[il].b + delta * pls->cmap1[ir].b;
    }
}

/*--------------------------------------------------------------------------*\
 * plOpenFile()
 *
 * Opens file for output, prompting if not set.
 * Prints extra newline at end to make output look better in batch runs.
 * A file name of "-" indicates output to stdout.
\*--------------------------------------------------------------------------*/

#define MAX_NUM_TRIES	10
void
plOpenFile(PLStream *pls)
{
    int i = 0, count = 0;
    size_t len;
    char line[256];

    while (pls->OutFile == NULL) {

/* Setting pls->FileName = NULL forces creation of a new family member */
/* You should also free the memory associated with it if you do this */

	if (pls->family && pls->BaseName != NULL)
	    plP_getmember(pls);

/* Prompt if filename still not known */

	if (pls->FileName == NULL) {
	    do {
		fprintf(stdout, "Enter graphics output file name: ");
		if (!fgets(line, sizeof(line), stdin)) {
			return;
		}
		len = strlen(line);
		if (len)
		    len--;
		line[len] = '\0';	/* strip new-line */
		count++;		/* count zero entries */
	    } while (!len && count < MAX_NUM_TRIES);
	    plP_sfnam(pls, line);
	}

/* If name is "-", send to stdout */

	if ( ! strcmp(pls->FileName, "-")) {
	    pls->OutFile = stdout;
	    pls->output_type = 1;
	    break;
	}

/* Need this here again, for prompted family initialization */

	if (pls->family && pls->BaseName != NULL) 
	    plP_getmember(pls);

	if (i++ > 10)
	    plexit("Too many tries.");

	if ((pls->OutFile = fopen(pls->FileName, "wb+")) == NULL) 
	    fprintf(stdout, "Can't open %s.\n", pls->FileName);
	else
	  {
//	    fprintf(stderr, "Opened %s\n", pls->FileName);
// 	    this is not needed
	  }
    }
}

/*--------------------------------------------------------------------------*\
 * plP_getmember()
 *
 * Sets up next file member name (in pls->FileName), but does not open it.
\*--------------------------------------------------------------------------*/

void
plP_getmember(PLStream *pls)
{
    char tmp[256];

    if (pls->FileName == NULL)
	pls->FileName = (char *) malloc(10 + strlen(pls->BaseName));

    sprintf(tmp, "%s.%%0%1ii", pls->BaseName, (int) pls->fflen);
    sprintf(pls->FileName, tmp, pls->member);
}

/*--------------------------------------------------------------------------*\
 * plP_sfnam()
 *
 * Sets up file name & family stem name.
 * Reserve some extra space (5 chars) to hold an optional member number.
\*--------------------------------------------------------------------------*/

void
plP_sfnam(PLStream *pls, const char *fnam)
{
    pls->OutFile = NULL;

    if (pls->FileName != NULL)
	free((void *) pls->FileName);

    pls->FileName = (char *) malloc(10 + strlen(fnam));

    strcpy(pls->FileName, fnam);

    if (pls->BaseName != NULL)
	free((void *) pls->BaseName);

    pls->BaseName = (char *) malloc(10 + strlen(fnam));

    strcpy(pls->BaseName, fnam);
}

/*--------------------------------------------------------------------------*\
 * plFamInit()
 *
 * Initializes family file parameters.
\*--------------------------------------------------------------------------*/

void
plFamInit(PLStream *pls)
{
    if (pls->family) {
	pls->bytecnt = 0;
	if ( ! pls->member)
	    pls->member = 1;
	if ( ! pls->finc)
	    pls->finc = 1;
	if ( ! pls->fflen)
	    pls->fflen = 1;
	if ( ! pls->bytemax)
	    pls->bytemax = PL_FILESIZE_KB * 1000;
    }
}

/*--------------------------------------------------------------------------*\
 * plGetFam()
 *
 * Starts new member file of family file set if necessary.
 *
 * Note each member file is a complete graphics file (can be printed
 * individually), although 'plrender' will treat a family as a single
 * logical file if given the family name instead of the member name.
\*--------------------------------------------------------------------------*/

void
plGetFam(PLStream *pls)
{
    PLFLT xpmm_loc, ypmm_loc;
    if (pls->family) {
	if (pls->bytecnt > pls->bytemax || pls->famadv) {
	    plP_tidy();
	    pls->member += pls->finc;
	    pls->famadv = 0;
	    plP_init();
	   /* Apply compensating factor to original xpmm and ypmm so that 
	    * character aspect ratio is preserved when overall aspect ratio
	    * is changed. */
	    plP_gpixmm(&xpmm_loc, &ypmm_loc);
	    plP_setpxl(xpmm_loc*plsc->caspfactor, ypmm_loc/plsc->caspfactor); 
	    return;
	}
    }
}

/*--------------------------------------------------------------------------*\
 * plRotPhy()
 *
 * Rotates physical coordinates if necessary for given orientation.
 * Each time orient is incremented, the plot is rotated 90 deg clockwise.
 * Note: this is now used only to rotate by 90 degrees for devices that
 * expect portrait mode.
\*--------------------------------------------------------------------------*/

void
plRotPhy(PLINT orient, PLINT xmin, PLINT ymin, PLINT xmax, PLINT ymax,
	 int *px, int *py)
{
    int x, y;

    x = *px;
    y = *py;

    switch (orient%4) {

    case 1:
	*px = xmin + (y - ymin);
	*py = ymin + (xmax - x);
	break;

    case 2:
	*px = xmin + (xmax - x);
	*py = ymin + (ymax - y);
	break;

    case 3:
	*px = xmin + (ymax - y);
	*py = ymin + (x - xmin);
	break;

    default:
	break;			/* do nothing */
    }
}

/*--------------------------------------------------------------------------*\
 * plAllocDev()
 *
 * Allocates a standard PLDev structure for device-specific data, stores
 * the address in pls->dev, and returns the address as well.
\*--------------------------------------------------------------------------*/

PLDev *
plAllocDev(PLStream *pls)
{
    if (pls->dev != NULL)
	free((void *) pls->dev);

    pls->dev = calloc(1, (size_t) sizeof(PLDev));
    if (pls->dev == NULL)
	plexit("plAllocDev: cannot allocate memory\n");

    return (PLDev *) pls->dev;
}

/*--------------------------------------------------------------------------*\
 * plGinInit()
 *
 * Just fills in the PLGraphicsIn with appropriate initial values.
\*--------------------------------------------------------------------------*/

void
plGinInit(PLGraphicsIn *gin)
{
    gin->type = 0;
    gin->state = 0;
    gin->keysym = 0;
    gin->button = 0;
    gin->string[0] = '\0';
    gin->pX = gin->pY = -1;
    gin->dX = gin->dY = 0.;
    gin->wX = gin->wY = 0.;
}

/*--------------------------------------------------------------------------*\
 * plGetInt()
 *
 * Prompts human to input an integer in response to given message.
\*--------------------------------------------------------------------------*/

PLINT
plGetInt(char *s)
{
    int m;
    int i = 0;
    char line[256];

    while (i++ < 10) {
	fprintf(stdout, "%s", s);
	if (! (fgets(line, sizeof(line), stdin))) {
		return 0;
	}
#ifdef MSDOS
	m = atoi(line);
	return (m);
#else
	if (sscanf(line, "%d", &m) == 1)
	    return (m);
	fprintf(stdout, "No value or value out of range; please try again\n");
#endif
    }
    plexit("Too many tries.");
    return (0);
}

/*--------------------------------------------------------------------------*\
 * plGetFlt()
 *
 * Prompts human to input a float in response to given message.
\*--------------------------------------------------------------------------*/

PLFLT
plGetFlt(char *s)
{
    PLFLT m;
    double m1;
    int i = 0;
    char line[256];

    while (i++ < 10) {
	fprintf(stdout, "%s", s);
	if (! (fgets(line, sizeof(line), stdin))) {
		return (0.);
	}
#ifdef MSDOS
	m = atof(line);
	return (m);
#else
	if (sscanf(line, "%lf", &m1) == 1) {
	    m = (PLFLT) m1;
	    return (m);
	}
	fprintf(stdout, "No value or value out of range; please try again\n");
#endif
    }
    plexit("Too many tries.");
    return (0.);
}

/*--------------------------------------------------------------------------*\
 * plstrdup()
 *
 * A replacement for strdup(), which isn't portable.
 * Caller responsible for freeing the allocated memory.
\*--------------------------------------------------------------------------*/

char *
plstrdup(const char *src)
{
    char *dest = (char *) malloc( (strlen(src) + 1) * sizeof(char) );
    if (dest != NULL)
	strcpy(dest, src);
    else
	plabort("Out of memory");

    return dest;
}
 
