/* $Id: plbuf.c,v 1.1 2004/03/01 20:54:50 cozmic Exp $

    Handle plot buffer.

    Copyright 1992
    Maurice LeBrun

    This software may be freely copied, modified and redistributed without
    fee provided that this copyright notice is preserved intact on all
    copies and modified copies.

    There is no warranty or other guarantee of fitness of this software.
    It is provided solely "as is". The author(s) disclaim(s) all
    responsibility and liability with respect to this software's usage or
    its effect upon hardware or computer systems.
*/

#define NEED_PLDEBUG
#include "plplotP.h"
#include "drivers.h"
#include "metadefs.h"

#include <string.h>

/* Function prototypes */

static int	rd_command	(PLStream *pls, U_CHAR *p_c);
static int	wr_command	(PLStream *pls, U_CHAR c);
static void	plbuf_control	(PLStream *pls, U_CHAR c);

static void	rdbuf_init	(PLStream *pls);
static void	rdbuf_line	(PLStream *pls);
static void	rdbuf_polyline	(PLStream *pls);
static void	rdbuf_eop	(PLStream *pls);
static void	rdbuf_bop	(PLStream *pls);
static void	rdbuf_state	(PLStream *pls);
static void	rdbuf_esc	(PLStream *pls);

static void	plbuf_fill	(PLStream *pls);
static void	rdbuf_fill	(PLStream *pls);
static void	plbuf_swin	(PLStream *pls, PLWindow *plwin);
static void	rdbuf_swin	(PLStream *pls);

/*--------------------------------------------------------------------------*\
 * plbuf_init()
 *
 * Initialize device.
 * Actually just disables writes if plot buffer is already open (occurs
 * when one stream is cloned, as in printing).
\*--------------------------------------------------------------------------*/

void
plbuf_init(PLStream *pls)
{
    dbug_enter("plbuf_init");

    pls->plbuf_read = FALSE;
    if (pls->plbufFile != NULL) 
	pls->plbuf_write = FALSE;
}

/*--------------------------------------------------------------------------*\
 * plbuf_line()
 *
 * Draw a line in the current color from (x1,y1) to (x2,y2).
\*--------------------------------------------------------------------------*/

void
plbuf_line(PLStream *pls, short x1a, short y1a, short x2a, short y2a)
{
    short xpl[2], ypl[2];

    dbug_enter("plbuf_line");

    wr_command(pls, (U_CHAR) LINE);

    xpl[0] = x1a;
    xpl[1] = x2a;
    ypl[0] = y1a;
    ypl[1] = y2a;

    fwrite(xpl, sizeof(short), 2, pls->plbufFile);
    fwrite(ypl, sizeof(short), 2, pls->plbufFile);
}

/*--------------------------------------------------------------------------*\
 * plbuf_polyline()
 *
 * Draw a polyline in the current color.
\*--------------------------------------------------------------------------*/

void
plbuf_polyline(PLStream *pls, short *xa, short *ya, PLINT npts)
{
    dbug_enter("plbuf_polyline");

    wr_command(pls, (U_CHAR) POLYLINE);
    fwrite(&npts, sizeof(PLINT), 1, pls->plbufFile);

    fwrite(xa, sizeof(short), npts, pls->plbufFile);
    fwrite(ya, sizeof(short), npts, pls->plbufFile);
}

/*--------------------------------------------------------------------------*\
 * plbuf_eop()
 *
 * End of page.
\*--------------------------------------------------------------------------*/

void
plbuf_eop(PLStream *pls)
{
    dbug_enter("plbuf_eop");
}

/*--------------------------------------------------------------------------*\
 * plbuf_bop()
 *
 * Set up for the next page.
 * To avoid problems redisplaying partially filled pages, on each BOP the
 * old file is thrown away and a new one is obtained.  This way we can just
 * read up to EOF to get everything on the current page.
 *
 * Also write state information to ensure the next page is correct.
\*--------------------------------------------------------------------------*/

void
plbuf_bop(PLStream *pls)
{
    dbug_enter("plbuf_bop");

    plbuf_tidy(pls);

    pls->plbufFile = tmpfile();
    if (pls->plbufFile == NULL) 
	plexit("plbuf_init: Error opening plot data storage file.");

    wr_command(pls, (U_CHAR) BOP);
    plbuf_state(pls, PLSTATE_COLOR0);
    plbuf_state(pls, PLSTATE_WIDTH);
}

/*--------------------------------------------------------------------------*\
 * plbuf_tidy()
 *
 * Close graphics file
\*--------------------------------------------------------------------------*/

void
plbuf_tidy(PLStream *pls)
{
    dbug_enter("plbuf_tidy");

    if (pls->plbufFile == NULL)
	return;

    fclose(pls->plbufFile);
    pls->plbufFile = NULL;
}

/*--------------------------------------------------------------------------*\
 * plbuf_state()
 *
 * Handle change in PLStream state (color, pen width, fill attribute, etc).
\*--------------------------------------------------------------------------*/

void 
plbuf_state(PLStream *pls, PLINT op)
{
    dbug_enter("plbuf_state");

    wr_command(pls, (U_CHAR) CHANGE_STATE);
    wr_command(pls, (U_CHAR) op);

    switch (op) {

    case PLSTATE_WIDTH: {
	U_CHAR width = pls->width;

	fwrite(&width, sizeof(U_CHAR), 1, pls->plbufFile);
	break;
    }

    case PLSTATE_COLOR0: {
	U_CHAR icol0 = pls->icol0;
	U_CHAR r = pls->curcolor.r;
	U_CHAR g = pls->curcolor.g;
	U_CHAR b = pls->curcolor.b;

	fwrite(&icol0, sizeof(U_CHAR), 1, pls->plbufFile);
	if (icol0 == PL_RGB_COLOR) {
	    fwrite(&r, sizeof(U_CHAR), 1, pls->plbufFile);
	    fwrite(&g, sizeof(U_CHAR), 1, pls->plbufFile);
	    fwrite(&b, sizeof(U_CHAR), 1, pls->plbufFile);
	}
	break;
    }

    case PLSTATE_COLOR1: {
	U_CHAR icol1 = pls->icol1;

	fwrite(&icol1, sizeof(U_CHAR), 1, pls->plbufFile);
	break;
    }

    case PLSTATE_FILL:{
	signed char patt = pls->patt;

	fwrite(&patt, sizeof(signed char), 1, pls->plbufFile);
	break;
    }
    }
}


/*--------------------------------------------------------------------------*\
 * plbuf_image()
 *
 * write image described in points pls->dev_x[], pls->dev_y[], pls->dev_z[].
 *                      pls->nptsX, pls->nptsY.
\*--------------------------------------------------------------------------*/

static void
plbuf_image(PLStream *pls, IMG_DT *img_dt)
{
    PLINT npts = pls->dev_nptsX * pls->dev_nptsY;

    dbug_enter("plbuf_image");

    fwrite(&pls->dev_nptsX, sizeof(PLINT), 1, pls->plbufFile);
    fwrite(&pls->dev_nptsY, sizeof(PLINT), 1, pls->plbufFile);

    fwrite(&img_dt->xmin, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&img_dt->ymin, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&img_dt->dx, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&img_dt->dy, sizeof(PLFLT), 1, pls->plbufFile);

    fwrite(&pls->dev_zmin, sizeof(short), 1, pls->plbufFile);
    fwrite(&pls->dev_zmax, sizeof(short), 1, pls->plbufFile);

    fwrite(pls->dev_ix, sizeof(short), npts, pls->plbufFile);
    fwrite(pls->dev_iy, sizeof(short), npts, pls->plbufFile);
    fwrite(pls->dev_z, sizeof(unsigned short), (pls->dev_nptsX-1)*(pls->dev_nptsY-1), pls->plbufFile);
}

/*--------------------------------------------------------------------------*\
 * plbuf_esc()
 *
 * Escape function.  Note that any data written must be in device
 * independent form to maintain the transportability of the metafile.
 *
 * Functions:
 *
 *	PLESC_FILL	Fill polygon
 *	PLESC_SWIN	Set plot window parameters
 *      PLESC_IMAGE     Draw image
\*--------------------------------------------------------------------------*/

void
plbuf_esc(PLStream *pls, PLINT op, void *ptr)
{
    dbug_enter("plbuf_esc");

    wr_command(pls, (U_CHAR) ESCAPE);
    wr_command(pls, (U_CHAR) op);

    switch (op) {
    case PLESC_FILL:
	plbuf_fill(pls);
	break;
    case PLESC_SWIN:
	plbuf_swin(pls, (PLWindow *) ptr);
	break;
    case PLESC_IMAGE:
	plbuf_image(pls, (IMG_DT *) ptr);
	break;
    }
}

/*--------------------------------------------------------------------------*\
 * plbuf_fill()
 *
 * Fill polygon described in points pls->dev_x[] and pls->dev_y[].
\*--------------------------------------------------------------------------*/

static void
plbuf_fill(PLStream *pls)
{
    dbug_enter("plbuf_fill");

    fwrite(&pls->dev_npts, sizeof(PLINT), 1, pls->plbufFile);
    fwrite(pls->dev_x, sizeof(short), pls->dev_npts, pls->plbufFile);
    fwrite(pls->dev_y, sizeof(short), pls->dev_npts, pls->plbufFile);
}

/*--------------------------------------------------------------------------*\
 * plbuf_swin()
 *
 * Set up plot window parameters. 
\*--------------------------------------------------------------------------*/

static void
plbuf_swin(PLStream *pls, PLWindow *plwin)
{
    fwrite(&plwin->dxmi, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&plwin->dxma, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&plwin->dymi, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&plwin->dyma, sizeof(PLFLT), 1, pls->plbufFile);

    fwrite(&plwin->wxmi, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&plwin->wxma, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&plwin->wymi, sizeof(PLFLT), 1, pls->plbufFile);
    fwrite(&plwin->wyma, sizeof(PLFLT), 1, pls->plbufFile);
}

/*--------------------------------------------------------------------------*\
 * Routines to read from & process the plot buffer.
\*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*\
 * rdbuf_init()
 *
 * Initialize device.
\*--------------------------------------------------------------------------*/

static void
rdbuf_init(PLStream *pls)
{
    dbug_enter("rdbuf_init");
}

/*--------------------------------------------------------------------------*\
 * rdbuf_line()
 *
 * Draw a line in the current color from (x1,y1) to (x2,y2).
\*--------------------------------------------------------------------------*/

static void
rdbuf_line(PLStream *pls)
{
    short xpl[2], ypl[2];
    PLINT npts = 2;

    dbug_enter("rdbuf_line");

    fread(xpl, sizeof(short), npts, pls->plbufFile);
    fread(ypl, sizeof(short), npts, pls->plbufFile);

    plP_line(xpl, ypl);
}

/*--------------------------------------------------------------------------*\
 * rdbuf_polyline()
 *
 * Draw a polyline in the current color.
\*--------------------------------------------------------------------------*/

static void
rdbuf_polyline(PLStream *pls)
{
    short xpl[PL_MAXPOLY], ypl[PL_MAXPOLY];
    PLINT npts;

    dbug_enter("rdbuf_polyline");

    fread(&npts, sizeof(PLINT), 1, pls->plbufFile);
    fread(xpl, sizeof(short), npts, pls->plbufFile);
    fread(ypl, sizeof(short), npts, pls->plbufFile);

    plP_polyline(xpl, ypl, npts);
}

/*--------------------------------------------------------------------------*\
 * rdbuf_eop()
 *
 * End of page.
\*--------------------------------------------------------------------------*/

static void
rdbuf_eop(PLStream *pls)
{
    dbug_enter("rdbuf_eop");
}

/*--------------------------------------------------------------------------*\
 * rdbuf_bop()
 *
 * Set up for the next page.
\*--------------------------------------------------------------------------*/

static void
rdbuf_bop(PLStream *pls)
{
    dbug_enter("rdbuf_bop");

    pls->nplwin = 0;
}

/*--------------------------------------------------------------------------*\
 * rdbuf_state()
 *
 * Handle change in PLStream state (color, pen width, fill attribute, etc).
\*--------------------------------------------------------------------------*/

static void 
rdbuf_state(PLStream *pls)
{
    U_CHAR op;

    dbug_enter("rdbuf_state");

    fread(&op, sizeof(U_CHAR), 1, pls->plbufFile);

    switch (op) {

    case PLSTATE_WIDTH:{
	U_CHAR width;

	fread(&width, sizeof(U_CHAR), 1, pls->plbufFile);
	pls->width = width;
	plP_state(PLSTATE_WIDTH);

	break;
    }

    case PLSTATE_COLOR0:{
	U_CHAR icol0, r, g, b;

	fread(&icol0, sizeof(U_CHAR), 1, pls->plbufFile);
	if (icol0 == PL_RGB_COLOR) {
	    fread(&r, sizeof(U_CHAR), 1, pls->plbufFile);
	    fread(&g, sizeof(U_CHAR), 1, pls->plbufFile);
	    fread(&b, sizeof(U_CHAR), 1, pls->plbufFile);
	}
	else {
	    if ((int) icol0 > 15) {
		plwarn("rdbuf_state: Color map 0 entry hosed");
		icol0 = 1;
	    }
	    r = pls->cmap0[icol0].r;
	    g = pls->cmap0[icol0].g;
	    b = pls->cmap0[icol0].b;
	}
	pls->icol0 = icol0;
	pls->curcolor.r = r;
	pls->curcolor.g = g;
	pls->curcolor.b = b;

	plP_state(PLSTATE_COLOR0);
	break;
    }

    case PLSTATE_COLOR1: {
	U_CHAR icol1;

	fread(&icol1, sizeof(U_CHAR), 1, pls->plbufFile);

	pls->icol1 = icol1;
	pls->curcolor.r = pls->cmap1[icol1].r;
	pls->curcolor.g = pls->cmap1[icol1].g;
	pls->curcolor.b = pls->cmap1[icol1].b;

	plP_state(PLSTATE_COLOR1);
	break;
    }

    case PLSTATE_FILL: {
	signed char patt;

	fread(&patt, sizeof(signed char), 1, pls->plbufFile);

	pls->patt = patt;
	plP_state(PLSTATE_FILL);
	break;
    }
    }
}

/*--------------------------------------------------------------------------*\
 * rdbuf_esc()
 *
 * Escape function.
 * Must fill data structure with whatever data that was written,
 * then call escape function.
 *
 * Note: it is best to only call the escape function for op-codes that
 * are known to be supported.
 *
 * Functions:
 *
 *	PLESC_FILL	Fill polygon
 *	PLESC_SWIN	Set plot window parameters
 *      PLESC_IMAGE     Draw image
\*--------------------------------------------------------------------------*/

static void
rdbuf_image(PLStream *pls);

static void
rdbuf_esc(PLStream *pls)
{
    U_CHAR op;

    dbug_enter("rdbuf_esc");

    fread(&op, sizeof(U_CHAR), 1, pls->plbufFile);

    switch (op) {
    case PLESC_FILL:
	rdbuf_fill(pls);
	break;
    case PLESC_SWIN:
	rdbuf_swin(pls);
	break;
    case PLESC_IMAGE:
	rdbuf_image(pls);
	break;
    }
}

/*--------------------------------------------------------------------------*\
 * rdbuf_fill()
 *
 * Fill polygon described by input points.
\*--------------------------------------------------------------------------*/

static void
rdbuf_fill(PLStream *pls)
{
    short xpl[PL_MAXPOLY], ypl[PL_MAXPOLY];
    PLINT npts;

    dbug_enter("rdbuf_fill");

    fread(&npts, sizeof(PLINT), 1, pls->plbufFile);
    fread(xpl, sizeof(short), npts, pls->plbufFile);
    fread(ypl, sizeof(short), npts, pls->plbufFile);
    
    plP_fill(xpl, ypl, npts);
}

/*--------------------------------------------------------------------------*\
 * rdbuf_image()
 *
 * .
\*--------------------------------------------------------------------------*/

static void
rdbuf_image(PLStream *pls)
{
  short *dev_ix, *dev_iy;
  unsigned short *dev_z, dev_zmin, dev_zmax;
  PLINT nptsX,nptsY, npts;
  PLFLT xmin, ymin, dx, dy;

    dbug_enter("rdbuf_image");

    fread(&nptsX, sizeof(PLINT), 1, pls->plbufFile);
    fread(&nptsY, sizeof(PLINT), 1, pls->plbufFile);
    npts = nptsX*nptsY;

    fread(&xmin, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&ymin, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&dx, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&dy, sizeof(PLFLT), 1, pls->plbufFile);

    fread(&dev_zmin, sizeof(short), 1, pls->plbufFile);
    fread(&dev_zmax, sizeof(short), 1, pls->plbufFile);

    dev_ix=(short *)malloc(npts*sizeof(short));
    dev_iy=(short *)malloc(npts*sizeof(short));
    dev_z=(unsigned short *)malloc((nptsX-1)*(nptsY-1)*sizeof(unsigned short));

    fread(dev_ix, sizeof(short), npts, pls->plbufFile);
    fread(dev_iy, sizeof(short), npts, pls->plbufFile);
    fread(dev_z, sizeof(unsigned short), (nptsX-1)*(nptsY-1), pls->plbufFile);

    plP_image(dev_ix, dev_iy, dev_z, nptsX, nptsY, xmin, ymin, dx, dy, dev_zmin, dev_zmax);

    free(dev_ix);
    free(dev_iy);
    free(dev_z);
}

/*--------------------------------------------------------------------------*\
 * rdbuf_swin()
 *
 * Set up plot window parameters. 
\*--------------------------------------------------------------------------*/

static void
rdbuf_swin(PLStream *pls)
{
    PLWindow plwin;

    fread(&plwin.dxmi, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&plwin.dxma, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&plwin.dymi, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&plwin.dyma, sizeof(PLFLT), 1, pls->plbufFile);

    fread(&plwin.wxmi, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&plwin.wxma, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&plwin.wymi, sizeof(PLFLT), 1, pls->plbufFile);
    fread(&plwin.wyma, sizeof(PLFLT), 1, pls->plbufFile);

    plP_swin(&plwin);
}

/*--------------------------------------------------------------------------*\
 * plRemakePlot()
 *
 * Rebuilds plot from plot buffer, usually in response to a window
 * resize or exposure event.
\*--------------------------------------------------------------------------*/

void
plRemakePlot(PLStream *pls)
{
    U_CHAR c;
    int plbuf_status;

    dbug_enter("plRemakePlot");

    if (pls->plbufFile == NULL)
	return;

    rewind(pls->plbufFile);

    plbuf_status = pls->plbuf_write;
    pls->plbuf_write = FALSE;
    pls->plbuf_read = TRUE;
    while (rd_command(pls, &c)) {
	plbuf_control(pls, c);
    }

    pls->plbuf_read = FALSE;
    pls->plbuf_write = plbuf_status;
}

/*--------------------------------------------------------------------------*\
 * plbuf_control()
 *
 * Processes commands read from the plot buffer.
\*--------------------------------------------------------------------------*/

static void
plbuf_control(PLStream *pls, U_CHAR c)
{
    static U_CHAR c_old = 0;

    dbug_enter("plbuf_control");

    switch ((int) c) {

    case INITIALIZE:
	rdbuf_init(pls);
	break;

    case EOP:
	rdbuf_eop(pls);
	break;

    case BOP:
	rdbuf_bop(pls);
	break;

    case CHANGE_STATE:
	rdbuf_state(pls);
	break;

    case LINE:
	rdbuf_line(pls);
	break;

    case POLYLINE:
	rdbuf_polyline(pls);
	break;

    case ESCAPE:
	rdbuf_esc(pls);
	break;

    default:
      pldebug("plbuf_control", "Unrecognized command %d, previous %d\n", c, c_old);
    }
    c_old = c;
}

/*--------------------------------------------------------------------------*\
 * rd_command()
 *
 * Read & return the next command
\*--------------------------------------------------------------------------*/

static int
rd_command(PLStream *pls, U_CHAR *p_c)
{
    int count;
    
    count = fread(p_c, sizeof(U_CHAR), 1, pls->plbufFile);
    return (count);
}

/*--------------------------------------------------------------------------*\
 * wr_command()
 *
 * Write the next command
\*--------------------------------------------------------------------------*/

static int
wr_command(PLStream *pls, U_CHAR c)
{
    U_CHAR c1 = c;
    int count;

    count = fwrite(&c1, sizeof(U_CHAR), 1, pls->plbufFile);
    return (count);
}
