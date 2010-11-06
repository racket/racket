/* $Id: plpage.c,v 1.1 2004/03/01 20:54:52 cozmic Exp $

	Page/subpage handling routines
*/

#include "plplotP.h"

/*--------------------------------------------------------------------------*\
 * void pladv()
 *
 * Advance to subpage "page", or to the next one if "page" = 0.
\*--------------------------------------------------------------------------*/

void
c_pladv(PLINT page)
{
    if (plsc->level < 1) {
	plabort("pladv: Please call plinit first");
	return;
    }

    if (page > 0 && page <= plsc->nsubx * plsc->nsuby)
	plsc->cursub = page;

    else if (page == 0) {
	if (plsc->cursub >= plsc->nsubx * plsc->nsuby) {
	    plP_eop();
	    plP_bop();
	    plsc->cursub = 1;
	}
	else
	    plsc->cursub++;
    }
    else {
	plabort("pladv: Invalid subpage number");
	return;
    }

    plP_setsub();
}

/*--------------------------------------------------------------------------*\
 * void plclear()
 *
 * Clear current subpage.  Subpages can be set with pladv before 
 * calling plclear. Not all drivers support this.
\*--------------------------------------------------------------------------*/

void
c_plclear(void)
{
    if (plsc->level < 1) {
	plabort("plclear: Please call plinit first");
	return;
    }

    if (plsc->dev_clear)
      plP_esc(PLESC_CLEAR, NULL);
    else { /* driver does not support clear, fill using background color */

	  short x[5], y[5];
	  int ocolor = plsc->icol0;

	  x[0] = x[3] = x[4] = plsc->sppxmi;
	  x[1] = x[2] = plsc->sppxma;
	  y[0] = y[1] = y[4] = plsc->sppymi;
	  y[2] = y[3] = plsc->sppyma;
	  plcol0(0);
	  plP_fill(x, y, 5);
	  plcol0(ocolor);
    }
}

/*--------------------------------------------------------------------------*\
 * void pleop()
 *
 * End current page.
\*--------------------------------------------------------------------------*/

void
c_pleop(void)
{
    if (plsc->level < 1) {
	plabort("pleop: Please call plinit first");
	return;
    }

    plsc->cursub = plsc->nsubx * plsc->nsuby;
    plP_eop();
}

/*--------------------------------------------------------------------------*\
 * void plbop()
 *
 * Start new page.  Should only be used with pleop().
\*--------------------------------------------------------------------------*/

void
c_plbop(void)
{
    if (plsc->level < 1) {
	plabort("pladv: Please call plinit first");
	return;
    }
    plP_bop();
    plsc->cursub = 1;
    plP_setsub();
}

/*--------------------------------------------------------------------------*\
 * void plP_subpInit()
 *
 * Set up plot parameters according to the number of subpages.
\*--------------------------------------------------------------------------*/

void
plP_subpInit(void)
{
    PLFLT scale, size_chr, size_sym, size_maj, size_min;

/* Subpage checks */

    if (plsc->nsubx <= 0)
	plsc->nsubx = 1;
    if (plsc->nsuby <= 0)
	plsc->nsuby = 1;

    plsc->cursub = 0;

/*
 * Set default sizes
 * Global scaling:
 *	Normalize to the page length for more uniform results.
 * 	A virtual page length of 200 mm is assumed.
 * Subpage scaling:
 *	Reduce sizes with plot area (non-proportional, so that character
 *	size doesn't get too small).
 */
    scale = 0.5 *
	((plsc->phyxma - plsc->phyxmi) / plsc->xpmm +
	 (plsc->phyyma - plsc->phyymi) / plsc->ypmm) / 200.0;

    if (plsc->nsuby > 1)
	scale /= sqrt((double) plsc->nsuby);

    size_chr = 4.0;
    size_sym = 4.0;		/* All these in virtual plot units */
    size_maj = 3.0;
    size_min = 1.5;

    plsc->chrdef = plsc->chrht = size_chr * scale;
    plsc->symdef = plsc->symht = size_sym * scale;
    plsc->majdef = plsc->majht = size_maj * scale;
    plsc->mindef = plsc->minht = size_min * scale;
}

/*--------------------------------------------------------------------------*\
 * void plP_setsub()
 *
 * Set up the subpage boundaries according to the current subpage selected.
\*--------------------------------------------------------------------------*/

void
plP_setsub(void)
{
    PLINT ix, iy;

    ix = (plsc->cursub - 1) % plsc->nsubx;
    iy = plsc->nsuby - (plsc->cursub - 1) / plsc->nsubx;

    plsc->spdxmi = (PLFLT) (ix)     / (PLFLT) (plsc->nsubx);
    plsc->spdxma = (PLFLT) (ix + 1) / (PLFLT) (plsc->nsubx);
    plsc->spdymi = (PLFLT) (iy - 1) / (PLFLT) (plsc->nsuby);
    plsc->spdyma = (PLFLT) (iy)     / (PLFLT) (plsc->nsuby);

    plsc->sppxmi = plP_dcpcx(plsc->spdxmi);
    plsc->sppxma = plP_dcpcx(plsc->spdxma);
    plsc->sppymi = plP_dcpcy(plsc->spdymi);
    plsc->sppyma = plP_dcpcy(plsc->spdyma);

    plP_sclp(plsc->sppxmi, plsc->sppxma, plsc->sppymi, plsc->sppyma);
}

/*--------------------------------------------------------------------------*\
 * void plgspa()
 *
 * Get subpage boundaries in absolute coordinates (mm from bottom
 * left-hand corner of page.
\*--------------------------------------------------------------------------*/

void
c_plgspa(PLFLT *xmin, PLFLT *xmax, PLFLT *ymin, PLFLT *ymax)
{
    if (plsc->level < 1) {
	plabort("plgspa: Please call plinit first");
	return;
    }
    *xmin = plP_dcmmx(plsc->spdxmi);
    *xmax = plP_dcmmx(plsc->spdxma);
    *ymin = plP_dcmmy(plsc->spdymi);
    *ymax = plP_dcmmy(plsc->spdyma);
}

/*--------------------------------------------------------------------------*\
 * int plGetCursor()
 *
 * Wait for graphics input event and translate to world coordinates.
 * Returns 0 if no translation to world coordinates is possible.  
 * Written by Paul Casteels.
\*--------------------------------------------------------------------------*/

int
plGetCursor(PLGraphicsIn *plg)
{
    plP_esc(PLESC_GETC, plg);
    return plTranslateCursor(plg);
}

/*--------------------------------------------------------------------------*\
 * int plTranslateCursor()
 *
 * Translates cursor position from relative device coordinates to world
 * coordinates.  Returns 0 if no translation to world coordinates is
 * possible.  Written by Paul Casteels and modified by Alan W. Irwin.
\*--------------------------------------------------------------------------*/

int
plTranslateCursor(PLGraphicsIn *plg)
{
    int window;
    c_plcalc_world(plg->dX, plg->dY, &plg->wX, &plg->wY, 
		   (PLINT *) &window);
    if ( window >= 0 ) { 
	plg->subwindow = window;
	return 1;
    }
    else
	return 0;
}

/*--------------------------------------------------------------------------*\
 * void c_plcalc_world
 *
 * Calculate world coordinates wx, and wy from relative device coordinates, rx
 * and ry.  Also, return the window index for which the world coordinates 
 * are valid. window is set to -1 and wx and wy to 0. if rx and ry do not 
 * correspond to valid world coordinates for any currently existing window.
 * Originally written by Paul Casteels and modified by Alan W. Irwin.
\*--------------------------------------------------------------------------*/

void
c_plcalc_world(PLFLT rx, PLFLT ry, PLFLT *wx, PLFLT *wy, PLINT *window)
{
    int i;
    int lastwin = plsc->nplwin - 1;
    int firstwin = MAX(plsc->nplwin - PL_MAXWINDOWS, 0);
    PLWindow *w;

    for (i = lastwin; i >= firstwin; i--) {
	w = &plsc->plwin[i % PL_MAXWINDOWS];
	if ((rx >= w->dxmi) &&
	    (rx <= w->dxma) &&
	    (ry >= w->dymi) &&
	    (ry <= w->dyma) ) {

	    *wx = w->wxmi + (rx - w->dxmi) * 
		(w->wxma - w->wxmi) / (w->dxma - w->dxmi);

	    *wy = w->wymi + (ry - w->dymi) * 
		(w->wyma - w->wymi) / (w->dyma - w->dymi);
	   
	    *window = i;

	    return;
	}
    }
    /* No valid window found with these relative coordinates. */
    *wx = 0.;
    *wy = 0.;
    *window = -1;
    return;
}
