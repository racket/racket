/* $Id: pltick.c,v 1.2 2005/03/17 21:39:21 eli Exp $

	Routines for drawing error bars and tick marks.
*/

#include "plplotP.h"

/*----------------------------------------------------------------------*\
 * void plxtik()
 *
 * Draws a tick parallel to x.
\*----------------------------------------------------------------------*/

void
plxtik(PLINT x, PLINT y, PLINT below, PLINT above)
{
    plP_draphy(x, y);

    if (below != 0)
	plP_draphy(x, y - below);

    if (above != 0)
	plP_draphy(x, y + above);

    plP_draphy(x, y);
}

/*----------------------------------------------------------------------*\
 * void plytik()
 *
 * Draws a tick parallel to y.
\*----------------------------------------------------------------------*/

void
plytik(PLINT x, PLINT y, PLINT left, PLINT right)
{
    plP_draphy(x, y);

    if (left != 0)
	plP_draphy(x - left, y);

    if (right != 0)
	plP_draphy(x + right, y);

    plP_draphy(x, y);
}

/*----------------------------------------------------------------------*\
 * void plstik()
 *
 * Draws a slanting tick at position (mx,my) (measured in mm) of
 * vector length (dx,dy).
\*----------------------------------------------------------------------*/

void 
plstik(PLFLT mx, PLFLT my, PLFLT dx, PLFLT dy)
{
    plP_draphy(plP_mmpcx(mx), plP_mmpcy(my));
    plP_draphy(plP_mmpcx((PLFLT) (mx + dx)), plP_mmpcy((PLFLT) (my + dy)));
    plP_draphy(plP_mmpcx(mx), plP_mmpcy(my));
}

/*----------------------------------------------------------------------*\
 * void plerx1()
 *
 * Plot single horizontal error bar.
\*----------------------------------------------------------------------*/

static void
plerx1(PLFLT xmin, PLFLT xmax, PLFLT y)
{
    PLINT yminor;

    yminor = MAX(1.0, plsc->minht * plsc->ypmm);
    plP_movwor(xmin, y);
    plxtik(plP_wcpcx(xmin), plP_wcpcy(y), yminor, yminor);
    plP_drawor(xmax, y);
    plxtik(plP_wcpcx(xmax), plP_wcpcy(y), yminor, yminor);
}

/*----------------------------------------------------------------------*\
 * void plery1()
 *
 * Plot single vertical error bar.
\*----------------------------------------------------------------------*/

static void
plery1(PLFLT x, PLFLT ymin, PLFLT ymax)
{
    PLINT xminor;

    xminor = MAX(1.0, plsc->minht * plsc->xpmm);
    plP_movwor(x, ymin);
    plytik(plP_wcpcx(x), plP_wcpcy(ymin), xminor, xminor);
    plP_drawor(x, ymax);
    plytik(plP_wcpcx(x), plP_wcpcy(ymax), xminor, xminor);
}

/*----------------------------------------------------------------------*\
 * void plerrx()
 *
 * Plot horizontal error bars (xmin(i),y(i)) to (xmax(i),y(i)).
\*----------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plerrx(PLINT n, PLFLT *xmin, PLFLT *xmax, PLFLT *y)
{
    PLINT i;

    if (plsc->level < 3) {
	plabort("plerrx: Please set up window first");
	return;
    }

    for (i = 0; i < n; i++)
	plerx1(xmin[i], xmax[i], y[i]);
}

/*----------------------------------------------------------------------*\
 * void plerry()
 *
 * Plot vertical error bars (x,ymin(i)) to (x(i),ymax(i)).
\*----------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plerry(PLINT n, PLFLT *x, PLFLT *ymin, PLFLT *ymax)
{
    PLINT i;

    if (plsc->level < 3) {
	plabort("plerry: Please set up window first");
	return;
    }

    for (i = 0; i < n; i++)
	plery1(x[i], ymin[i], ymax[i]);
}
