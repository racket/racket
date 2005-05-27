/* $Id: plhist.c,v 1.1 2004/03/01 20:54:52 cozmic Exp $

	Histogram plotter.
*/

#include "plplotP.h"

/*----------------------------------------------------------------------*\
 * void plhist()
 *
 * Draws a histogram of n values of a variable in array data[0..n-1] in
 * the range datmin to datmax using nbin bins. If "flags"'s first bit is 1, the
 * histogram is plotted in the current window. If not, the routine calls
 * "plenv" to set up the graphics environment.
 * 
 * If flags's second bit is set, then items which fall outside the bin
 * range are ignored.
 * 
 * If flags's third bit is set, the outside bars are the same size
 * as the rest.  The default old behaviour was for the first and last
 * bars to expand visually to fill the entire available space.
\*----------------------------------------------------------------------*/

void
c_plhist(PLINT n, PLFLT *data, PLFLT datmin, PLFLT datmax,
	 PLINT nbin, PLINT flags)
{
    PLINT i, bin;
    PLFLT *x, *y, dx, ymax;

    if (plsc->level < 1) {
	plabort("plhist: Please call plinit first");
	return;
    }
    if (plsc->level < 3 && (flags & 1)) {
	plabort("plhist: Please set up window first");
	return;
    }
    if (datmin >= datmax) {
	plabort("plhist: Data range invalid");
	return;
    }
    if ( ! (x = (PLFLT *) malloc((size_t) nbin * sizeof(PLFLT)))) {
	plabort("plhist: Out of memory");
	return;
    }
    if ( ! (y = (PLFLT *) malloc((size_t) nbin * sizeof(PLFLT)))) {
	free((void *) x);
	plabort("plhist: Out of memory");
	return;
    }

    dx = (datmax - datmin) / nbin;
    for (i = 0; i < nbin; i++) {
	x[i] = datmin + i * dx;
	y[i] = 0.0;
    }

    for (i = 0; i < n; i++) {
	bin = (data[i] - datmin) / dx;
	if ((flags & 2) == 0) {
	    bin = bin > 0 ? bin : 0;
	    bin = bin < nbin ? bin : nbin - 1;
	    y[bin]++;
	} else {
	    if(bin >= 0 && bin < nbin) {
		y[bin]++;
	    }
	}
    }

    if (!(flags & 1)) {
	ymax = 0.0;
	for (i = 0; i < nbin; i++)
	    ymax = MAX(ymax, y[i]);

	plenv(datmin, datmax, (PLFLT) 0.0, (PLFLT) (1.1 * ymax), 0, 0);
    }
    /* We pass on the highest couple of bits to the 'plbin' routine */
    plbin(nbin, x, y, (flags & (4+8+16+32)) >> 2);
    free((void *) x);
    free((void *) y);
}

/*----------------------------------------------------------------------*\
 * void plbin()
 *
 * Plot a histogram using the arrays x and y to represent data values
 * and frequencies respectively. If flags first bit is false, x values 
 * denote the lower edge of the bin, and if it is true, they denote 
 * the center of the bin.  If flags second bit is true, then we assume 
 * the edge bins are the same size as the rest (i.e. the edge bins 
 * needn't go as far as the variables vpwxmi, vpwxma below).
\*----------------------------------------------------------------------*/

void
c_plbin(PLINT nbin, PLFLT *x, PLFLT *y, PLINT flags)
{
    PLINT i;
    PLFLT xmin, xmax, vpwxmi, vpwxma, vpwymi, vpwyma;

    if (plsc->level < 3) {
	plabort("plbin: Please set up window first");
	return;
    }

    /* Check x[i] are in ascending order */

    for (i = 0; i < nbin - 1; i++) {
	if (x[i] >= x[i + 1]) {
	    plabort("plbin: Elements of x array must be increasing");
	    return;
	}
    }

    plgvpw(&vpwxmi, &vpwxma, &vpwymi, &vpwyma);
    if (!(flags & 1)) {
	for (i = 0; i < nbin - 1; i++) {
	    if (!(flags & 4) || (y[i] != vpwymi)) {
		pljoin(x[i], vpwymi, x[i], y[i]);
		pljoin(x[i], y[i], x[i + 1], y[i]);
		pljoin(x[i + 1], y[i], x[i + 1], vpwymi);
	    }
	}
	if (flags & 2) {
	    if (!(flags & 4) || (y[i] != vpwymi)) {
		int xm = x[i] + (x[i] - x[i-1]);
		pljoin(x[i], vpwymi, x[i], y[i]);
		pljoin(x[i], y[i], xm, y[i]);
		pljoin(xm, y[i], xm, vpwymi);
	    }
	} else {
	    if (x[i] < vpwxma) {
		if (!(flags & 4) || (y[i] != vpwymi)) {
		    pljoin(x[i], vpwymi, x[i], y[i]);
		    pljoin(x[i], y[i], vpwxma, y[i]);
		    pljoin(vpwxma, y[i], vpwxma, vpwymi);
		}
	    }
	}
    } else {
	if (nbin < 2)
	    return;
	if (flags & 2) {
	    xmin = MAX(vpwxmi, 0.5 * (3 * x[0] - x[1]));
	} else {
	    xmin = vpwxmi;
	}
	/* Vince fixed bug May 1998 */
	xmax = MAX(0.5 * (x[0] + x[1]), vpwxmi);
	if (xmin < xmax) {
	    pljoin(xmin, vpwymi, xmin, y[0]);
	    pljoin(xmin, y[0], xmax, y[0]);
	    pljoin(xmax, y[0], xmax, vpwymi);
	}
	for (i = 1; i < nbin - 1; i++) {
	    xmin = xmax;
	    xmax = MIN(0.5 * (x[i] + x[i + 1]), vpwxma);
	    if (!(flags & 4) || (y[i] != vpwymi)) {
		pljoin(xmin, vpwymi, xmin, y[i]);
		pljoin(xmin, y[i], xmax, y[i]);
		pljoin(xmax, y[i], xmax, vpwymi);
	    }
	}
	xmin = xmax;
	xmax = vpwxma;
	if (flags & 2) {
	    xmax = MIN(vpwxma, 0.5 * (3 * x[i] - x[i-1]));
	} else {
	    xmax = vpwxma;
	}
	if (xmin < xmax) {
	    if (!(flags & 4) || (y[i] != vpwymi)) {
		pljoin(xmin, vpwymi, xmin, y[i]);
		pljoin(xmin, y[i], xmax, y[i]);
		pljoin(xmax, y[i], xmax, vpwymi);
	    }
	}
    }
}
