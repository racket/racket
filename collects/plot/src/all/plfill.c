/* $Id: plfill.c,v 1.2 2005/03/17 21:39:21 eli Exp $

	Polygon pattern fill.
*/

#include "plplotP.h"

#define DTOR            0.0174533
#define BINC            50

struct point {
    PLINT x, y;
};
static PLINT bufferleng, buffersize, *buffer;

/* Static function prototypes */
/* INDENT OFF */

static int   compar	(const void *, const void *);
static void  addcoord	(PLINT, PLINT);
static void  tran	(PLINT *, PLINT *, PLFLT, PLFLT);
static void  buildlist	(PLINT, PLINT, PLINT, PLINT, PLINT, PLINT, PLINT);

/* INDENT ON */

/*----------------------------------------------------------------------*\
 * void plfill()
 *
 * Pattern fills the polygon bounded by the input points.
 * If hardware fill is used, a maximum of PL_MAXPOLY-1 vertices is allowed.
 * The final point is explicitly added if it doesn't match up to the first,
 * to prevent clipping problems.
\*----------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plfill(PLINT n, PLFLT *x, PLFLT *y)
{
    PLINT xpoly[PL_MAXPOLY], ypoly[PL_MAXPOLY];
    PLINT i;

    if (plsc->level < 3) {
	plabort("plfill: Please set up window first");
	return;
    }
    if (n < 3) {
	plabort("plfill: Not enough points in object");
	return;
    }
    if (n > PL_MAXPOLY-1) {
	plwarn("plfill: too many points in polygon");
	n = PL_MAXPOLY;
    }
    for (i = 0; i < n; i++) {
	xpoly[i] = plP_wcpcx(x[i]);
	ypoly[i] = plP_wcpcy(y[i]);
    }

    if (x[0] != x[n-1] || y[0] != y[n-1]) {
	n++;
	xpoly[n-1] = plP_wcpcx(x[0]);
	ypoly[n-1] = plP_wcpcy(y[0]);
    }

    plP_plfclp(xpoly, ypoly, n, plsc->clpxmi, plsc->clpxma,
	       plsc->clpymi, plsc->clpyma, plP_fill);
}

/*----------------------------------------------------------------------*\
 * void plfill3()
 *
 * Pattern fills the polygon in 3d bounded by the input points.
 * If hardware fill is used, a maximum of PL_MAXPOLY-1 vertices is allowed.
 * The final point is explicitly added if it doesn't match up to the first,
 * to prevent clipping problems.
\*----------------------------------------------------------------------*/

void
c_plfill3(PLINT n, PLFLT *x, PLFLT *y, PLFLT *z)
{
    PLFLT tx[PL_MAXPOLY], ty[PL_MAXPOLY], tz[PL_MAXPOLY];
    PLFLT *V[3];
    PLINT xpoly[PL_MAXPOLY], ypoly[PL_MAXPOLY];
    PLINT i;
    PLFLT xmin, xmax, ymin, ymax, zmin, zmax, zscale;

    if (plsc->level < 3) {
	plabort("plfill3: Please set up window first");
	return;
    }
    if (n < 3) {
	plabort("plfill3: Not enough points in object");
	return;
    }
    if (n > PL_MAXPOLY-1) {
	plwarn("plfill3: too many points in polygon");
	n = PL_MAXPOLY;
    }

    plP_gdom(&xmin, &xmax, &ymin, &ymax);
    plP_grange(&zscale, &zmin, &zmax);
    
    /* copy the vertices so we can clip without corrupting the input */
    for( i=0; i < n; i++ ) {
      tx[i] = x[i]; ty[i] = y[i]; tz[i] = z[i];
    }
    if (tx[0] != tx[n-1] || ty[0] != ty[n-1] || tz[0] != tz[n-1]) {
      tx[n] = tx[0]; ty[n] = ty[0]; tz[n] = tz[0];
      n++;
    }
    V[0] = tx; V[1] = ty; V[2] = tz;
    n = plP_clip_poly(n, V, 0,  1, -xmin);
    n = plP_clip_poly(n, V, 0, -1,  xmax);
    n = plP_clip_poly(n, V, 1,  1, -ymin);
    n = plP_clip_poly(n, V, 1, -1,  ymax);
    n = plP_clip_poly(n, V, 2,  1, -zmin);
    n = plP_clip_poly(n, V, 2, -1,  zmax);
    for( i=0; i < n; i++ ) {
	xpoly[i] = plP_wcpcx(plP_w3wcx( tx[i], ty[i], tz[i] ));
	ypoly[i] = plP_wcpcy(plP_w3wcy( tx[i], ty[i], tz[i] ));
	}

/* AWI: in the past we have used
 *  plP_fill(xpoly, ypoly, n);
 * here, but our educated guess is this fill should be done via the clipping
 * interface instead as below.
 * No example tests this code so one of our users will end up inadvertently
 * testing this for us. 
 *
 * jc: I have checked, and both versions does give the same result, i.e., clipping
 * to the window boundaries. The reason is that the above plP_clip_poly() does
 * the clipping. To check this, is enough to diminish the x/y/z min/max arguments in
 * plw3d() in x08c. But let's keep it, although 10% slower...
 */
    plP_plfclp(xpoly, ypoly, n, plsc->clpxmi, plsc->clpxma,
           plsc->clpymi, plsc->clpyma, plP_fill);
}

/*----------------------------------------------------------------------*\
 * void plfill_soft()
 *
 * Pattern fills in software the polygon bounded by the input points.
\*----------------------------------------------------------------------*/

void
plfill_soft(short *x, short *y, PLINT n)
{
    PLINT i, j;
    PLINT xp1, yp1, xp2, yp2, xp3, yp3;
    PLINT k, dinc;
    PLFLT ci, si;
    double temp;

    buffersize = 2 * BINC;
    buffer = (PLINT *) malloc((size_t) buffersize * sizeof(PLINT));
    if ( ! buffer) {
	plabort("plfill: Out of memory");
	return;
    }

/* Loop over sets of lines in pattern */

    for (k = 0; k < plsc->nps; k++) {
	bufferleng = 0;

        temp = DTOR * plsc->inclin[k] * 0.1;
        si = sin(temp) * plsc->ypmm;
        ci = cos(temp) * plsc->xpmm;

	/* normalize: 1 = si*si + ci*ci */

        temp = sqrt((double) (si*si + ci*ci));
	si /= temp;
	ci /= temp;

	dinc = plsc->delta[k] * SSQR(plsc->ypmm * ABS(ci),
				     plsc->xpmm * ABS(si)) / 1000.;

	if (dinc < 0) dinc = -dinc;
	if (dinc == 0) dinc = 1;

	xp1 = x[n-2];
	yp1 = y[n-2];
	tran(&xp1, &yp1, (PLFLT) ci, (PLFLT) si);

	xp2 = x[n-1];
	yp2 = y[n-1];
	tran(&xp2, &yp2, (PLFLT) ci, (PLFLT) si);

/* Loop over points in polygon */

	for (i = 0; i < n; i++) {
	    xp3 = x[i];
	    yp3 = y[i];
	    tran(&xp3, &yp3, (PLFLT) ci, (PLFLT) si);
	    buildlist(xp1, yp1, xp2, yp2, xp3, yp3, dinc);
	    xp1 = xp2;
	    yp1 = yp2;
	    xp2 = xp3;
	    yp2 = yp3;
	}

/* Sort list by y then x */

	qsort((void *) buffer, (size_t) bufferleng / 2,
	      (size_t) sizeof(struct point), compar);

/* OK, now do the hatching */

	i = 0;

	while (i < bufferleng) {
	    xp1 = buffer[i];
	    yp1 = buffer[i + 1];
	    i += 2;
	    xp2 = xp1;
	    yp2 = yp1;
	    tran(&xp1, &yp1, (PLFLT) ci, (PLFLT) (-si));
	    plP_movphy(xp1, yp1);
	    xp1 = buffer[i];
	    yp1 = buffer[i + 1];
	    i += 2;
	    if (yp2 != yp1) {
		fprintf(stderr, "plfill: oh oh we are lost\n");
		for (j = 0; j < bufferleng; j+=2) {
		    fprintf(stderr, "plfill: %d %d\n",
			    (int) buffer[j], (int) buffer[j+1]);
		}
		continue;	/* Uh oh we're lost */
	    }
	    tran(&xp1, &yp1, (PLFLT) ci, (PLFLT) (-si));
	    plP_draphy(xp1, yp1);
	}
    }
    free((void *) buffer);
}

/*----------------------------------------------------------------------*\
 * Utility functions
\*----------------------------------------------------------------------*/

static void
tran(PLINT *a, PLINT *b, PLFLT c, PLFLT d)
{
    PLINT ta, tb;

    ta = *a;
    tb = *b;

    *a = floor((double) (ta * c + tb * d + 0.5));
    *b = floor((double) (tb * c - ta * d + 0.5));
}

static void
buildlist(PLINT xp1, PLINT yp1, PLINT xp2, PLINT yp2, PLINT xp3, PLINT yp3,
	  PLINT dinc)
{
    PLINT min_y, max_y;
    PLINT dx, dy, cstep, nstep, ploty, plotx;

    dx = xp2 - xp1;
    dy = yp2 - yp1;

    if (dy == 0) {
	if (yp2 > yp3 && ((yp2 % dinc) == 0)) 
	    addcoord(xp2, yp2);
	return;
    }

    if (dy > 0) {
	cstep = 1;
	min_y = yp1;
	max_y = yp2;
    }
    else {
	cstep = -1;
	min_y = yp2;
	max_y = yp1;
    }

    nstep = (yp3 > yp2 ? 1 : -1);
    if (yp3 == yp2) nstep = 0;

    /* Build coordinate list */

    ploty = (min_y / dinc) * dinc;
    if (ploty < min_y) ploty += dinc;

    for (; ploty <= max_y; ploty += dinc) {
	if (ploty == yp1) continue;
	if (ploty == yp2) {
	    if (cstep == -nstep) continue;
	    if (yp2 == yp3 && yp1 > yp2) continue;
	}
	plotx = xp1 + floor(((double) (ploty - yp1) * dx) / dy + 0.5);
	addcoord(plotx, ploty);
    }
}

static void
addcoord(PLINT xp1, PLINT yp1)
{
    PLINT *temp;

    if (bufferleng + 2 > buffersize) {
	buffersize += 2 * BINC;
	temp = (PLINT *) realloc((void *) buffer,
				 (size_t) buffersize * sizeof(PLINT));
	if (!temp) {
	    free((void *) buffer);
	    plexit("plfill: Out of memory!");
	}
	buffer = temp;
    }

    buffer[bufferleng++] = xp1;
    buffer[bufferleng++] = yp1;
}

static int
compar(const void *pnum1, const void *pnum2)
{
    const struct point *pnt1, *pnt2;

    pnt1 = (const struct point *) pnum1;
    pnt2 = (const struct point *) pnum2;

    if (pnt1->y < pnt2->y)
	return -1;
    else if (pnt1->y > pnt2->y)
	return 1;

    /* Only reach here if y coords are equal, so sort by x */

    if (pnt1->x < pnt2->x)
	return -1;
    else if (pnt1->x > pnt2->x)
	return 1;

    return 0;
}
