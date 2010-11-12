/* $Id: plline.c,v 1.2 2005/03/17 21:39:21 eli Exp $

	Routines dealing with line generation.
*/

#include "plplotP.h"

#define INSIDE(ix,iy) (BETW(ix,xmin,xmax) && BETW(iy,ymin,ymax))

static PLINT xline[PL_MAXPOLY], yline[PL_MAXPOLY];

static PLINT lastx = PL_UNDEFINED, lasty = PL_UNDEFINED;

/* Function prototypes */

/* Draws a polyline within the clip limits. */

static void
pllclp(PLINT *x, PLINT *y, PLINT npts);

/* Get clipped endpoints */

static int
clipline(PLINT *p_x1, PLINT *p_y1, PLINT *p_x2, PLINT *p_y2,
	 PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax);

/* General line-drawing routine.  Takes line styles into account. */

static void
genlin(short *x, short *y, PLINT npts);

/* Draws a dashed line to the specified point from the previous one. */

static void
grdashline(short *x, short *y);

/*----------------------------------------------------------------------*\
 * void pljoin()
 *
 * Draws a line segment from (x1, y1) to (x2, y2).
\*----------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_pljoin(PLFLT x1, PLFLT y1, PLFLT x2, PLFLT y2)
{
    plP_movwor(x1, y1);
    plP_drawor(x2, y2);
}

/*----------------------------------------------------------------------*\
 * void plline()
 *
 * Draws line segments connecting a series of points.
\*----------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plline(PLINT n, PLFLT *x, PLFLT *y)
{
    if (plsc->level < 3) {
	plabort("plline: Please set up window first");
	return;
    }
    plP_drawor_poly(x, y, n);
}

/*----------------------------------------------------------------------*\
 * void plline3(n, x, y, z)
 *
 * Draws a line in 3 space.  You must first set up the viewport, the
 * 2d viewing window (in world coordinates), and the 3d normalized
 * coordinate box.  See x18c.c for more info.
 *
 * This version adds clipping against the 3d bounding box specified in plw3d

 MZ_DLLEXPORT\*----------------------------------------------------------------------*/
void
c_plline3(PLINT n, PLFLT *x, PLFLT *y, PLFLT *z)
{
    int i;
    PLFLT vmin[3], vmax[3], zscale;

    if (plsc->level < 3) {
	plabort("plline3: Please set up window first");
	return;
    }

    /* get the bounding box in 3d */
    plP_gdom(&vmin[0], &vmax[0], &vmin[1], &vmax[1]);
    plP_grange(&zscale, &vmin[2], &vmax[2]);

    /* interate over the vertices */
    for( i=0; i < n-1; i++ ) {
      PLFLT p0[3], p1[3];
      int axis;

      /* copy the end points of the segment to allow clipping */
      p0[0] = x[i]; p0[1] = y[i]; p0[2] = z[i];
      p1[0] = x[i+1]; p1[1] = y[i+1]; p1[2] = z[i+1];

      /* check against each axis of the bounding box */
      for(axis = 0; axis < 3; axis++) {
	if(p0[axis] < vmin[axis]) { /* first out */
	  if(p1[axis] < vmin[axis]) {
	    break; /* both endpoints out so quit */
	  } else {
	    int j;
	    /* interpolate to find intersection with box */
	    PLFLT t = (vmin[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	    p0[axis] = vmin[axis];
	    for(j = 1; j<3; j++) {
	      int k = (axis+j)%3;
	      p0[k] = (1-t)*p0[k] + t*p1[k];
	    }
	  }
	} else if(p1[axis] < vmin[axis]) { /* second out */
	  int j;
	  /* interpolate to find intersection with box */
	  PLFLT t = (vmin[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	  p1[axis] = vmin[axis];
	  for(j = 1; j<3; j++) {
	    int k = (axis+j)%3;
	    p1[k] = (1-t)*p0[k] + t*p1[k];
	  }
	}
	if(p0[axis] > vmax[axis]) { /* first out */
	  if(p1[axis] > vmax[axis]) {
	    break; /* both out so quit */
	  } else {
	    int j;
	    /* interpolate to find intersection with box */
	    PLFLT t = (vmax[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	    p0[axis] = vmax[axis];
	    for(j = 1; j<3; j++) {
	      int k = (axis+j)%3;
	      p0[k] = (1-t)*p0[k] + t*p1[k];
	    }
	  }
	} else if(p1[axis] > vmax[axis]) { /* second out */
	  int j;
	  /* interpolate to find intersection with box */
	  PLFLT t = (vmax[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	  p1[axis] = vmax[axis];
	  for(j = 1; j<3; j++) {
	    int k = (axis+j)%3;
	    p1[k] = (1-t)*p0[k] + t*p1[k];
	  }
	}
      }
      /* if we made it to here without "break"ing out of the loop, the 
	 remaining segment is visible */
      if( axis == 3 ) { /*  not clipped away */
	PLFLT u0, v0, u1, v1;
	u0 = plP_wcpcx(plP_w3wcx( p0[0], p0[1], p0[2] ));
	v0 = plP_wcpcy(plP_w3wcy( p0[0], p0[1], p0[2] ));
	u1 = plP_wcpcx(plP_w3wcx( p1[0], p1[1], p1[2] ));
	v1 = plP_wcpcy(plP_w3wcy( p1[0], p1[1], p1[2] ));
	plP_movphy(u0,v0);
	plP_draphy(u1,v1);
      }
    }
    return;
}
/*----------------------------------------------------------------------*\
 * void plpoly3( n, x, y, z, draw, ifcc )
 *
 * Draws a polygon in 3 space.  This differs from plline3() in that
 * this attempts to determine if the polygon is viewable.  If the back
 * of polygon is facing the viewer, then it isn't drawn.  If this
 * isn't what you want, then use plline3 instead.
 *
 * n specifies the number of points.  They are assumed to be in a
 * plane, and the directionality of the plane is determined from the
 * first three points.  Additional points do not /have/ to lie on the
 * plane defined by the first three, but if they do not, then the
 * determiniation of visibility obviously can't be 100% accurate...
 * So if you're 3 space polygons are too far from planar, consider
 * breaking them into smaller polygons.  "3 points define a plane" :-).
 *
 * For ifcc == 1, the directionality of the polygon is determined by assuming
 * the points are laid out in counter-clockwise order.
 *
 * For ifcc == 0, the directionality of the polygon is determined by assuming
 * the points are laid out in clockwise order.
 *
 * BUGS:  If one of the first two segments is of zero length, or if
 * they are colinear, the calculation of visibility has a 50/50 chance
 * of being correct.  Avoid such situations :-).  See x18c for an
 * example of this problem.  (Search for "20.1").
\*----------------------------------------------------------------------*/

MZ_DLLEXPORT
void
c_plpoly3(PLINT n, PLFLT *x, PLFLT *y, PLFLT *z, PLINT *draw, PLINT ifcc)
{
    int i;
    PLFLT vmin[3], vmax[3], zscale;
    PLFLT u1, v1, u2, v2, u3, v3;
    PLFLT c;

    if (plsc->level < 3) {
	plabort("plpoly3: Please set up window first");
	return;
    }

    if ( n < 3 ) {
	plabort("plpoly3: Must specify at least 3 points");
	return;
    }

/* Now figure out which side this is. */

    u1 = plP_wcpcx(plP_w3wcx( x[0], y[0], z[0] ));
    v1 = plP_wcpcy(plP_w3wcy( x[0], y[0], z[0] ));

    u2 = plP_wcpcx(plP_w3wcx( x[1], y[1], z[1] ));
    v2 = plP_wcpcy(plP_w3wcy( x[1], y[1], z[1] ));

    u3 = plP_wcpcx(plP_w3wcx( x[2], y[2], z[2] ));
    v3 = plP_wcpcy(plP_w3wcy( x[2], y[2], z[2] ));

    c = (u1-u2)*(v3-v2)-(v1-v2)*(u3-u2);

    if ( c *(1 - 2*ifcc) < 0. )
        return;

    /* get the bounding box in 3d */
    plP_gdom(&vmin[0], &vmax[0], &vmin[1], &vmax[1]);
    plP_grange(&zscale, &vmin[2], &vmax[2]);

    /* interate over the vertices */
    for( i=0; i < n-1; i++ ) {
      PLFLT p0[3], p1[3];
      int axis;

      /* copy the end points of the segment to allow clipping */
      p0[0] = x[i]; p0[1] = y[i]; p0[2] = z[i];
      p1[0] = x[i+1]; p1[1] = y[i+1]; p1[2] = z[i+1];

      /* check against each axis of the bounding box */
      for(axis = 0; axis < 3; axis++) {
	if(p0[axis] < vmin[axis]) { /* first out */
	  if(p1[axis] < vmin[axis]) {
	    break; /* both endpoints out so quit */
	  } else {
	    int j;
	    /* interpolate to find intersection with box */
	    PLFLT t = (vmin[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	    p0[axis] = vmin[axis];
	    for(j = 1; j<3; j++) {
	      int k = (axis+j)%3;
	      p0[k] = (1-t)*p0[k] + t*p1[k];
	    }
	  }
	} else if(p1[axis] < vmin[axis]) { /* second out */
	  int j;
	  /* interpolate to find intersection with box */
	  PLFLT t = (vmin[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	  p1[axis] = vmin[axis];
	  for(j = 1; j<3; j++) {
	    int k = (axis+j)%3;
	    p1[k] = (1-t)*p0[k] + t*p1[k];
	  }
	}
	if(p0[axis] > vmax[axis]) { /* first out */
	  if(p1[axis] > vmax[axis]) {
	    break; /* both out so quit */
	  } else {
	    int j;
	    /* interpolate to find intersection with box */
	    PLFLT t = (vmax[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	    p0[axis] = vmax[axis];
	    for(j = 1; j<3; j++) {
	      int k = (axis+j)%3;
	      p0[k] = (1-t)*p0[k] + t*p1[k];
	    }
	  }
	} else if(p1[axis] > vmax[axis]) { /* second out */
	  int j;
	  /* interpolate to find intersection with box */
	  PLFLT t = (vmax[axis] - p0[axis]) / (p1[axis] - p0[axis]);
	  p1[axis] = vmax[axis];
	  for(j = 1; j<3; j++) {
	    int k = (axis+j)%3;
	    p1[k] = (1-t)*p0[k] + t*p1[k];
	  }
	}
      }
      /* if we made it to here without "break"ing out of the loop, the 
	 remaining segment is visible */
      if( axis == 3 && draw[i] ) { /*  not clipped away */
	PLFLT u0, v0, u1, v1;
	u0 = plP_wcpcx(plP_w3wcx( p0[0], p0[1], p0[2] ));
	v0 = plP_wcpcy(plP_w3wcy( p0[0], p0[1], p0[2] ));
	u1 = plP_wcpcx(plP_w3wcx( p1[0], p1[1], p1[2] ));
	v1 = plP_wcpcy(plP_w3wcy( p1[0], p1[1], p1[2] ));
	plP_movphy(u0,v0);
	plP_draphy(u1,v1);
      }
    }
    return;
}

/*----------------------------------------------------------------------*\
 * void plstyl()
 *
 * Set up a new line style of "nms" elements, with mark and space
 * lengths given by arrays "mark" and "space".
\*----------------------------------------------------------------------*/

void
c_plstyl(PLINT nms, PLINT *mark, PLINT *space)
{
    short int i;

    if (plsc->level < 1) {
	plabort("plstyl: Please call plinit first");
	return;
    }
    if ((nms < 0) || (nms > 10)) {
	plabort("plstyl: Broken lines cannot have <0 or >10 elements");
	return;
    }
    for (i = 0; i < nms; i++) {
	if ((mark[i] < 0) || (space[i] < 0)) {
	    plabort("plstyl: Mark and space lengths must be > 0");
	    return;
	}
    }

    plsc->nms = nms;
    for (i = 0; i < nms; i++) {
	plsc->mark[i] = mark[i];
	plsc->space[i] = space[i];
    }

    plsc->curel = 0;
    plsc->pendn = 1;
    plsc->timecnt = 0;
    plsc->alarm = nms > 0 ? mark[0] : 0;
}

/*----------------------------------------------------------------------*\
 * void plP_movphy()
 *
 * Move to physical coordinates (x,y).
\*----------------------------------------------------------------------*/

void
plP_movphy(PLINT x, PLINT y)
{
    plsc->currx = x;
    plsc->curry = y;
}

/*----------------------------------------------------------------------*\
 * void plP_draphy()
 *
 * Draw to physical coordinates (x,y).
\*----------------------------------------------------------------------*/

void
plP_draphy(PLINT x, PLINT y)
{
    xline[0] = plsc->currx;
    xline[1] = x;
    yline[0] = plsc->curry;
    yline[1] = y;

    pllclp(xline, yline, 2);
}

/*----------------------------------------------------------------------*\
 * void plP_movwor()
 *
 * Move to world coordinates (x,y).
\*----------------------------------------------------------------------*/

void
plP_movwor(PLFLT x, PLFLT y)
{
    plsc->currx = plP_wcpcx(x);
    plsc->curry = plP_wcpcy(y);
}

/*----------------------------------------------------------------------*\
 * void plP_drawor()
 *
 * Draw to world coordinates (x,y).
\*----------------------------------------------------------------------*/

void
plP_drawor(PLFLT x, PLFLT y)
{
    xline[0] = plsc->currx;
    xline[1] = plP_wcpcx(x);
    yline[0] = plsc->curry;
    yline[1] = plP_wcpcy(y);

    pllclp(xline, yline, 2);
}

/*----------------------------------------------------------------------*\
 * void plP_draphy_poly()
 *
 * Draw polyline in physical coordinates.
 * Need to draw buffers in increments of (PL_MAXPOLY-1) since the
 * last point must be repeated (for solid lines).
\*----------------------------------------------------------------------*/

void
plP_draphy_poly(PLINT *x, PLINT *y, PLINT n)
{
    PLINT i, j, ib, ilim;

    for (ib = 0; ib < n; ib += PL_MAXPOLY - 1) {
	ilim = MIN(PL_MAXPOLY, n - ib);

	for (i = 0; i < ilim; i++) {
	    j = ib + i;
	    xline[i] = x[j];
	    yline[i] = y[j];
	}
	pllclp(xline, yline, ilim);
    }
}

/*----------------------------------------------------------------------*\
 * void plP_drawor_poly()
 *
 * Draw polyline in world coordinates.
 * Need to draw buffers in increments of (PL_MAXPOLY-1) since the
 * last point must be repeated (for solid lines).
\*----------------------------------------------------------------------*/

void
plP_drawor_poly(PLFLT *x, PLFLT *y, PLINT n)
{
    PLINT i, j, ib, ilim;

    for (ib = 0; ib < n; ib += PL_MAXPOLY - 1) {
	ilim = MIN(PL_MAXPOLY, n - ib);

	for (i = 0; i < ilim; i++) {
	    j = ib + i;
	    xline[i] = plP_wcpcx(x[j]);
	    yline[i] = plP_wcpcy(y[j]);
	}
	pllclp(xline, yline, ilim);
    }
}

/*----------------------------------------------------------------------*\
 * void pllclp()
 *
 * Draws a polyline within the clip limits.
 * Merely a front-end to plP_pllclp().
\*----------------------------------------------------------------------*/

static void
pllclp(PLINT *x, PLINT *y, PLINT npts)
{
    plP_pllclp(x, y, npts, plsc->clpxmi, plsc->clpxma,
	       plsc->clpymi, plsc->clpyma, genlin);
}

/*----------------------------------------------------------------------*\
 * void plP_pllclp()
 *
 * Draws a polyline within the clip limits.
\*----------------------------------------------------------------------*/

void
plP_pllclp(PLINT *x, PLINT *y, PLINT npts,
	   PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax, 
	   void (*draw) (short *, short *, PLINT))
{
    PLINT x1, x2, y1, y2;
    PLINT i, iclp = 0;
    short xclp[PL_MAXPOLY], yclp[PL_MAXPOLY];
    int drawable;

    for (i = 0; i < npts - 1; i++) {
	x1 = x[i];
	x2 = x[i + 1];
	y1 = y[i];
	y2 = y[i + 1];

	drawable = (INSIDE(x1, y1) && INSIDE(x2, y2));
	if ( ! drawable)
	    drawable = ! clipline(&x1, &y1, &x2, &y2, xmin, xmax, ymin, ymax);

	if (drawable) {

/* First point of polyline. */

	    if (iclp == 0) {
		xclp[iclp] = x1;
		yclp[iclp] = y1;
		iclp++;
		xclp[iclp] = x2;
		yclp[iclp] = y2;
	    }

/* Not first point.  Check if first point of this segment matches up to 
   previous point, and if so, add it to the current polyline buffer. */

	    else if (x1 == xclp[iclp] && y1 == yclp[iclp]) {
		iclp++;
		xclp[iclp] = x2;
		yclp[iclp] = y2;
	    }

/* Otherwise it's time to start a new polyline */

	    else {
		if (iclp + 1 >= 2)
		    (*draw)(xclp, yclp, iclp + 1);
		iclp = 0;
		xclp[iclp] = x1;
		yclp[iclp] = y1;
		iclp++;
		xclp[iclp] = x2;
		yclp[iclp] = y2;
	    }
	}
    }

/* Handle remaining polyline */

    if (iclp + 1 >= 2)
	(*draw)(xclp, yclp, iclp + 1);

    plsc->currx = x[npts-1];
    plsc->curry = y[npts-1];
}

/*----------------------------------------------------------------------*\
 * void plP_plfclp()
 *
 * Fills a polygon within the clip limits.
\*----------------------------------------------------------------------*/

void
plP_plfclp(PLINT *x, PLINT *y, PLINT npts,
	   PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax, 
	   void (*draw) (short *, short *, PLINT))
{
    PLINT x1, x2, y1, y2;
    PLINT i, iclp = -1;
    short xclp[PL_MAXPOLY], yclp[PL_MAXPOLY];
    int drawable;

    for (i = 0; i < npts - 1; i++) {
	x1 = x[i];
	x2 = x[i + 1];
	y1 = y[i];
	y2 = y[i + 1];

	drawable = (INSIDE(x1, y1) && INSIDE(x2, y2));
	if ( ! drawable)
	    drawable = ! clipline(&x1, &y1, &x2, &y2, xmin, xmax, ymin, ymax);

	if (drawable) {

/* Not first point.  If first point of this segment matches up to the
   previous point, just add it.  */

	    if (iclp >= 0 && x1 == xclp[iclp] && y1 == yclp[iclp]) {
		iclp++;
		xclp[iclp] = x2;
		yclp[iclp] = y2;
	    }

/* First point of polyline, OR . */

/* If not, we need to add both points, to connect the points in the
 * polygon along the clip boundary.  If any of the previous points were
 * outside one of the 4 corners, assume the corner was encircled and add
 * it first. 
 */
	    else {
		iclp++;
		xclp[iclp] = x1;
		yclp[iclp] = y1;

		if ((x1 == xmin && y2 == ymin) ||
		    (x2 == xmin && y1 == ymin)) {
		    iclp++;
		    xclp[iclp] = xmin;
		    yclp[iclp] = ymin;
		}
		else if ((x1 == xmax && y2 == ymin) ||
			 (x2 == xmax && y1 == ymin)) {
		    iclp++;
		    xclp[iclp] = xmax;
		    yclp[iclp] = ymin;
		}
		else if ((x1 == xmax && y2 == ymax) ||
			 (x2 == xmax && y1 == ymax)) {
		    iclp++;
		    xclp[iclp] = xmax;
		    yclp[iclp] = ymax;
		}
		else if ((x1 == xmin && y2 == ymax) ||
			 (x2 == xmin && y1 == ymax)) {
		    iclp++;
		    xclp[iclp] = xmin;
		    yclp[iclp] = ymax;
		}
	    /* Experimental code from way back.
	       Polygon clipping is HARD.
		{
		    int j;
		    for (j = 0; j < i; j++) {
			if (x[j] < xmin && y[j] < ymin) {
			    break;
			}
			else if (x[j] < xmin && y[j] > ymax) {
			    iclp++;
			    xclp[iclp] = xmin;
			    yclp[iclp] = ymax;
			    break;
			}
			else if (x[j] > xmax && y[j] < ymin) {
			    iclp++;
			    xclp[iclp] = xmax;
			    yclp[iclp] = ymin;
			    break;
			}
			else if (x[j] > xmax && y[j] > ymax) {
			    iclp++;
			    xclp[iclp] = xmax;
			    yclp[iclp] = ymax;
			    break;
			}
		    }
		}
	    */
		iclp++;
		xclp[iclp] = x2;
		yclp[iclp] = y2;
	    }
	}
    }

/* Draw the sucker */

    if (iclp + 1 >= 2) {
	if ((xclp[0] == xmin && yclp[iclp] == ymin) ||
	    (xclp[iclp] == xmin && yclp[0] == ymin)) {
	    iclp++;
	    xclp[iclp] = xmin;
	    yclp[iclp] = ymin;
	}
	else if ((xclp[0] == xmax && yclp[iclp] == ymin) ||
		 (xclp[iclp] == xmax && yclp[0] == ymin)) {
	    iclp++;
	    xclp[iclp] = xmax;
	    yclp[iclp] = ymin;
	}
	else if ((xclp[0] == xmax && yclp[iclp] == ymax) ||
		 (xclp[iclp] == xmax && yclp[0] == ymax)) {
	    iclp++;
	    xclp[iclp] = xmax;
	    yclp[iclp] = ymax;
	}
	else if ((xclp[0] == xmin && yclp[iclp] == ymax) ||
		 (xclp[iclp] == xmin && yclp[0] == ymax)) {
	    iclp++;
	    xclp[iclp] = xmin;
	    yclp[iclp] = ymax;
	}
    }
    if (iclp + 1 >= 3) {
      if(draw)
	(*draw)(xclp, yclp, iclp + 1);
    }
}

/*----------------------------------------------------------------------*\
 * int clipline()
 *
 * Get clipped endpoints
\*----------------------------------------------------------------------*/

static int
clipline(PLINT *p_x1, PLINT *p_y1, PLINT *p_x2, PLINT *p_y2,
	 PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax)
{
    PLINT t, dx, dy, flipx, flipy;
    double dydx = 0, dxdy = 0;

/* If both points are outside clip region with no hope of intersection,
   return with an error */

    if ((*p_x1 <= xmin && *p_x2 <= xmin) ||
	(*p_x1 >= xmax && *p_x2 >= xmax) ||
	(*p_y1 <= ymin && *p_y2 <= ymin) ||
	(*p_y1 >= ymax && *p_y2 >= ymax))
	return 1;

    flipx = 0;
    flipy = 0;

    if (*p_x2 < *p_x1) {
	*p_x1 = 2 * xmin - *p_x1;
	*p_x2 = 2 * xmin - *p_x2;
	xmax = 2 * xmin - xmax;
	t = xmax;
	xmax = xmin;
	xmin = t;
	flipx = 1;
    }

    if (*p_y2 < *p_y1) {
	*p_y1 = 2 * ymin - *p_y1;
	*p_y2 = 2 * ymin - *p_y2;
	ymax = 2 * ymin - ymax;
	t = ymax;
	ymax = ymin;
	ymin = t;
	flipy = 1;
    }

    dx = *p_x2 - *p_x1;
    dy = *p_y2 - *p_y1;

    if (dx != 0 && dy != 0) {
	dydx = (double) dy / (double) dx;
	dxdy = 1./ dydx;
    }

    if (*p_x1 < xmin) {
	if (dx != 0 && dy != 0)
	    *p_y1 = *p_y1 + ROUND((xmin - *p_x1) * dydx);
	*p_x1 = xmin;
    }

    if (*p_y1 < ymin) {
	if (dx != 0 && dy != 0)
	    *p_x1 = *p_x1 + ROUND((ymin - *p_y1) * dxdy);
	*p_y1 = ymin;
    }

    if (*p_x1 >= xmax || *p_y1 >= ymax)
	return 1;

    if (*p_y2 > ymax) {
	if (dx != 0 && dy != 0)
	    *p_x2 = *p_x2 - ROUND((*p_y2 - ymax) * dxdy);
	*p_y2 = ymax;
    }

    if (*p_x2 > xmax) {
	if (dx != 0 && dy != 0)
	    *p_y2 = *p_y2 - ROUND((*p_x2 - xmax) * dydx);
	*p_x2 = xmax;
    }

    if (flipx) {
	*p_x1 = 2 * xmax - *p_x1;
	*p_x2 = 2 * xmax - *p_x2;
    }

    if (flipy) {
	*p_y1 = 2 * ymax - *p_y1;
	*p_y2 = 2 * ymax - *p_y2;
    }

    return 0;
}

/*----------------------------------------------------------------------*\
 * void genlin()
 *
 * General line-drawing routine.  Takes line styles into account.
 * If only 2 points are in the polyline, it is more efficient to use
 * plP_line() rather than plP_polyline().
\*----------------------------------------------------------------------*/

static void
genlin(short *x, short *y, PLINT npts)
{
/* Check for solid line */

    if (plsc->nms == 0) {
	if (npts== 2)
	    plP_line(x, y);
	else
	    plP_polyline(x, y, npts);
    }

/* Right now dashed lines don't use polyline capability -- this
   should be improved */

    else {

	PLINT i;

        /* Call escape sequence to draw dashed lines, only for drivers
	   that have this capability */
        if (plsc->dev_dash) {
	    plsc->dev_npts = npts;
	    plsc->dev_x = x;
	    plsc->dev_y = y;
	    plP_esc(PLESC_DASH, NULL);
            return;
        }

	for (i = 0; i < npts - 1; i++) {
	    grdashline(x+i, y+i);
	}
    }
}

/*----------------------------------------------------------------------*\
 * void grdashline()
 *
 * Draws a dashed line to the specified point from the previous one.
\*----------------------------------------------------------------------*/

static void
grdashline(short *x, short *y)
{
    PLINT nx, ny, nxp, nyp, incr, temp;
    PLINT modulo, dx, dy, i, xtmp, ytmp;
    PLINT tstep, pix_distance, j;
    int loop_x;
    short xl[2], yl[2];
    double nxstep, nystep;

/* Check if pattern needs to be restarted */

    if (x[0] != lastx || y[0] != lasty) {
	plsc->curel = 0;
	plsc->pendn = 1;
	plsc->timecnt = 0;
	plsc->alarm = plsc->mark[0];
    }

    lastx = xtmp = x[0];
    lasty = ytmp = y[0];

    if (x[0] == x[1] && y[0] == y[1])
	return;

    nx = x[1] - x[0];
    dx = (nx > 0) ? 1 : -1;
    nxp = ABS(nx);

    ny = y[1] - y[0];
    dy = (ny > 0) ? 1 : -1;
    nyp = ABS(ny);

    if (nyp > nxp) {
	modulo = nyp;
	incr = nxp;
	loop_x = 0;
    }
    else {
	modulo = nxp;
	incr = nyp;
	loop_x = 1;
    }

    temp = modulo / 2;

/* Compute the timer step */

    nxstep = nxp * plsc->umx;
    nystep = nyp * plsc->umy;
    tstep = sqrt( nxstep * nxstep + nystep * nystep ) / modulo;
    if (tstep < 1) tstep = 1;

    /* tstep is distance per pixel moved */

    i = 0;
    while (i < modulo) {
        pix_distance = (plsc->alarm - plsc->timecnt + tstep - 1) / tstep;
	i += pix_distance;
	if (i > modulo)
	    pix_distance -= (i - modulo);
	plsc->timecnt += pix_distance * tstep;

	temp += pix_distance * incr;
	j = temp / modulo;
	temp = temp % modulo;

	if (loop_x) {
	    xtmp += pix_distance * dx;
	    ytmp += j * dy;
	}
	else {
	    xtmp += j * dx;
	    ytmp += pix_distance * dy;
	}
	if (plsc->pendn != 0) {
	    xl[0] = lastx;
	    yl[0] = lasty;
	    xl[1] = xtmp;
	    yl[1] = ytmp;
	    plP_line(xl, yl);
	}

/* Update line style variables when alarm goes off */

	while (plsc->timecnt >= plsc->alarm) {
	    if (plsc->pendn != 0) {
		plsc->pendn = 0;
		plsc->timecnt -= plsc->alarm;
		plsc->alarm = plsc->space[plsc->curel];
	    }
	    else {
		plsc->pendn = 1;
		plsc->timecnt -= plsc->alarm;
		plsc->curel++;
		if (plsc->curel >= plsc->nms)
		    plsc->curel = 0;
		plsc->alarm = plsc->mark[plsc->curel];
	    }
	}
	lastx = xtmp;
	lasty = ytmp;
    }
}
