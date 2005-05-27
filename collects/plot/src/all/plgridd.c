
#include "plplotP.h"

#ifdef WITH_CSA
#include "csa.h"
#endif
#include "nan.h" /* this is handy */

#ifdef HAVE_QHULL
#include "../lib/nn/nn.h"
#include <qhull/qhull_a.h>
#endif

#if !defined(HAVE_ISNAN)
#define isnan(x) ((x) != (x))
#endif

/* forward declarations */
static void
grid_nnaidw (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	     PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg);

static void
grid_nnli (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	   PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg,
	   PLFLT threshold);

static void
grid_nnidw (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	    PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg,
	    int knn_order);

#ifdef WITH_CSA
static void
grid_csa (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	  PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg);
#endif

#ifdef HAVE_QHULL
static void
grid_nni (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	  PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg,
	  PLFLT wmin);

static void
grid_dtli (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	   PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg);
#endif

static void
dist1(PLFLT gx, PLFLT gy, PLFLT *x, PLFLT *y, int npts, int knn_order);
static void
dist2(PLFLT gx, PLFLT gy, PLFLT *x, PLFLT *y, int npts);

#define KNN_MAX_ORDER 100

typedef struct pt{
  PLFLT dist;
  int   item;
}PT;

static PT items[KNN_MAX_ORDER];

/*----------------------------------------------------------------------*\
 *
 * plgriddata(): grids data from irregularly sampled data.
 *
 *    Real world data is frequently irregularly sampled, but most 3D plots
 *    require regularly gridded data. This function does exactly this
 *    using several methods:
 *    Irregularly sampled data x[npts], y[npts], z[npts] is gridded into
 *    zg[nptsx, nptsy] according to methode 'type' and grid information
 *    xg[nptsx], yg[nptsy].
 *
 *    'type' can be:
 *
 *       GRID_CSA:    Bivariate Cubic Spline approximation (1)
 *       GRID_NNIDW:  Nearest Neighbors Inverse Distance Weighted
 *       GRID_NNLI:   Nearest Neighbors Linear Interpolation
 *       GRID_NNAIDW: Nearest Neighbors Around Inverse Distance Weighted
 *       GRID_DTLI:   Delaunay Triangulation Linear Interpolation (2)
 *       GRID_NNI:    Natural Neighbors interpolation (2)
 * 
 * (1): Copyright 2000-2002 CSIRO Marine Research, Pavel Sakov's csa library
 * (2): Copyright 2002 CSIRO Marine Research, Pavel Sakov's nn library
 *
\*----------------------------------------------------------------------*/

void
c_plgriddata(PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	   PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy,
	   PLFLT **zg, int type, PLFLT data)
{
  int i, j;

  if(npts < 1 || nptsx < 1 || nptsy < 1) {
    plabort("plgriddata: Bad array dimensions"); 
    return;
  }

  /* Check that points in xg and in yg are strictly increasing */

  for (i = 0; i < nptsx - 1; i++) {
    if (xg[i] >= xg[i + 1]) {
      plabort("plgriddata: xg array must be strictly increasing");
      return;
    }
  }
  for (i = 0; i < nptsy - 1; i++) {
    if (yg[i] >= yg[i + 1]) {
      plabort("plgriddata: yg array must be strictly increasing");
      return;
    }
  }

  /* clear array to return */
  for(i=0; i<nptsx; i++)
    for(j=0; j<nptsy; j++)
      zg[i][j] = 0.; /* NaN signals a not processed grid point */

  switch (type) {
    
  case (GRID_CSA):     /*  Bivariate Cubic Spline Approximation */
#ifdef WITH_CSA
    grid_csa(x, y, z, npts, xg, nptsx, yg, nptsy, zg);
#else
    plabort("plgriddata(): PLplot was configured to not use GRID_CSA.");
#endif
    break;

  case (GRID_NNIDW): /* Nearest Neighbors Inverse Distance Weighted */
    grid_nnidw(x, y, z, npts, xg, nptsx, yg, nptsy, zg, (int) data);
    break;

  case (GRID_NNLI): /* Nearest Neighbors Linear Interpolation */
    grid_nnli(x, y, z, npts, xg, nptsx, yg, nptsy, zg, data);
    break;

  case (GRID_NNAIDW): /* Nearest Neighbors "Around" Inverse Distance Weighted */
    grid_nnaidw(x, y, z, npts, xg, nptsx, yg, nptsy, zg);
    break;

  case (GRID_DTLI): /* Delaunay Triangulation Linear Interpolation */
#ifdef HAVE_QHULL
    grid_dtli(x, y, z, npts, xg, nptsx, yg, nptsy, zg);
#else
    plabort("plgriddata(): you must have Qhull to use GRID_DTLI.");
#endif
    break;

  case (GRID_NNI): /* Natural Neighbors */
#ifdef HAVE_QHULL
    grid_nni(x, y, z, npts, xg, nptsx, yg, nptsy, zg, data);
#else
    plabort("plgriddata(): you must have Qhull to use GRID_NNI.");
#endif
    break;

  default:
    plabort("plgriddata: unknown algorithm type"); 
  }
}

#ifdef WITH_CSA
/* 
 * Bivariate Cubic Spline Approximation using Pavel Sakov's csa package
 *
 * NaNs are returned where no interpolation can be done.
 */

static void
grid_csa (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	  PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg)
{
  PLFLT *xt, *yt, *zt;
  point *pin, *pgrid, *pt;
  csa* a = NULL;
  int i, j, nptsg;

  pin = (point *) malloc(npts * sizeof(point));

  xt = x; yt = y; zt = z; pt = pin;
  for(i=0; i<npts; i++) {
    pt->x = (double) *xt++;
    pt->y = (double) *yt++;
    pt->z = (double) *zt++;
    pt++;
  }

  nptsg = nptsx * nptsy;
  pgrid =  (point *) malloc(nptsg * sizeof(point));

  yt = yg; pt = pgrid;
  for(j=0; j<nptsy; j++) {
    xt = xg;
    for(i=0; i<nptsx; i++) {
      pt->x = (double) *xt++;
      pt->y = (double) *yt;
      pt++;
    }
    yt++;
  }

  a = csa_create();
  csa_addpoints(a, npts, pin);
  csa_calculatespline(a);
  csa_approximate_points(a, nptsg, pgrid);

  for(i=0; i<nptsx; i++) {
    for(j=0; j<nptsy; j++) {
      pt = &pgrid[j*nptsx + i];
      zg[i][j] = (PLFLT) pt->z;
    }
  }

  csa_destroy(a);
  free(pin);
  free(pgrid); 
}
#endif /* WITH_CSA */

/* Nearest Neighbors Inverse Distance Weighted, brute force approach.
 *
 * The z value at the grid position will be the weighted average
 * of the z values of the KNN points found. The weigth is the
 * inverse squared distance between the grid point and each
 * neighbor.
 */

static void
grid_nnidw (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	    PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg,
	    int knn_order)
{
  int i, j, k;
  PLFLT wi, nt;

  if (knn_order > KNN_MAX_ORDER) {
    plabort("plgriddata(): GRID_NNIDW: knn_order too big"); /* make sure it is smaller that KNN_MAX_ORDER */
    return;
  }

  if (knn_order == 0) {
    plwarn("plgriddata(): GRID_NNIDW: knn_order must be specified with 'data' arg. Using 15");
    knn_order = 15;;
  }

  for (i=0; i<nptsx; i++) {
    for (j=0; j<nptsy; j++) {
      dist1(xg[i], yg[j], x, y, npts, knn_order);

#ifdef GMS /* alternative weight coeficients. I Don't like the results */
      /* find the maximum distance */
      md = items[0].dist;
      for (k=1; k<knn_order; k++)
	if (items[k].dist > md)
	  md = items[k].dist;
#endif
      zg[i][j] = 0.;
      nt = 0.;

      for (k=0; k<knn_order; k++) {
	if(items[k].item == -1) /* not enough neighbors found ?! */
	  continue;
#ifdef GMS
	wi = (md - items[k].dist)/(md * items[k].dist);
	wi = wi*wi;
#else
	wi = 1./(items[k].dist*items[k].dist);
#endif
	zg[i][j] += wi * z[items[k].item];
	nt += wi;
      }
      if (nt != 0.)
	zg[i][j] /= nt;
      else
	zg[i][j] = NaN;
    }
  }
}

/* Nearest Neighbors Linear Interpolation
 *
 * The z value at the grid position will be interpolated from the
 * plane passing through the 3 nearest neighbors.
 */

static void
grid_nnli (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	   PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg,
	   PLFLT threshold)
{
  PLFLT xx[4], yy[4], zz[4], t, A, B, C, D, d1, d2, d3, max_thick;
  int i, j, ii, excl, cnt, excl_item;
      
  if (threshold == 0.) {
    plwarn("plgriddata(): GRID_NNLI: threshold must be specified with 'data' arg. Using 1.001");
    threshold = 1.001;
  } else if ( threshold > 2. ||  threshold < 1.) {
    plabort("plgriddata(): GRID_NNLI: 1. < threshold < 2.");
    return;
  }

  for (i=0; i<nptsx; i++) {
    for (j=0; j<nptsy; j++) {
      dist1(xg[i], yg[j], x, y, npts, 3);

      /* see if the triangle is a thin one */
      for (ii=0; ii<3; ii++) {
	xx[ii] = x[items[ii].item];
	yy[ii] = y[items[ii].item];
	zz[ii] = z[items[ii].item];
      }

      d1 = sqrt((xx[1]-xx[0])*(xx[1]-xx[0]) + (yy[1]-yy[0])*(yy[1]-yy[0]));
      d2 = sqrt((xx[2]-xx[1])*(xx[2]-xx[1]) + (yy[2]-yy[1])*(yy[2]-yy[1]));
      d3 = sqrt((xx[0]-xx[2])*(xx[0]-xx[2]) + (yy[0]-yy[2])*(yy[0]-yy[2]));

      if (d1 == 0. || d2 == 0. || d3 == 0.) { /* coincident points */
	zg[i][j] = NaN; 
	continue;
      }

      /* make d1 < d2 */
      if (d1 > d2) {
	t = d1; d1 = d2; d2 = t;
      }

      /* and d2 < d3 */
      if (d2 > d3) {
	t = d2; d2 = d3; d3 = t;
      }

      if ((d1+d2)/d3 < threshold) { /* thin triangle! */
	zg[i][j] = NaN; /* deal with it latter */
      } else {  /* calculate the plane passing through the three points */
	 	 
	A = yy[0]*(zz[1]-zz[2]) + yy[1]*(zz[2]-zz[0]) + yy[2]*(zz[0]-zz[1]);
	B = zz[0]*(xx[1]-xx[2]) + zz[1]*(xx[2]-xx[0]) + zz[2]*(xx[0]-xx[1]);
	C = xx[0]*(yy[1]-yy[2]) + xx[1]*(yy[2]-yy[0]) + xx[2]*(yy[0]-yy[1]);
	D = - A*xx[0] - B*yy[0] - C*zz[0];

	/* and interpolate (or extrapolate...) */
	zg[i][j] = - xg[i]*A/C - yg[j]*B/C - D/C;
      }
    }
  }

  /* now deal with NaNs resulting from thin triangles. The idea is
   * to use the 4 KNN points and exclude one at a time, creating
   * four triangles, evaluating their thickness and choosing the
   * most thick as the final one from where the interpolating
   * plane will be build.  Now that I'm talking of interpolating,
   * one should really check that the target point is interior to
   * the candidate triangle... otherwise one is extrapolating
   */

  { 
    for (i=0; i<nptsx; i++) {
      for (j=0; j<nptsy; j++) {

	if (isnan(zg[i][j])) {
	  dist1(xg[i], yg[j], x, y, npts, 4);

	  /* sort by distances. Not really needed! 
	     for (ii=3; ii>0; ii--) {
	     for (jj=0; jj<ii; jj++) {
	     if (items[jj].dist > items[jj+1].dist) {
	     t = items[jj].dist;
	     items[jj].dist = items[jj+1].dist;
	     items[jj+1].dist = t;
	     }
	     }
	     } 	     
	  */

	  max_thick = 0.; excl_item = -1;
	  for (excl=0; excl<4; excl++) { /* the excluded point */
	      
	    cnt = 0;
	    for (ii=0; ii<4; ii++) {
	      if (ii != excl) {
		xx[cnt] = x[items[ii].item];
		yy[cnt] = y[items[ii].item];
		cnt++;
	      }
	    }

	    d1 = sqrt((xx[1]-xx[0])*(xx[1]-xx[0]) + (yy[1]-yy[0])*(yy[1]-yy[0]));
	    d2 = sqrt((xx[2]-xx[1])*(xx[2]-xx[1]) + (yy[2]-yy[1])*(yy[2]-yy[1]));
	    d3 = sqrt((xx[0]-xx[2])*(xx[0]-xx[2]) + (yy[0]-yy[2])*(yy[0]-yy[2]));
	    if (d1 == 0. || d2 == 0. || d3 == 0.) /* coincident points */	 		  continue;

	    /* make d1 < d2 */
	    if (d1 > d2) {
	      t = d1; d1 = d2; d2 = t;
	    }
	    /* and d2 < d3 */
	    if (d2 > d3) {
	      t = d2; d2 = d3; d3 = t;
	    }

	    t = (d1+d2)/d3;
	    if ( t > max_thick) {
	      max_thick = t;
	      excl_item = excl;
	    }
	  }

	  if (excl_item == -1) /* all points are coincident? */
	    continue;

	  /* one has the thicker triangle constructed from the 4 KNN */
	  cnt = 0;
	  for (ii=0; ii<4; ii++) {
	    if (ii != excl_item) {
	      xx[cnt] = x[items[ii].item];
	      yy[cnt] = y[items[ii].item];
	      zz[cnt] = z[items[ii].item];
	      cnt++;
	    }
	  }
	 
	  A = yy[0]*(zz[1]-zz[2]) + yy[1]*(zz[2]-zz[0]) + yy[2]*(zz[0]-zz[1]);
	  B = zz[0]*(xx[1]-xx[2]) + zz[1]*(xx[2]-xx[0]) + zz[2]*(xx[0]-xx[1]);
	  C = xx[0]*(yy[1]-yy[2]) + xx[1]*(yy[2]-yy[0]) + xx[2]*(yy[0]-yy[1]);
	  D = - A*xx[0] - B*yy[0] - C*zz[0];

	  /* and interpolate (or extrapolate...) */
	  zg[i][j] = - xg[i]*A/C - yg[j]*B/C - D/C;

	}
      }
    }
  }
}

/* 
 * Nearest Neighbors "Around" Inverse Distance Weighted, brute force approach.
 * 
 * This uses the 1-KNN in each quadrant around the grid point, then
 * Inverse Distance Weighted is used as in GRID_NNIDW.
 */

static void
grid_nnaidw (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	     PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg)
{
  PLFLT d, nt;
  int i, j, k;

  for (i=0; i<nptsx; i++) {
    for (j=0; j<nptsy; j++) {
      dist2(xg[i], yg[j], x, y, npts);
      zg[i][j] = 0.;
      nt = 0.;
      for (k=0; k<4; k++) {
	if (items[k].item != -1) { /* was found */
	  d = 1./(items[k].dist * items[k].dist); /* 1/square distance */
	  zg[i][j] += d * z[items[k].item];
	  nt += d;
	}
      }
      if (nt == 0.) /* no points found?! */
	zg[i][j] = NaN;
      else
	zg[i][j] /= nt;
    }
  }
}

#ifdef HAVE_QHULL
/*
 * Delaunay Triangulation Linear Interpolation using Pavel Sakov's nn package
 *
 * The Delaunay Triangulation on the data points is build and for
 * each grid point the triangle where it is enclosed found and a
 * linear interpolation performed.
 *
 * Points exterior to the convex hull of the data points cannot
 * be interpolated and are set to NaN.
 */

static void
grid_dtli(PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	  PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg)
{
  point *pin, *pgrid, *pt;
  PLFLT *xt, *yt, *zt;
  int i, j, nptsg;

  if (sizeof(realT) != sizeof(double)) {
    plabort("plgridata: QHull was compiled for floats instead of doubles");
    return;
  }

  pin = (point *) malloc(npts * sizeof(point));

  xt = x; yt = y; zt = z; pt = pin;
  for(i=0; i<npts; i++) {
    pt->x = (double) *xt++;
    pt->y = (double) *yt++;
    pt->z = (double) *zt++;
    pt++;
  }

  nptsg = nptsx * nptsy;
  pgrid =  (point *) malloc(nptsg * sizeof(point));

  yt = yg; pt = pgrid;
  for(j=0; j<nptsy; j++) {
    xt = xg;
    for(i=0; i<nptsx; i++) {
      pt->x = (double) *xt++;
      pt->y = (double) *yt;
      pt++;
    }
    yt++;
  }

  lpi_interpolate_points(npts, pin, nptsg, pgrid);
  for(i=0; i<nptsx; i++) {
    for(j=0; j<nptsy; j++) {
      pt = &pgrid[j*nptsx + i];
      zg[i][j] = (PLFLT) pt->z;
    }
  }

  free(pin);
  free(pgrid); 
}

/*
 * Natural Neighbors using Pavel Sakov's nn package
 *
 * Points exterior to the convex hull of the data points cannot
 * be interpolated and are set to NaN.
 */

static void
grid_nni (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	  PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg,
	  PLFLT wmin)
{
  PLFLT *xt, *yt, *zt;
  point *pin, *pgrid, *pt;
  int i, j, nptsg;
  nn_algorithm = NON_SIBSONIAN;

  if (sizeof(realT) != sizeof(double)) {
    plabort("plgridata: QHull was compiled for floats instead of doubles");
    return;
  }

  if (wmin == 0.) {/* only accept weights greater than wmin */
   plwarn("plgriddata(): GRID_NNI: wmin must be specified with 'data' arg. Using -PLFLT_MAX"); 
    wmin =  -PLFLT_MAX;
  }

  pin = (point *) malloc(npts * sizeof(point));

  xt = x; yt = y; zt = z; pt = pin;
  for(i=0; i<npts; i++) {
    pt->x = (double) *xt++;
    pt->y = (double) *yt++;
    pt->z = (double) *zt++;
    pt++;
  }

  nptsg = nptsx * nptsy;
  pgrid =  (point *) malloc(nptsg * sizeof(point));

  yt = yg; pt = pgrid;
  for(j=0; j<nptsy; j++) {
    xt = xg;
    for(i=0; i<nptsx; i++) {
      pt->x = (double) *xt++;
      pt->y = (double) *yt;
      pt++;
    }
    yt++;
  }

  nnpi_interpolate_points(npts, pin, wmin, nptsg, pgrid);
  for(i=0; i<nptsx; i++) {
    for(j=0; j<nptsy; j++) {
      pt = &pgrid[j*nptsx + i];
      zg[i][j] = (PLFLT) pt->z;
    }
  }

  free(pin);
  free(pgrid); 
}
#endif /* HAVE_QHULL*/

/* 
 * this function just calculates the K Nearest Neighbors of grid point
 * [gx, gy].
 */

static void
dist1(PLFLT gx, PLFLT gy, PLFLT *x, PLFLT *y, int npts, int knn_order)
{

  PLFLT d, max_dist;
  int   max_slot, i, j;

  max_dist = PLFLT_MAX;
  max_slot = 0;

  for (i=0; i<knn_order; i++) {
    items[i].dist = PLFLT_MAX;
    items[i].item = -1;
  }

  for (i=0; i<npts; i++) {
    d = ((gx - x[i])*(gx - x[i]) + (gy - y[i])*(gy - y[i])); /* save sqrt() time */
    
    if (d < max_dist) {
      /* found an item with a distance smaller than the
       * maximum distance found so far. Replace.
       */

      items[max_slot].dist = d;
      items[max_slot].item = i;

      /* find new maximum distance */
      max_dist = items[0].dist;
      max_slot = 0;
      for (j=1; j<knn_order; j++) {
	if (items[j].dist > max_dist) {
	  max_dist = items[j].dist;
	  max_slot = j;
	}
      }
    }
  }
  for (j=0; j<knn_order; j++)
    items[j].dist = sqrt(items[j].dist); /* now calculate the distance */
}

/*
 * This function searchs the 1-nearest neighbor in each quadrant around
 * the grid point.
 */

static void
dist2(PLFLT gx, PLFLT gy, PLFLT *x, PLFLT *y, int npts)
{

  PLFLT d;
  int   i, quad;

  for (i=0; i<4; i++) {
    items[i].dist = PLFLT_MAX;
    items[i].item = -1;
  }

  for (i=0; i<npts; i++) {
    d = ((gx - x[i])*(gx - x[i]) + (gy - y[i])*(gy - y[i])); /* save sqrt() time */

    /* trick to quickly compute a quadrant. The determined quadrants will be
     * miss-assigned, i.e., 1->2, 2->0, 3->1, 4->3, but that is not important,
     * speed is. */

    quad = 2*(x[i] > gx) +  (y[i] < gy);

    /* try to use the octants around the grid point, as it will give smoother
     * (and slower) results.
     * Hint: use the quadrant info plus x[i]/y[i] to determine the octant */

    if (d < items[quad].dist) {
      items[quad].dist = d;
      items[quad].item = i;
    }
  }

  for (i=0; i<4; i++)
    if (items[i].item != -1)
      items[i].dist = sqrt(items[i].dist); /* now calculate the distance */
}

#ifdef NONN /* another DTLI, based only on QHULL, not nn */ 
static void
grid_adtli (PLFLT *x, PLFLT *y, PLFLT *z, int npts,
	   PLFLT *xg, int nptsx, PLFLT *yg,  int nptsy, PLFLT **zg)
{
  coordT *points;           /* array of coordinates for each point */
  boolT ismalloc = False;   /* True if qhull should free points */
  char flags[250];          /* option flags for qhull */
  facetT *facet;	    /* set by FORALLfacets */
  vertexT *vertex, **vertexp;
  facetT *neighbor,**neighborp;
  int curlong, totlong;	    /* memory remaining after qh_memfreeshort */
  FILE *outfile = NULL;
  FILE *errfile = stderr;   /* error messages from qhull code */

  int exitcode;
  int i, j, k, l;
  int dim = 2;
  PLFLT xt[3], yt[3], zt[3];
  PLFLT A, B, C, D;
  coordT point[3];
  boolT isoutside;
  realT bestdist;
  int totpart=0;
  int numfacets, numsimplicial, numridges;
  int totneighbors, numcoplanars, numtricoplanars;	 
      
  plwarn("plgriddata: GRID_DTLI, If you have QHull knowledge, FIXME.");

  /* Could pass extra args to qhull through the 'data' argument of
     plgriddata() */
  sprintf(flags, "qhull d Qbb Qt"); 
  points = (coordT *) malloc(npts * (dim+1) * sizeof(coordT));

  for (i=0; i<npts; i++) {
    points[i*dim] = x[i];
    points[i*dim+1] = y[i];
  }

#if 1 /* easy way */
  exitcode = qh_new_qhull (dim, npts, points, ismalloc,
			   flags, outfile, errfile); 
#else
  qh_init_A (stdin, stdout, stderr, 0, NULL);
  exitcode = setjmp (qh errexit);
  if (!exitcode) {
    qh_initflags (flags);
    qh PROJECTdelaunay = True;
    qh_init_B (points, npts, dim, ismalloc);
    qh_qhull();
  }
#endif
  if (!exitcode) {                  /* if no error */

#if 0 /* print the triangles vertices */
    printf("Triangles\n");
    FORALLfacets {
      if (!facet->upperdelaunay) {
	FOREACHvertex_(facet->vertices)
	  printf (" %d", qh_pointid (vertex->point)); /* vertices index */
	printf ("\n");
      }
    }
#endif

#if 0 /* print each triangle neighbors */
    printf("Neigbors\n");

    qh_findgood_all (qh facet_list);
    qh_countfacets (qh facet_list, NULL, !qh_ALL, &numfacets, &numsimplicial, 
		    &totneighbors, &numridges, &numcoplanars, &numtricoplanars);

    FORALLfacets {
      if (!facet->upperdelaunay) {
	FOREACHneighbor_(facet) 
	  printf (" %d", neighbor->visitid ? neighbor->visitid - 1: - neighbor->id);
	printf ("\n");
      }
    }
#endif

    /* Without the setjmp(), Qhull will exit() after reporting an error */
    exitcode = setjmp (qh errexit);  
    if (!exitcode) {
      qh NOerrexit= False;
      for(i=0; i<nptsx; i++)
	for(j=0; j<nptsy; j++){
	  l = 0;
	  point[0] = xg[i];
	  point[1] = yg[j];
	  qh_setdelaunay (3, 1, point);


	  /* several ways to find the triangle given a point follow.
	     None but brute force works */
#if 0
	  facet = qh_findbestfacet (point, qh_ALL, &bestdist, &isoutside);
#endif

#if 0
	  facet = qh_findbest (point, qh facet_list, qh_ALL,
			       !qh_ISnewfacets, /*qh_ALL*/ qh_NOupper,
			       &bestdist, &isoutside, &totpart);
#endif
	
#if 0
	  vertex = qh_nearvertex (facet, point, &bestdist);
#endif

	  /* Until someone implements a working qh_findbestfacet(),
	   * do an exautive search!
	   *
	   * As far as I understand it, qh_findbestfacet() and
	   * qh_findbest() fails when 'point' does not belongs to
	   * the convex hull, i.e., when the search becomes blocked
	   * when a facet is upperdelaunay (although the error
	   * message says that the facet may be upperdelaynay or
	   * flipped, I never found a flipped one).
	   *
	   * Another possibility is to implement the 'walking
	   * triangle algorithm */
		 
	  facet = qh_findfacet_all( point, &bestdist, &isoutside, &totpart );

	  if (facet->upperdelaunay)
	    zg[i][j] = NaN;
	  else {	      
	    FOREACHvertex_(facet->vertices) {
	      k = qh_pointid(vertex->point);
	      xt[l] = x[k];
	      yt[l] = y[k];
	      zt[l] = z[k];
	      l++;
	    }

	    /* calculate the plane passing through the three points */

	    A = yt[0]*(zt[1]-zt[2]) + yt[1]*(zt[2]-zt[0]) + yt[2]*(zt[0]-zt[1]);
	    B = zt[0]*(xt[1]-xt[2]) + zt[1]*(xt[2]-xt[0]) + zt[2]*(xt[0]-xt[1]);
	    C = xt[0]*(yt[1]-yt[2]) + xt[1]*(yt[2]-yt[0]) + xt[2]*(yt[0]-yt[1]);
	    D = - A*xt[0] - B*yt[0] - C*zt[0];

	    /* and interpolate */
	    zg[i][j] = - xg[i]*A/C - yg[j]*B/C - D/C;

	  } 
	}
    }
    qh NOerrexit= True;
  }

  free(points);
  qh_freeqhull(!qh_ALL);                 /* free long memory */
  qh_memfreeshort (&curlong, &totlong);  /* free short memory and memory allocator */
  if (curlong || totlong) 
    fprintf (errfile,
	     "qhull: did not free %d bytes of long memory (%d pieces)\n",
	     totlong, curlong);
}
#endif /* NONN */
