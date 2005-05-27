/******************************************************************************
 *
 * File:           csa.h
 *
 * Created:        16/10/2002
 *
 * Author:         Pavel Sakov
 *                 CSIRO Marine Research
 *
 * Purpose:        An "internal" header for csa library (2D data approximation
 *                 with bivariate cubic spline)
 *
 * Revisions:      None
 *
 *****************************************************************************/

#if !defined(_CSA_INTERNAL_H)
#define _CSA_INTERNAL_H

#include "csa.h"

struct square;
typedef struct square square;

typedef struct {
    square* parent;
    int index;                  /* index within parent square; 0 <= index <= 
                                 * 3 */
    point vertices[3];
    point middle;               /* barycenter */
    double h;                   /* parent square edge length */
    double r;                   /* data visibility radius */

    /*
     * points used -- in primary triangles only 
     */
    int nallocated;
    int npoints;
    point** points;

    int primary;                /* flag -- whether calculate spline
                                 * coefficients directly (by least squares
                                 * method) (primary = 1) or indirectly (from 
                                 * * C1 smoothness conditions) (primary = 0) 
                                 */
    int hascoeffs;              /* flag -- whether there are no NaNs among
                                 * the spline coefficients */
    int order;                  /* spline order -- for primary triangles
                                 * only */
} triangle;

struct square {
    csa* parent;
    int i, j;                   /* indices */

    int nallocated;
    int npoints;
    point** points;

    int primary;                /* flag -- whether this square contains a
                                 * primary triangle */

    triangle* triangles[4];

    double coeffs[25];
};

struct csa {
    int verbose;                /* flag */

    double xmin;
    double xmax;
    double ymin;
    double ymax;

    int nallocated;
    int npoints;
    point** points;

    /*
     * squarization 
     */
    int ni;
    int nj;
    double h;
    square*** squares;          /* square* [j][i] */

    int npt;                    /* Number of Primary Triangles */
    triangle** pt;              /* Primary Triangles -- triangle* [npt] */

    /*
     * algorithm parameters 
     */
    int nmin;
    int nmax;
    double k;
};

#endif
