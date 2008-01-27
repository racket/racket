#ifndef lint
static char *RCSid = "$Id: matrix.c,v 1.4 2005/03/15 23:21:26 eli Exp $";
#endif

/*  NOTICE: Change of Copyright Status
 *
 *  The author of this module, Carsten Grammes, has expressed in
 *  personal email that he has no more interest in this code, and
 *  doesn't claim any copyright. He has agreed to put this module
 *  into the public domain.
 *
 *  Lars Hecking  15-02-1999
 */

/*
 *	Matrix algebra, part of
 *
 *	Nonlinear least squares fit according to the
 *	Marquardt-Levenberg-algorithm
 *
 *	added as Patch to Gnuplot (v3.2 and higher)
 *	by Carsten Grammes
 *	Experimental Physics, University of Saarbruecken, Germany
 *
 *	Internet address: cagr@rz.uni-sb.de
 *
 *	Copyright of this module:   Carsten Grammes, 1993
 *
 *	Permission to use, copy, and distribute this software and its
 *	documentation for any purpose with or without fee is hereby granted,
 *	provided that the above copyright notice appear in all copies and
 *	that both that copyright notice and this permission notice appear
 *	in supporting documentation.
 *
 *	This software is provided "as is" without express or implied warranty.
 */

#define NULL 0
#define null 0

#include "fit.h"
#include "matrix.h"
#include <math.h>

// create a simple gc malloc...
typedef struct Node {
  struct Node * next;
  void * ptr;
} Node;

Node * head = null;

void * my_gc_malloc(int size) {
  void * ptr = malloc(size);
  Node * n = (Node *)malloc(sizeof (Node));
  n->ptr = ptr;
  n->next = head;
  head = n;
  return ptr;
}

void gc_cleanup(){
  while(head) {
    Node * current = head;
    head = current->next;
    free(current->ptr);
    free(current);
  }
}



/*****************************************************************/

#define Swap(a,b)   {double temp = (a); (a) = (b); (b) = temp;}
#define WINZIG	      1e-30


/*****************************************************************
    internal prototypes
*****************************************************************/

static int fsign (double x);

/*****************************************************************
    first straightforward vector and matrix allocation functions
*****************************************************************/
MZ_DLLEXPORT
double *vec (n)
int n;
{
    /* allocates a double vector with n elements */
    double *dp;
    if( n < 1 )
	return (double *) NULL;
    dp = (double *) my_gc_malloc (n * sizeof(double));
    return dp;
}


MZ_DLLEXPORT
double **matr (rows, cols)
int rows;
int cols;
{
    /* allocates a double matrix */

    register int i;
    register double **m;

    if ( rows < 1  ||  cols < 1 )
        return NULL;
    m = (double **) my_gc_malloc (rows * sizeof(double *));
    m[0] = (double *) my_gc_malloc (rows * cols * sizeof(double));
    for ( i = 1; i<rows ; i++ )
    	m[i] = m[i-1] + cols;
    return m;
}


void free_matr (m)
double **m;
{
    free (m[0]);
    free (m);
}


MZ_DLLEXPORT
double *redim_vec (v, n)
double **v;
int n;
{
    if ( n < 1 ) 
      *v = NULL;
    else       
      *v = (double *) my_gc_malloc( n * sizeof(double));
    return *v;
}

MZ_DLLEXPORT
void redim_ivec (v, n)
int **v;
int n;
{
    if ( n < 1 ) {
	*v = NULL;
	return;
    }
    *v = (int *) my_gc_malloc ( n * sizeof(int));
}


/* HBB: TODO: is there a better value for 'epsilon'? how to specify
 * 'inline'?  is 'fsign' really not available elsewhere? use
 * row-oriented version (p. 309) instead?
 */

static int fsign(x)
  double x;
{
    return( x>0 ? 1 : (x < 0) ? -1 : 0) ;
}

/*****************************************************************

     Solve least squares Problem C*x+d = r, |r| = min!, by Given rotations
     (QR-decomposition). Direct implementation of the algorithm
     presented in H.R.Schwarz: Numerische Mathematik, 'equation'
     number (7.33)

     If 'd == NULL', d is not accesed: the routine just computes the QR
     decomposition of C and exits.

     If 'want_r == 0', r is not rotated back (\hat{r} is returned
     instead).

*****************************************************************/

MZ_DLLEXPORT
void Givens (C, d, x, r, N, n, want_r)
double **C;
double *d;
double *x;
double *r;
int N;
int n;
int want_r;
{
    int i, j, k;
    double w, gamma, sigma, rho, temp;
    double epsilon = 1e-5; /* FIXME (?)*/

/* 
 * First, construct QR decomposition of C, by 'rotating away'
 * all elements of C below the diagonal. The rotations are
 * stored in place as Givens coefficients rho.
 * Vector d is also rotated in this same turn, if it exists 
 */
    for (j = 0; j<n; j++) 
    	for (i = j+1; i<N; i++) 
    	    if (C[i][j]) {
    	    	if (fabs(C[j][j])<epsilon*fabs(C[i][j])) { /* find the rotation parameters */
    	    	    w = -C[i][j];
    	    	    gamma = 0;
    	    	    sigma = 1;
    	    	    rho = 1;
		} else {
		    w = fsign(C[j][j])*sqrt(C[j][j]*C[j][j] + C[i][j]*C[i][j]);
		    if (w == 0) {
		      //			Eex3 ( "w = 0 in Givens();  Cjj = %g,  Cij = %g", C[j][j], C[i][j]);
		    }
		    gamma = C[j][j]/w;
		    sigma = -C[i][j]/w;
		    rho = (fabs(sigma)<gamma) ? sigma : fsign(sigma)/gamma;
		}
		C[j][j] = w;
		C[i][j] = rho;           /* store rho in place, for later use */
		for (k = j+1; k<n; k++) {   /* rotation on index pair (i,j) */
		    temp =    gamma*C[j][k] - sigma*C[i][k];
		    C[i][k] = sigma*C[j][k] + gamma*C[i][k];
		    C[j][k] = temp;
		    
		}
		if (d) {               /* if no d vector given, don't use it */
		    temp = gamma*d[j] - sigma*d[i];  /* rotate d */
		    d[i] = sigma*d[j] + gamma*d[i];
		    d[j] = temp;
	        }
	    }
    if (!d)               /* stop here if no d was specified */
         return;

    for (i = n-1; i >= 0; i--) {   /* solve R*x+d = 0, by backsubstitution */
        double s = d[i];
        r[i] = 0;              /* ... and also set r[i] = 0 for i<n */
        for (k = i+1; k<n; k++) 
            s += C[i][k]*x[k];
	if (C[i][i] == 0) {
	  //Eex ( "Singular matrix in Givens()");
	}
        x[i] = - s / C[i][i];
	}
    for (i = n; i < N; i++) 
    	r[i] = d[i];	     	/* set the other r[i] to d[i] */
    	
    if (!want_r)        	/* if r isn't needed, stop here */
    	return;
    	
    /* rotate back the r vector */
    for (j = n-1; j >= 0; j--)
    	for (i = N-1; i >= 0; i--) {
    	    if ((rho = C[i][j]) == 1) { /* reconstruct gamma, sigma from stored rho */
    	     	gamma = 0;
    	     	sigma = 1;
    	    } else if (fabs(rho)<1) {
    	    	sigma = rho; 
    	    	gamma = sqrt(1-sigma*sigma);
    	    } else {
    	    	gamma = 1/fabs(rho);
    	    	sigma = fsign(rho)*sqrt(1-gamma*gamma);
    	    }
	    temp = gamma*r[j] + sigma*r[i];	/* rotate back indices (i,j) */
	    r[i] = -sigma*r[j] + gamma*r[i];
	    r[j] = temp;
    }
}


/* Given a triangular Matrix R, compute (R^T * R)^(-1), by forward
 * then back substitution
 * 
 * R, I are n x n Matrices, I is for the result. Both must already be
 * allocated.
 * 
 * Will only calculate the lower triangle of I, as it is symmetric 
 */

MZ_DLLEXPORT
void Invert_RtR ( R, I, n)
double **R;
double **I;
int n;
{
  int i, j, k;

  /* fill in the I matrix, and check R for regularity : */

  for (i = 0; i<n; i++) {
    for (j = 0; j<i; j++)  /* upper triangle isn't needed */
      I[i][j] = 0;
    I[i][i] = 1;
    if (! R[i][i])
      {
      //      Eex ("Singular matrix in Invert_RtR");
      }
      }
  
  /* Forward substitution: Solve R^T * B = I, store B in place of I */
  
  for (k = 0; k<n; k++) 
    for (i = k; i<n; i++) {  /* upper half needn't be computed */
      double s = I[i][k];
      for (j = k; j<i; j++)  /* for j<k, I[j][k] always stays zero! */
	s -= R[j][i] * I[j][k];
      I[i][k] = s / R[i][i];
    }

  /* Backward substitution: Solve R * A = B, store A in place of B */

  for (k = 0; k<n; k++)
    for (i = n-1; i >= k; i--) {  /* don't compute upper triangle of A */
      double s = I[i][k];
      for (j = i+1; j<n; j++)
	s -= R[i][j] * I[j][k];
      I[i][k] = s / R[i][i]; 
    }
}
