/* $Id: matrix.h,v 1.5 2005/03/15 23:23:56 eli Exp $ */

/* GNUPLOT - matrix.h */

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
 *	Header file: public functions in matrix.c
 *
 *
 *	Copyright of this module:   Carsten Grammes, 1993
 *      Experimental Physics, University of Saarbruecken, Germany
 *
 *	Internet address: cagr@rz.uni-sb.de
 *
 *	Permission to use, copy, and distribute this software and its
 *	documentation for any purpose with or without fee is hereby granted,
 *	provided that the above copyright notice appear in all copies and
 *	that both that copyright notice and this permission notice appear
 *	in supporting documentation.
 *
 *      This software is provided "as is" without express or implied warranty.
 */


#ifndef MATRIX_H
#define MATRIX_H

#include "../dllexport.h"


#ifdef EXT
#undef EXT
#endif

#ifdef MATRIX_MAIN
#define EXT
#else
#define EXT extern
#endif


/******* public functions ******/

MZ_DLLEXPORT
EXT double  *vec (int n);
MZ_DLLEXPORT
EXT int     *ivec (int n);
MZ_DLLEXPORT
EXT double  **matr (int r, int c);
MZ_DLLEXPORT
EXT double  *redim_vec (double **v, int n);
MZ_DLLEXPORT
EXT void    redim_ivec (int **v, int n);
EXT void    solve (double **a, int n, double **b, int m);
MZ_DLLEXPORT
EXT void    Givens (double **C, double *d, double *x, double *r, int N, int n, int want_r); 
MZ_DLLEXPORT
EXT void    Invert_RtR (double **R, double **I, int n);

#endif

// a kludgy version of a malloc

void * my_gc_malloc(int size);
void gc_cleanup();

