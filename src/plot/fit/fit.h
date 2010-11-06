/* $Id: fit.h,v 1.5 2005/03/15 23:19:40 eli Exp $ */

/* GNUPLOT - fit.h */

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
 *	Header file: public functions in fit.c
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

#include "../dllexport.h"

MZ_DLLEXPORT
double * do_fit(void * function,
		int n_values,
		double * x_values,
		double * y_values,
		double * z_values,
		double * errors,
		int n_parameters,
		double * parameters);


MZ_DLLEXPORT
double get_rms();

MZ_DLLEXPORT
double get_varience();

MZ_DLLEXPORT
double * get_asym_error();

MZ_DLLEXPORT
double * get_asym_error_percent();
