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
 *    Nonlinear least squares fit according to the
 *      Marquardt-Levenberg-algorithm
 *
 *      added as Patch to Gnuplot (v3.2 and higher)
 *      by Carsten Grammes
 *      Experimental Physics, University of Saarbruecken, Germany
 *
 *      Internet address: cagr@rz.uni-sb.de
 *
 *      Copyright of this module:  1993, 1998  Carsten Grammes
 *
 *      Permission to use, copy, and distribute this software and its
 *      documentation for any purpose with or without fee is hereby granted,
 *      provided that the above copyright notice appear in all copies and
 *      that both that copyright notice and this permission notice appear
 *      in supporting documentation.
 *
 *      This software is provided "as is" without express or implied warranty.
 *
 *      930726:     Recoding of the Unix-like raw console I/O routines by:
 *                  Michele Marziani (marziani@ferrara.infn.it)
 * drd: start unitialised variables at 1 rather than NEARLY_ZERO
 *  (fit is more likely to converge if started from 1 than 1e-30 ?)
 *
 * HBB (Broeker@physik.rwth-aachen.de) : fit didn't calculate the errors
 * in the 'physically correct' (:-) way, if a third data column containing
 * the errors (or 'uncertainties') of the input data was given. I think
 * I've fixed that, but I'm not sure I really understood the M-L-algo well
 * enough to get it right. I deduced my change from the final steps of the
 * equivalent algorithm for the linear case, which is much easier to
 * understand. (I also made some minor, mostly cosmetic changes)
 *
 * HBB (again): added error checking for negative covar[i][i] values and
 * for too many parameters being specified.
 *
 * drd: allow 3d fitting. Data value is now called fit_z internally,
 * ie a 2d fit is z vs x, and a 3d fit is z vs x and y.
 *
 * Lars Hecking : review update command, for VMS in particular, where
 * it is not necessary to rename the old file.
 *
 * HBB, 971023: lifted fixed limit on number of datapoints, and number
 * of parameters.
 */


#define FIT_MAIN

#define NULL 0

//#include <scheme.h>



#include "matrix.h"
#include "fit.h"
#include <math.h>

/* #define STANDARD stderr */


enum marq_res {
    OK, ERROR, BETTER, WORSE
};
typedef enum marq_res marq_res_t;

#ifdef INFINITY
# undef INFINITY
#endif

#define INFINITY    1e30
#define NEARLY_ZERO 1e-30


/* Relative change for derivatives */
#define DELTA	    0.001

#define MAX_DATA    2048
#define MAX_PARAMS  32
#define MAX_LAMBDA  1e20
#define MIN_LAMBDA  1e-20
#define LAMBDA_UP_FACTOR 10
#define LAMBDA_DOWN_FACTOR 10

#define TBOOLEAN int

# define PLUSMINUS   "+/-"



/* HBB 971023: new, allow for dynamic adjustment of these: */
static int max_data;
static int max_params;

static double epsilon = 1e-5;	/* convergence limit */
static int maxiter = 0;		/* HBB 970304: maxiter patch */

static char *FIXED = "# FIXED";
static char *GNUFITLOG = "FIT_LOG";
static char *FITLIMIT = "FIT_LIMIT";
static char *FITSTARTLAMBDA = "FIT_START_LAMBDA";
static char *FITLAMBDAFACTOR = "FIT_LAMBDA_FACTOR";
static char *FITMAXITER = "FIT_MAXITER";	/* HBB 970304: maxiter patch */
static char *FITSCRIPT = "FIT_SCRIPT";
static char *DEFAULT_CMD = "replot";	/* if no fitscript spec. */


static int num_data, num_params;
static int columns;
static double  *fit_x;
static double    *fit_y;
static double  *fit_z ;
static double   *err_data;
static double *a;


/* static fixstr * par_name; */

static double startup_lambda = 0;
static double lambda_down_factor = LAMBDA_DOWN_FACTOR;
static double lambda_up_factor = LAMBDA_UP_FACTOR;


static void * current_fun;


/*****************************************************************
			 internal vars to store results of fit
*****************************************************************/

double rms = 0;
double varience = 0;
double *asym_error;
double *asym_error_percent;

MZ_DLLEXPORT
double get_rms()
{return rms;}

MZ_DLLEXPORT
double get_varience()
{return varience;}

MZ_DLLEXPORT
double * get_asym_error()
{return asym_error;}

MZ_DLLEXPORT
double * get_asym_error_percent()
{return asym_error_percent;}


/*****************************************************************
			 internal Prototypes
*****************************************************************/

/*static void printmatrix __PROTO((double **C, int m, int n)); */
static void print_matrix_and_vectors (double **C, double *d, double *r, int m, int n);
static marq_res_t marquardt (double a[], double **alpha, double *chisq, 
			     double *lambda); 
static TBOOLEAN analyze (double a[], double **alpha, double beta[], 
			 double *chisq);
static void calculate (double *zfunc, double **dzda, double a[]);
static void call_scheme (double *par, double *data);

static TBOOLEAN regress (double a[]);
//static void show_fit (int i, double chisq, double last_chisq, double *a, 
//			      double lambda, FILE * device); 


/*****************************************************************
    New utility routine: print a matrix (for debugging the alg.)
*****************************************************************/
static void printmatrix(C, m, n)
double **C;
int m, n;
{
    int i, j;

    for (i = 0; i < m; i++) {
	for (j = 0; j < n - 1; j++);
	  /* Dblf2("%.8g |", C[i][j]); */
	/* Dblf2("%.8g\n", C[i][j]); */
    }
    /* Dblf("\n"); */
}

/**************************************************************************
    Yet another debugging aid: print matrix, with diff. and residue vector
**************************************************************************/
static void print_matrix_and_vectors(C, d, r, m, n)
double **C;
double *d, *r;
int m, n;
{
    int i, j;

    for (i = 0; i < m; i++) {
	for (j = 0; j < n; j++);
	  /* Dblf2("%8g ", C[i][j]); */
	  /* Dblf3("| %8g | %8g\n", d[i], r[i]); */
    }
    /* Dblf("\n"); */
}


/*****************************************************************
    Marquardt's nonlinear least squares fit
*****************************************************************/
static marq_res_t marquardt(a, C, chisq, lambda)
double a[];
double **C;
double *chisq;
double *lambda;
{
    int i, j;
    static double *da = 0,	/* delta-step of the parameter */
    *temp_a = 0,		/* temptative new params set   */
    *d = 0, *tmp_d = 0, **tmp_C = 0, *residues = 0;
    double tmp_chisq;

    /* Initialization when lambda == -1 */

    if (*lambda == -1) {	/* Get first chi-square check */
	TBOOLEAN analyze_ret;

	temp_a = vec(num_params);
	d = vec(num_data + num_params);
	tmp_d = vec(num_data + num_params);
	da = vec(num_params);
	residues = vec(num_data + num_params);
	tmp_C = matr(num_data + num_params, num_params);

	analyze_ret = analyze(a, C, d, chisq);

	/* Calculate a useful startup value for lambda, as given by Schwarz */
	/* FIXME: this is doesn't turn out to be much better, really... */
	if (startup_lambda != 0)
	    *lambda = startup_lambda;
	else {
	    *lambda = 0;
	    for (i = 0; i < num_data; i++)
		for (j = 0; j < num_params; j++)
		    *lambda += C[i][j] * C[i][j];
	    *lambda = sqrt(*lambda / num_data / num_params);
	}

	/* Fill in the lower square part of C (the diagonal is filled in on
	   each iteration, see below) */
	for (i = 0; i < num_params; i++)
	    for (j = 0; j < i; j++)
		C[num_data + i][j] = 0, C[num_data + j][i] = 0;
	/* printmatrix(C, num_data+num_params, num_params); */
	return analyze_ret ? OK : ERROR;
    }
    /* once converged, free dynamic allocated vars */

    if (*lambda == -2) {
	return OK;
    }
    /* Givens calculates in-place, so make working copies of C and d */

    for (j = 0; j < num_data + num_params; j++)
	memcpy(tmp_C[j], C[j], num_params * sizeof(double));
    memcpy(tmp_d, d, num_data * sizeof(double));

    /* fill in additional parts of tmp_C, tmp_d */

    for (i = 0; i < num_params; i++) {
	/* fill in low diag. of tmp_C ... */
	tmp_C[num_data + i][i] = *lambda;
	/* ... and low part of tmp_d */
	tmp_d[num_data + i] = 0;
    }
    /* printmatrix(tmp_C, num_data+num_params, num_params); */

    /* FIXME: residues[] isn't used at all. Why? Should it be used? */

    Givens(tmp_C, tmp_d, da, residues, num_params + num_data, num_params, 1);
    /*print_matrix_and_vectors (tmp_C, tmp_d, residues,
       num_params+num_data, num_params); */

    /* check if trial did ameliorate sum of squares */

    for (j = 0; j < num_params; j++)
	temp_a[j] = a[j] + da[j];

    if (!analyze(temp_a, tmp_C, tmp_d, &tmp_chisq)) {
	/* FIXME: will never be reached: always returns TRUE */
	return ERROR;
    }

    if (tmp_chisq < *chisq) {	/* Success, accept new solution */
	if (*lambda > MIN_LAMBDA) {
	  /* (void) putc('/', stderr); */
          *lambda /= lambda_down_factor;
	}
	*chisq = tmp_chisq;
	for (j = 0; j < num_data; j++) {
	    memcpy(C[j], tmp_C[j], num_params * sizeof(double));
	    d[j] = tmp_d[j];
	}
	for (j = 0; j < num_params; j++)
	    a[j] = temp_a[j];
	return BETTER;
    } else {			/* failure, increase lambda and return */
      /* (void) putc('*', stderr); */
      *lambda *= lambda_up_factor;
      return WORSE;
    }
}


/* FIXME: in the new code, this function doesn't really do enough to be
 * useful. Maybe it ought to be deleted, i.e. integrated with
 * calculate() ?
 */
/*****************************************************************
    compute chi-square and numeric derivations
*****************************************************************/
static TBOOLEAN analyze(a, C, d, chisq)
double a[];
double **C;
double d[];
double *chisq;
{
/*
 *  used by marquardt to evaluate the linearized fitting matrix C
 *  and vector d, fills in only the top part of C and d
 *  I don't use a temporary array zfunc[] any more. Just use
 *  d[] instead.
 */
    int i, j;

    *chisq = 0;
    calculate(d, C, a);

    for (i = 0; i < num_data; i++) {
	/* note: order reversed, as used by Schwarz */
	d[i] = (d[i] - fit_z[i]) / err_data[i];
	*chisq += d[i] * d[i];
	for (j = 0; j < num_params; j++)
	    C[i][j] /= err_data[i];
    }
    /* FIXME: why return a value that is always TRUE ? */
    return 1;
}


/* To use the more exact, but slower two-side formula, activate the
   following line: */

#define TWO_SIDE_DIFFERENTIATION 

/*****************************************************************
    compute function values and partial derivatives of chi-square
*****************************************************************/
static void calculate(zfunc, dzda, a)
double *zfunc;
double **dzda;
double a[];
{
    int k, p;
    double tmp_a;
    double *tmp_high, *tmp_pars;
#ifdef TWO_SIDE_DIFFERENTIATION
    double *tmp_low;
#endif

    tmp_high = vec(num_data);	/* numeric derivations */
#ifdef TWO_SIDE_DIFFERENTIATION
    tmp_low = vec(num_data);
#endif
    tmp_pars = vec(num_params);

    /* first function values */

    call_scheme(a, zfunc);

    /* then derivatives */

    for (p = 0; p < num_params; p++)
	tmp_pars[p] = a[p];
    for (p = 0; p < num_params; p++) {
	tmp_a = fabs(a[p]) < NEARLY_ZERO ? NEARLY_ZERO : a[p];
	tmp_pars[p] = tmp_a * (1 + DELTA);
	call_scheme(tmp_pars, tmp_high);
#ifdef TWO_SIDE_DIFFERENTIATION
	tmp_pars[p] = tmp_a * (1 - DELTA);
	call_scheme(tmp_pars, tmp_low);
#endif
	for (k = 0; k < num_data; k++)
#ifdef TWO_SIDE_DIFFERENTIATION
	    dzda[k][p] = (tmp_high[k] - tmp_low[k]) / (2 * tmp_a * DELTA);
#else
	    dzda[k][p] = (tmp_high[k] - zfunc[k]) / (tmp_a * DELTA);
#endif
	tmp_pars[p] = a[p];
    }

}


/*****************************************************************
    evaluate the scheme function
*****************************************************************/
static void call_scheme(par, data)
double *par;
double *data;
{
  int rators = 2 + num_params;
  double * rands =
    (double *) malloc(rators * sizeof(double));

  int i;

  /* set up the constant params */
  for(i = 0 ; i< num_params; i++) {
    rands[i+2] = par[i];
  }

  /* now calculate the function at the existing points */
  for (i = 0; i < num_data; i++) {
    rands[0] = fit_x[i];
    rands[1] = fit_y[i];

    data[i] = ((double (*) (int, double *) )current_fun) // ouch!
               (rators, rands);
  }

  free(rands);

}

/* /\***************************************************************** */
/*     evaluate the scheme function */
/* *****************************************************************\/ */
/* static void call_scheme(par, data) */
/* double *par; */
/* double *data; */
/* { */
/*   int rators = 2 + num_params; */
/*   Scheme_Object ** rands =  */
/*     scheme_malloc(rators * sizeof(Scheme_Object)); */

/*   int i; */

/*   /\* set up the constant params *\/ */
/*   for(i = 0 ; i< num_params; i++) { */
/*     rands[i+2] = scheme_make_double(par[i]); */
/*   } */

/*   /\* now calculate the function at the existing points *\/ */
/*     for (i = 0; i < num_data; i++) { */
/*       rands[0] = scheme_make_double(fit_x[i]); */
/*       rands[1] = scheme_make_double(fit_y[i]); */

/*       data[i] = scheme_real_to_double(scheme_apply(current_fun, rators, rands)); */
/*     } */
/* } */

/*****************************************************************
    Frame routine for the marquardt-fit
*****************************************************************/
static TBOOLEAN regress(a)
    double a[];
{
  double **covar, *dpar, **C, chisq, last_chisq, lambda;
  int iter, i, j;
  marq_res_t res;
  
  chisq = last_chisq = INFINITY;
  C = matr(num_data + num_params, num_params);
  lambda = -1;          /* use sign as flag */
  iter = 0;             /* iteration counter  */

  /* Initialize internal variables and 1st chi-square check */

  if ((res = marquardt(a, C, &chisq, &lambda)) == ERROR)
    return 0; /* an error occurded */

  res = BETTER;

  /* show_fit(iter, chisq, chisq, a, lambda, STANDARD); */

  /* MAIN FIT LOOP: do the regression iteration */

  do {
    if (res == BETTER) {
      iter++;
      last_chisq = chisq;
    }
    if ((res = marquardt(a, C, &chisq, &lambda)) == BETTER)
      {};
    /* show_fit(iter, chisq, last_chisq, a, lambda, STANDARD); */
  } while ((res != ERROR)
	   && (lambda < MAX_LAMBDA)
	   && ((maxiter == 0) || (iter <= maxiter))
	   && (res == WORSE
	       || ((chisq > NEARLY_ZERO)
		   ? ((last_chisq - chisq) / chisq)
		   : (last_chisq - chisq)) > epsilon
	       )
	   );

    /* fit done */

  /* save all the info that was otherwise printed out */

  rms = sqrt(chisq / (num_data - num_params));
  varience = chisq / (num_data - num_params);
  asym_error = malloc (num_params * sizeof (double));  
  asym_error_percent = malloc (num_params * sizeof (double)) ;

  /* don't know what the following code does... */

  /* compute covar[][] directly from C */ 
  Givens(C, 0, 0, 0, num_data, num_params, 0); 
  covar = C + num_data; 
  Invert_RtR(C, covar, num_params);
  
  dpar = vec(num_params); 
  for (i = 0; i < num_params; i++) {
    /* FIXME: can this still happen ? */
      if (covar[i][i] <= 0.0)	/* HBB: prevent floating point exception later on */
	return 0;  /* Eex("Calculation error: non-positive diagonal element in covar. matrix"); */
    dpar[i] = sqrt(covar[i][i]);
  }  

  /* transform covariances into correlations */
  for (i = 0; i < num_params; i++) {
    /* only lower triangle needs to be handled */
      for (j = 0; j <= i; j++)
      covar[i][j] /= dpar[i] * dpar[j];
  }

  /* scale parameter errors based on chisq */
  chisq = sqrt(chisq / (num_data - num_params));
  for (i = 0; i < num_params; i++)
    dpar[i] *= chisq;

  for(i = 0; i< num_params; i++)
    {
      double temp = 
	(fabs(a[i]) < NEARLY_ZERO) ? 0.0 : fabs(100.0 * dpar[i] / a[i]);
      asym_error[i] = dpar[i];
      asym_error_percent[i] = temp;
    }

  return 1;


    /******** CRAP LEFT OVER FROM GNUPLOT ***********/

    /* HBB 970304: the maxiter patch: */
    /*
    if ((maxiter > 0) && (iter > maxiter)) {
	Dblf2("\nMaximum iteration count (%d) reached. Fit stopped.\n", maxiter);
    } else  {
	Dblf2("\nAfter %d iterations the fit converged.\n", iter);
    }

    Dblf2("final sum of squares of residuals : %g\n", chisq);
    if (chisq > NEARLY_ZERO) {
	Dblf2("rel. change during last iteration : %g\n\n", (chisq - last_chisq) / chisq);
    } else {
	Dblf2("abs. change during last iteration : %g\n\n", (chisq - last_chisq));
    }

    if (res == ERROR)
      // Eex("FIT: error occurred during fit");
    */
    /* compute errors in the parameters */

  /*   if (num_data == num_params) { */
/* 	int i; */

/* 	Dblf("\nExactly as many data points as there are parameters.\n"); */
/* 	Dblf("In this degenerate case, all errors are zero by definition.\n\n"); */
/* 	Dblf("Final set of parameters \n"); */
/* 	Dblf("======================= \n\n"); */
/* 	for (i = 0; i < num_params; i++) */
/* 	    Dblf3("%-15.15s = %-15g\n", par_name[i], a[i]); */
/*     } else if (chisq < NEARLY_ZERO) { */
/* 	int i; */

/* 	Dblf("\nHmmmm.... Sum of squared residuals is zero. Can't compute errors.\n\n"); */
/* 	Dblf("Final set of parameters \n"); */
/* 	Dblf("======================= \n\n"); */
/* 	for (i = 0; i < num_params; i++) */
/* 	    Dblf3("%-15.15s = %-15g\n", par_name[i], a[i]); */
/*     } else { */
/* 	Dblf2("degrees of freedom (ndf) : %d\n",  num_data - num_params); */
/* 	Dblf2("rms of residuals      (stdfit) = sqrt(WSSR/ndf)      : %g\n", sqrt(chisq / (num_data - num_params))); */
/*  	Dblf2("variance of residuals (reduced chisquare) = WSSR/ndf : %g\n\n", chisq / (num_data - num_params)); */
 
/* 	/\* get covariance-, Korrelations- and Kurvature-Matrix *\/ */
/* 	/\* and errors in the parameters                     *\/ */

/* 	/\* compute covar[][] directly from C *\/ */
/* 	Givens(C, 0, 0, 0, num_data, num_params, 0); */
/* 	/\*printmatrix(C, num_params, num_params); *\/ */

/* 	/\* Use lower square of C for covar *\/ */
/* 	covar = C + num_data; */
/* 	Invert_RtR(C, covar, num_params); */
/* 	/\*printmatrix(covar, num_params, num_params); *\/ */

/* 	/\* calculate unscaled parameter errors in dpar[]: *\/ */
/* 	dpar = vec(num_params); */
/* 	for (i = 0; i < num_params; i++) { */
/* 	    /\* FIXME: can this still happen ? *\/ */
/* 	    if (covar[i][i] <= 0.0)	/\* HBB: prevent floating point exception later on *\/ */
/* 		Eex("Calculation error: non-positive diagonal element in covar. matrix"); */
/* 	    dpar[i] = sqrt(covar[i][i]); */
/* 	} */

/* 	/\* transform covariances into correlations *\/ */
/* 	for (i = 0; i < num_params; i++) { */
/* 	    /\* only lower triangle needs to be handled *\/ */
/* 	    for (j = 0; j <= i; j++) */
/* 		covar[i][j] /= dpar[i] * dpar[j]; */
/* 	} */

/* 	/\* scale parameter errors based on chisq *\/ */
/* 	chisq = sqrt(chisq / (num_data - num_params)); */
/* 	for (i = 0; i < num_params; i++) */
/* 	    dpar[i] *= chisq; */

/* 	Dblf("Final set of parameters            Asymptotic Standard Error\n"); */
/* 	Dblf("=======================            ==========================\n\n"); */

/* 	for (i = 0; i < num_params; i++) { */
/* 	    double temp = */
/* 	    (fabs(a[i]) < NEARLY_ZERO) ? 0.0 : fabs(100.0 * dpar[i] / a[i]); */
/* 	    Dblf6("%-15.15s = %-15g  %-3.3s %-12.4g (%.4g%%)\n", */
/* 		  par_name[i], a[i], PLUSMINUS, dpar[i], temp); */
/* 	} */

/* 	Dblf("\n\ncorrelation matrix of the fit parameters:\n\n"); */
/* 	Dblf("               "); */

/* 	for (j = 0; j < num_params; j++) */
/* 	    Dblf2("%-6.6s ", par_name[j]); */

/* 	Dblf("\n"); */
/* 	for (i = 0; i < num_params; i++) { */
/* 	    Dblf2("%-15.15s", par_name[i]); */
/* 	    for (j = 0; j <= i; j++) { */
/* 		/\* Only print lower triangle of symmetric matrix *\/ */
/* 		Dblf2("%6.3f ", covar[i][j]); */
/* 	    } */
/* 	    Dblf("\n"); */
/* 	} */

/* 	free(dpar); */
/*     } */

    return 1;
}


/*****************************************************************
    display actual state of the fit
*****************************************************************/
/* static void show_fit(i, chisq, last_chisq, a, lambda, device) */
/* int i; */
/* double chisq; */
/* double last_chisq; */
/* double *a; */
/* double lambda; */
/* FILE *device; */
//{
  /*
    int k;

    fprintf(device, "\n\n\
Iteration %d\n\
WSSR        : %-15g   delta(WSSR)/WSSR   : %g\n\
delta(WSSR) : %-15g   limit for stopping : %g\n\
lambda	  : %g\n\n%s parameter values\n\n",
      i, chisq, chisq > NEARLY_ZERO ? (chisq - last_chisq) / chisq : 0.0,
	    chisq - last_chisq, epsilon, lambda,
	    (i > 0 ? "resultant" : "initial set of free"));
    for (k = 0; k < num_params; k++)
	fprintf(device, "%-15.15s = %g\n", par_name[k], a[k]);
  */
//}






/*****************************************************************
    Interface to scheme
*****************************************************************/
MZ_DLLEXPORT
double * do_fit(void * function,
		int n_values,
		double * x_values,
		double * y_values,
		double * z_values,
		double * errors,
		int n_parameters,
		double * parameters) {

  /* reset lambda and other parameters if desired */
  int i;
  current_fun = function;

  num_data = n_values;
  fit_x = x_values;
  fit_y = y_values;
  fit_z = z_values; /* value is stored in z */
  err_data = errors;

  a = parameters;
  num_params = n_parameters;

  /* redim_vec(&a, num_params); */
  /* par_name = (fixstr *) gp_realloc(par_name, (num_params + 1) * sizeof(fixstr), "fit param"); */

  /* avoid parameters being equal to zero */
  for (i = 0; i < num_params; i++) {
    if (a[i] == 0) {
      a[i] = NEARLY_ZERO; 
    }
  }
  
  if(regress(a)) {
    gc_cleanup();
    return a;
  }
  else { /* something went wrong */
    gc_cleanup(); 
    return NULL;
  }
}
