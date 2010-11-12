/* $Id: pldtik.c,v 1.1 2004/03/01 20:54:51 cozmic Exp $

	Determines tick spacing and mode (fixed or floating) of
	numeric axis labels.
*/

#include "plplotP.h"

/*----------------------------------------------------------------------*\
 * void pldtik()
 *
 * Determine tick spacing: works out a "nice" interval (if tick == 0) such
 * that there are between 3 and 7.5 major tick intervals in the input
 * range vmin to vmax.  The recommended number of subticks is returned in
 * "nsubt" unless the routine is entered with a non-zero value of "nsubt".
 * n.b. big change: now returns only positive values of tick and nsubt
\*----------------------------------------------------------------------*/

void
pldtik(PLFLT vmin, PLFLT vmax, PLFLT *tick, PLINT *nsubt)
{
    PLFLT t1, t2, tick_reasonable;
    PLINT np, ns;

/* Magnitude of min/max difference to get tick spacing */

    t1 = (PLFLT) log10(ABS(vmax - vmin));
    np = (PLINT) floor(t1);
    t1 = t1 - np;

/* Get tick spacing. */

    if (t1 > 0.7781512503) {
	t2 = 2.0;
	ns = 4;
    }
    else if (t1 > 0.4771212549) {
	t2 = 1.0;
	ns = 5;
    }
    else if (t1 > 0.1760912591) {
	t2 = 5.0;
	ns = 5;
	np = np - 1;
    }
    else {
	t2 = 2.0;
	ns = 4;
	np = np - 1;
    }

/* Now compute reasonable tick spacing */

    tick_reasonable = t2 * pow(10.0, (double) np);
    if (*tick == 0) {
	*tick = t2 * pow(10.0, (double) np);
    }
    else {
        *tick = ABS(*tick);
        if(*tick < 1.e-4*tick_reasonable) {
	   plexit("pldtik: magnitude of specified tick spacing is much too small");
	   return;
	}
    }
    if (*nsubt == 0)
	*nsubt = ns;

    *nsubt = ABS(*nsubt);
}

/*----------------------------------------------------------------------*\
 * void pldprec()
 *
 * Determine precision: the output variable "mode" is set to 0 if labels
 * are to be written in floating-point format, or to 1 if they are to be
 * written in scientific format.  For mode = 1, the exponent will be
 * placed at:
 *
 * 	top left	for vertical axis on left
 * 	top right	for vertical axis on right
 * 	bottom right	for horizontal axis
 *
 * The digmax flag can be set by the user, and represents the maximum
 * number of digits a label may occupy including sign and decimal point.
 * digmin, calculated internally, is the maximum number of digits
 * labels at vmin and vmax would occupy if floating point.
 * If digmax<0, it is disregarded,
 * and if digmax=0 the default value is used.  For digmax>0, mode=1 is
 * chosen if there is insufficient room for the label within the specified
 * # of digits (digmin > digfix, where digfix is determined from digmax with
 * fuzz factors).
 *
 * In the case of mode=0, the actual # of digits will become too large
 * when the magnitude of the labels become too large.  The mode=1 case
 * offers the greatest precision for the smallest field length.
 *
 * The determination of maximum length for fixed point quantities is
 * complicated by the fact that very long fixed point representations look
 * much worse than the same sized floating point representation.  Further,
 * a fixed point number with a large negative exponent will actually gain
 * in precision when written as floating point.  Thus we use certain fuzz
 * factors to get 'digfix' from 'digmax', however it will always be true
 * that digfix<=digmax.
 *
 * Finally, if 'digmax' is set, 'prec' is reduced in size if necessary so
 * that the labels fit the requested field length, where prec is the number of 
 * places after the decimal place.
\*----------------------------------------------------------------------*/

#define MIN_FLTDIG	3	/* disregarded if fractional part is 0 */
#define MAX_FIXDIG_POS	6
#define MAX_FIXDIG_NEG	4
#define DIGMAX_DEF	5

void
pldprec(PLFLT vmin, PLFLT vmax, PLFLT tick, PLINT lf, 
	PLINT *mode, PLINT *prec, PLINT digmax, PLINT *scale)
{
    PLFLT chosen, notchosen, vmod, t0;
    PLINT msd, notmsd, np, digmin, digfix;

    *mode = 0;
    *scale = 0;

    if (digmax == 0)
	digmax = DIGMAX_DEF;

/* Choose vmin or vmax depending on magnitudes of vmin and vmax. */
    chosen = (ABS(vmax) >= ABS(vmin))? vmax: vmin;
    notchosen = (ABS(vmax) >= ABS(vmin))? vmin: vmax;
/* Magnitute of chosen to get number of significant digits */

    if(ABS(chosen) > 0.) {
        vmod = ABS(chosen);
        t0 = (PLFLT) log10(vmod);
        msd = (PLINT) floor(t0);
    }
    else {
/* this branch occurs only when 0. --- 0. range put in */
        vmod = 1.;
        t0 = (PLFLT) log10(vmod);
        msd = (PLINT) floor(t0);
    }
        
    if(ABS(notchosen) > 0.)
	notmsd = (PLINT) floor( (PLFLT) log10(ABS(notchosen)));
    else
	notmsd = msd;
/* Autoselect the mode flag */
/* 'digmin' is the minimum number of places taken up by the label */

    if (msd >= 0) {
/* n.b. no decimal point in the minimal case  */
	digmin = msd + 1;
	digfix = MAX_FIXDIG_POS;
	if (digmax > 0)
	    digfix = MIN(digmax, MAX_FIXDIG_POS);
    }
    else {
/* adjust digmin to account for leading 0 and decimal point */
	digmin = -msd + 2;
	digfix = MAX_FIXDIG_NEG;
	if (digmax > 0)
	    digfix = MIN(digmax, MAX_FIXDIG_NEG);
    }
/* adjust digmin to account for sign on the chosen end of axis or sign on the 
 * nonchosen end of axis if notmsd = msd or (msd <= 0 and notmsd < 0)
 * For the latter case the notchosen label starts with "-0."
 * For checking for the latter case, the notmsd < 0 condition is redundant
 * since notmsd <= msd always and the equal part is selected by the first
 * condition.
 */
    if(chosen < 0.||(notchosen < 0. && (notmsd == msd || msd <= 0)))
        digmin = digmin + 1;

    if (digmin > digfix && !lf) {
	*mode = 1;
	*scale = msd;
    }

/* Establish precision.  */
/* It must be fine enough to resolve the tick spacing */

    np = (PLINT) floor(log10(ABS(tick)));

    if (*mode != 0)
	*prec = msd - np;
    else
	*prec = MAX(-np, 0);

/* One last hack required: if exponent < 0, i.e. number has leading '0.',
 * it's better to change to floating point form if the number of digits
 * is insufficient to represent the tick spacing.
*/
    if (*mode == 0 && digmax > 0 && !lf) {
	if (t0 < 0.0) {
	    if (digmax - 2 - *prec < 0) {
		*mode = 1;
		*scale = msd;
	    }
	}
	else
	    *prec = MAX(MIN(*prec, digmax - msd - 1), 0);
    }
    if (*mode != 0) {
	*prec = msd - np;
	*prec = MAX(MIN(*prec, MAX(digmax-1, MIN_FLTDIG)), 0);
    }
}
