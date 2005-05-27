/* $Id: plsdef.c,v 1.1 2004/03/01 20:54:52 cozmic Exp $

	Routines to set various plplot parameters, such as char height,
	symbol size, tick length, line and fill patterns, etc.
*/

#include "plplotP.h"

/* Line pattern defaults */

static struct line {
    PLINT nels;
    PLINT mark[4];
    PLINT space[4];
} line[] = {

    {
	0,			/* Continuous line */
	{
	    0, 0, 0, 0
	},
	{
	    0, 0, 0, 0
	}
    },
    {
	1,
	{
	    1000, 0, 0, 0
	},
	{
	    1000, 0, 0, 0
	}
    },
    {
	1,
	{
	    2000, 0, 0, 0
	},
	{
	    2000, 0, 0, 0
	}
    },
    {
	1,
	{
	    2000, 0, 0, 0
	},
	{
	    1000, 0, 0, 0
	}
    },
    {
	2,
	{
	    2500, 1000, 0, 0
	},
	{
	    1000, 1000, 0, 0
	}
    },
    {
	2,
	{
	    2000, 1000, 0, 0
	},
	{
	    2000, 1000, 0, 0
	}
    },
    {
	3,
	{
	    1000, 1500, 2000, 0
	},
	{
	    1000, 1500, 2000, 0
	}
    },
    {
	3,
	{
	    1000, 1500, 2000, 0
	},
	{
	    1000, 1000, 1000, 0
	}
    }
};

/* Fill pattern defaults */

static struct pattern {
    PLINT nlines;		/* Number of lines in pattern (1 or 2) */
    PLINT inc[2];		/* Inclination 10 ths of degrees */
    PLINT del[2];		/* Spacing for each line */
} pattern[] = {

    {
	1,
	{
	    0, 0
	},
	{
	    2000, 0
	}
    },
    {
	1,
	{
	    900, 0
	},
	{
	    2000, 0
	}
    },
    {
	1,
	{
	    450, 0
	},
	{
	    2000, 0
	}
    },
    {
	1,
	{
	    -450, 0
	},
	{
	    2000, 0
	}
    },
    {
	1,
	{
	    300, 0
	},
	{
	    2000, 0
	}
    },
    {
	1,
	{
	    -300, 0
	},
	{
	    2000, 0
	}
    },
    {
	2,
	{
	    0, 900
	},
	{
	    2000, 2000
	}
    },
    {
	2,
	{
	    450, -450
	},
	{
	    2000, 2000
	}
    }
};

/* Set defining parameters for pattern fill */

static void
spat(PLINT inc[], PLINT del[], PLINT nlin);

/*----------------------------------------------------------------------*\
 * void plschr()
 *
 * Set character height.
\*----------------------------------------------------------------------*/

void
c_plschr(PLFLT def, PLFLT scale)
{
    if (def != 0.0) 
	plsc->chrdef = def;

    plsc->chrht = scale * plsc->chrdef;
}

/*----------------------------------------------------------------------*\
 * void plsmin()
 *
 * Set up lengths of minor tick marks.
\*----------------------------------------------------------------------*/

void
c_plsmin(PLFLT def, PLFLT scale)
{
    if (def != 0.0) 
	plsc->mindef = def;

    plsc->minht = scale * plsc->mindef;
}

/*----------------------------------------------------------------------*\
 * void plsmaj()
 *
 * Set up lengths of major tick marks.
\*----------------------------------------------------------------------*/

void
c_plsmaj(PLFLT def, PLFLT scale)
{
    if (def != 0.0) 
	plsc->majdef = def;

    plsc->majht = scale * plsc->majdef;
}

/*----------------------------------------------------------------------*\
 * void plssym()
 *
 * Set symbol height.
\*----------------------------------------------------------------------*/

void
c_plssym(PLFLT def, PLFLT scale)
{
    if (def != 0.0) 
	plsc->symdef = def;

    plsc->symht = scale * plsc->symdef;
}

/*----------------------------------------------------------------------*\
 * void pllsty()
 *
 * Set line style.
\*----------------------------------------------------------------------*/

void
c_pllsty(PLINT lin)
{
    if (plsc->level < 1) {
	plabort("pllsty: Please call plinit first");
	return;
    }
    if (lin < 1 || lin > 8) {
	plabort("pllsty: Invalid line style");
	return;
    }

    plstyl(line[lin - 1].nels,
	   &line[lin - 1].mark[0], &line[lin - 1].space[0]);
}

/*----------------------------------------------------------------------*\
 * void plpat()
 *
 * Set fill pattern directly.
\*----------------------------------------------------------------------*/

void
c_plpat(PLINT nlin, PLINT *inc, PLINT *del)
{
    PLINT i;

    if (plsc->level < 1) {
	plabort("plpat: Please call plinit first");
	return;
    }
    if (nlin < 1 || nlin > 2) {
	plabort("plpat: Only 1 or 2 line styles allowed");
	return;
    }
    for (i = 0; i < nlin; i++) {
	if (del[i] < 0) {
	    plabort("plpat: Line spacing must be greater than 0");
	    return;
	}
    }
    spat(inc, del, nlin);
}

/*----------------------------------------------------------------------*\
 * void plpsty()
 *
 * Set fill pattern, using one of the predefined patterns.
 * A fill pattern <= 0 indicates hardware fill.
\*----------------------------------------------------------------------*/

void
c_plpsty(PLINT patt)
{
    if (plsc->level < 1) {
	plabort("plpsty: Please call plinit first");
	return;
    }
    if (patt > 8) {
	plabort("plpsty: Invalid pattern");
	return;
    }
    if (patt != plsc->patt) {
	plsc->patt = patt;

	if (plsc->level > 0) {
	    plP_state(PLSTATE_FILL);
	}
    }
    if (patt > 0) {
	spat(&pattern[patt - 1].inc[0], &pattern[patt - 1].del[0],
	     pattern[patt - 1].nlines);
    }
}

/*----------------------------------------------------------------------*\
 * void spat()
 *
 * Set defining parameters for pattern fill 
\*----------------------------------------------------------------------*/

static void
spat(PLINT inc[], PLINT del[], PLINT nlin)
{
    PLINT i;

    plsc->nps = nlin;
    for (i = 0; i < nlin; i++) {
	plsc->inclin[i] = inc[i];
	plsc->delta[i] = del[i];
    }
}
