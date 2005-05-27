/* $Id: pldebug.h,v 1.1 2004/03/01 20:54:51 cozmic Exp $

    Copyright (C) 1995 by Maurice J. LeBrun

    Debugging support for PLplot.

    This software may be freely copied, modified and redistributed without
    fee provided that this copyright notice is preserved intact on all
    copies and modified copies. 
 
    There is no warranty or other guarantee of fitness of this software.
    It is provided solely "as is". The author(s) disclaim(s) all
    responsibility and liability with respect to this software's usage or
    its effect upon hardware or computer systems. 
*/

#ifndef __PLDEBUG_H__
#define __PLDEBUG_H__

#include <stdarg.h>

/* For the truly desperate debugging task */

#ifdef DEBUG_ENTER
#define dbug_enter(a) \
if (plsc->debug) \
    fprintf(stderr, "    entered %s (%s, line %d)\n", a, __FILE__, __LINE__);

#else
#define dbug_enter(a)
#endif

/* If we're using a debugging malloc, include the header file here */

#ifdef DEBUGGING_MALLOC
#include <malloc.h>
#endif

/*--------------------------------------------------------------------------*\
 * pldebug()
 *
 * Included into every plplot source file to control debugging output.  To
 * enable printing of debugging output, you must #define DEBUG before
 * including plplotP.h or specify -DDEBUG in the compile line, for each file
 * that you want to have debug output enabled.  When running the program you
 * must in addition specify -debug.  This allows debugging output to tailored
 * to many different circumstances but otherwise be fairly unobtrusive. 
 *
 * Note, any file that actually uses pldebug() must also define NEED_PLDEBUG
 * before the plplotP.h include.  This is to eliminate warnings caused by
 * those files in which this is defined but never referenced.  All this could
 * be much nicer if CPP had the abilities of m4, sigh.. 
 *
 * Syntax:
 *	pldebug(label, format [, arg1, arg2, ...] );
 *
 * The label is typically the calling function name.
\*--------------------------------------------------------------------------*/

#ifdef NEED_PLDEBUG
static void
pldebug( const char *label, ... )
{
#ifdef DEBUG
    va_list args;
    char *fmt;

    if (plsc->debug) {
	if (plsc->termin)
	    c_pltext();
	va_start(args, label);

    /* print out identifying tag */

	fprintf(stderr, "%s: ", label);

    /* print out remainder of message */
    /* Need to get fmt BEFORE it's used in the vfprintf */

	fmt = (char *) va_arg(args, char *);
	vfprintf(stderr, fmt, args);

	va_end(args);
	if (plsc->termin)
	    c_plgra();
    }
#endif	/* DEBUG */
}
#endif	/* NEED_PLDEBUG */

#endif	/* __PLDEBUG_H__ */
