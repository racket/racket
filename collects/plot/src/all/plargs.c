/* $Id: plargs.c,v 1.1 2004/03/01 20:54:50 cozmic Exp $

    Copyright 1993, 1994, 1995
    Maurice LeBrun			mjl@dino.ph.utexas.edu
    Institute for Fusion Studies	University of Texas at Austin

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

    Some parts of this code were derived from "xterm.c" and "ParseCmd.c" of 
    the X-windows Version 11 distribution.  The copyright notice is
    reproduced here:

Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

    The full permission notice is given in the PLplot documentation.

* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

    This file contains routines to extract & process command flags.  The
    command flags recognized by PLplot are stored in the "ploption_table"
    structure, along with strings giving the syntax, long help message, and
    option handler.  

    The command line parser -- plParseOpts() -- removes all recognized flags
    (decreasing argc accordingly), so that invalid input may be readily
    detected.  It can also be used to process user command line flags.  The
    user can merge an option table of type PLOptionTable into the internal
    option table info structure using plMergeOpts().  Or, the user can
    specify that ONLY the external table(s) be parsed by calling
    plClearOpts() before plMergeOpts().  

    The default action taken by plParseOpts() is as follows:
	- Returns with an error if an unrecognized option or badly formed
	  option-value pair are encountered.
	- Returns immediately (return code 0) when the first non-option
	  command line argument is found.
	- Returns with the return code of the option handler, if one
	  was called.
	- Deletes command line arguments from argv list as they are found,
	  and decrements argc accordingly.
	- Does not show "invisible" options in usage or help messages.
	- Assumes the program name is contained in argv[0].

    These behaviors may be controlled through the "mode" argument, which can
    have the following bits set:

    PL_PARSE_FULL -- Full parsing of command line and all error messages
    enabled, including program exit when an error occurs.  Anything on the
    command line that isn't recognized as a valid option or option argument
    is flagged as an error.

    PL_PARSE_QUIET -- Turns off all output except in the case of
    errors.

    PL_PARSE_NODELETE -- Turns off deletion of processed arguments.

    PL_PARSE_SHOWALL -- Show invisible options 

    PL_PARSE_NOPROGRAM -- Specified if argv[0] is NOT a pointer to the
    program name.

    PL_PARSE_NODASH -- Set if leading dash is NOT required.

    PL_PARSE_SKIP -- Set to quietly skip over any unrecognized args.

    Note: if you want to have both your option and a PLplot option of the
    same name processed (e.g. the -v option in plrender), do the following:
	1. Tag your option with PL_OPT_NODELETE
	2. Give it an option handler that uses a return code of 1.
	3. Merge your option table in.
    By merging your table, your option will be processed before the PLplot
    one.  The PL_OPT_NODELETE ensures that the option string is not deleted
    from the argv list, and the return code of 1 ensures that the parser
    continues looking for it.

    See plrender.c for examples of actual usage.  */

#include "plplotP.h"
#include <ctype.h>

/* Support functions */

static int  ParseOpt	(int *, char ***, int *, char ***, PLOptionTable *);
static int  ProcessOpt	(char *, PLOptionTable *, int *, char ***, int *);
static int  GetOptarg	(char **, int *, char ***, int *);
static void Help	(void);
static void Syntax	(void);

/* Option handlers */

static int opt_h		(char *, char *, void *);
static int opt_v		(char *, char *, void *);
static int opt_verbose		(char *, char *, void *);
static int opt_debug		(char *, char *, void *);
static int opt_hack		(char *, char *, void *);
static int opt_dev		(char *, char *, void *);
static int opt_o		(char *, char *, void *);
static int opt_geo		(char *, char *, void *);
static int opt_a		(char *, char *, void *);
static int opt_jx		(char *, char *, void *);
static int opt_jy		(char *, char *, void *);
static int opt_mar		(char *, char *, void *);
static int opt_ori		(char *, char *, void *);
static int opt_freeaspect	(char *, char *, void *);
static int opt_portrait		(char *, char *, void *);
static int opt_width		(char *, char *, void *);
static int opt_bg		(char *, char *, void *);
static int opt_ncol0		(char *, char *, void *);
static int opt_ncol1		(char *, char *, void *);
static int opt_fam		(char *, char *, void *);
static int opt_fsiz		(char *, char *, void *);
static int opt_fbeg		(char *, char *, void *);
static int opt_finc		(char *, char *, void *);
static int opt_fflen		(char *, char *, void *);
static int opt_bufmax		(char *, char *, void *);
static int opt_nopixmap		(char *, char *, void *);
static int opt_db		(char *, char *, void *);
static int opt_np		(char *, char *, void *);
static int opt_px		(char *, char *, void *);
static int opt_py		(char *, char *, void *);
static int opt_wplt		(char *, char *, void *);
static int opt_drvopt		(char *, char *, void *);

static int opt_plserver		(char *, char *, void *);
static int opt_plwindow		(char *, char *, void *);
static int opt_tcl_cmd		(char *, char *, void *);
static int opt_auto_path	(char *, char *, void *);
static int opt_bufmax		(char *, char *, void *);
static int opt_server_name	(char *, char *, void *);
static int opt_server_host	(char *, char *, void *);
static int opt_server_port	(char *, char *, void *);
static int opt_user		(char *, char *, void *);
static int opt_tk_file          (char *, char *, void *);
static int opt_dpi		(char *, char *, void *);
static int opt_dev_compression	(char *, char *, void *);

/* Global variables */

static char *program = NULL;
static char *usage   = NULL;

static int  mode_full;
static int  mode_quiet;
static int  mode_nodelete;
static int  mode_showall;
static int  mode_noprogram;
static int  mode_nodash;
static int  mode_skip;

/* Temporary buffer used for parsing */

#define OPTMAX 1024
static char opttmp[OPTMAX];

/*--------------------------------------------------------------------------*\
 * PLPLOT options data structure definition.
 *
 * The table is defined as follows
 *
 * typedef struct {
 *     char *opt;
 *     int  (*handler)	(char *, char *, void *);
 *     void *client_data;
 *     void *var;
 *     long mode;
 *     char *syntax;
 *     char *desc;
 * } PLOptionTable;
 *
 * where each entry has the following meaning:
 *
 * opt		option string
 * handler	pointer to function for processing the option and
 *		 (optionally) its argument
 * client_data	pointer to data that gets passed to (*handler)
 * var		address of variable to set based on "mode"
 * mode		governs handling of option (see below)
 * syntax	short syntax description
 * desc		long syntax description
 *
 * The syntax and or desc strings can be NULL if the option is never to be
 * described.  Usually this is only used for obsolete arguments; those we
 * just wish to hide from normal use are better made invisible (which are
 * made visible by either specifying -showall first or PL_PARSE_SHOWALL).
 *
 * The mode bits are:
 *
 * PL_OPT_ARG		Option has an argment 
 * PL_OPT_NODELETE	Don't delete after processing 
 * PL_OPT_INVISIBLE	Make invisible (usually for debugging)
 * PL_OPT_DISABLED	Ignore this option
 *
 * The following mode bits cause the option to be processed as specified:
 *
 * PL_OPT_FUNC		Call function handler (opt, optarg)
 * PL_OPT_BOOL		Set *var=1
 * PL_OPT_INT		Set *var=atoi(optarg)
 * PL_OPT_FLOAT		Set *var=atof(optarg)
 * PL_OPT_STRING	Set *var=optarg
 *
 * where opt points to the option string and optarg points to the
 * argument string.
 *
\*--------------------------------------------------------------------------*/

static PLOptionTable ploption_table[] = {
{
    "showall",			/* Turns on invisible options */
    NULL,
    NULL,
    &mode_showall,
    PL_OPT_BOOL | PL_OPT_INVISIBLE,
    "-showall",
    "Turns on invisible options" },
{
    "h",			/* Help */
    opt_h,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-h",
    "Print out this message" },
{
    "v",			/* Version */
    opt_v,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-v",
    "Print out the PLplot library version number" },
{
    "verbose",			/* Be more verbose than usual */
    opt_verbose,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-verbose",
    "Be more verbose than usual" },
{
    "debug",			/* Print debugging info */
    opt_debug,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-debug",
    "Print debugging info (implies -verbose)" },
{
    "hack",			/* Enable driver-specific hack(s) */
    opt_hack,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_INVISIBLE,
    "-hack",
    "Enable driver-specific hack(s)" },
{
    "dev",			/* Output device */
    opt_dev,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-dev name",
    "Output device name" },
{
    "o",			/* Output filename */
    opt_o,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-o name",
    "Output filename" },
{
    "display",			/* X server */
    opt_o,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-display name",
    "X server to contact" },
{
    "px",			/* Plots per page in x */
    opt_px,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-px number",
    "Plots per page in x" },
{
    "py",			/* Plots per page in y */
    opt_py,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-py number",
    "Plots per page in y" },
{
    "geometry",			/* Geometry */
    opt_geo,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-geometry geom",
    "Window size, in pixels (e.g. -geometry 400x300)" },
{
    "geo",			/* Geometry (alias) */
    opt_geo,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-geo geom",
    "Window size, in pixels (e.g. -geo 400x300)" },
{
    "wplt",			/* Plot window */
    opt_wplt,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-wplt xl,yl,xr,yr",
    "Relative coordinates [0-1] of window into plot" },
{
    "mar",			/* Margin */
    opt_mar,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-mar margin",
    "Margin space in relative coordinates (0 to 0.5, def 0)" },
{
    "a",			/* Aspect ratio */
    opt_a,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-a aspect",
    "Page aspect ratio (def: same as output device)"},
{
    "jx",			/* Justification in x */
    opt_jx,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-jx justx",
    "Page justification in x (-0.5 to 0.5, def 0)"},
{
    "jy",			/* Justification in y */
    opt_jy,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-jy justy",
    "Page justification in y (-0.5 to 0.5, def 0)"},
{
    "ori",			/* Orientation */
    opt_ori,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-ori orient",
    "Plot orientation (0,1,2,3=landscape,portrait,seascape,upside-down)" },
{
    "freeaspect",		/* floating aspect ratio */
    opt_freeaspect,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-freeaspect",
    "Allow aspect ratio to adjust to orientation swaps" },
{
    "portrait",			/* floating aspect ratio */
    opt_portrait,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-portrait",
    "Sets portrait mode (both orientation and aspect ratio)" },
{
    "width",			/* Pen width */
    opt_width,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-width width",
    "Sets pen width (0 <= width)" },
{
    "bg",			/* Background color */
    opt_bg,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-bg color",
    "Background color (0=black, FFFFFF=white)" },
{
    "ncol0",			/* Allocated colors in cmap 0 */
    opt_ncol0,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-ncol0 n",
    "Number of colors to allocate in cmap 0 (upper bound)" },
{
    "ncol1",			/* Allocated colors in cmap 1 */
    opt_ncol1,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-ncol1 n",
    "Number of colors to allocate in cmap 1 (upper bound)" },
{
    "fam",			/* Familying on switch */
    opt_fam,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-fam",
    "Create a family of output files" },
{
    "fsiz",			/* Family file size */
    opt_fsiz,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-fsiz size[kKmMgG]",
    "Output family file size (e.g. -fsiz 0.5G, def MB)" },
{
    "fbeg",			/* Family starting member */
    opt_fbeg,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-fbeg number",
    "First family member number on output" },
{
    "finc",			/* Family member increment */
    opt_finc,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-finc number",
    "Increment between family members" },
{
    "fflen",			/* Family member min field width */
    opt_fflen,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-fflen length",
    "Family member number minimum field width" },
{
    "nopixmap",			/* Do not use pixmaps */
    opt_nopixmap,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-nopixmap",
    "Don't use pixmaps in X-based drivers" },
{
    "db",			/* Double buffering on switch */
    opt_db,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-db",
    "Double buffer X window output" },
{
    "np",			/* Page pause off switch */
    opt_np,
    NULL,
    NULL,
    PL_OPT_FUNC,
    "-np",
    "No pause between pages" },
{
    "bufmax",			/* # bytes sent before flushing output */
    opt_bufmax,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-bufmax",
    "bytes sent before flushing output" },
{
    "server_name",		/* Main window name of server */
    opt_server_name,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-server_name name",
    "Main window name of PLplot server (tk driver)" },
{
    "server_host",		/* Host to run server on */
    opt_server_host,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-server_host name",
    "Host to run PLplot server on (dp driver)" },
{
    "server_port",		/* Port to talk to server on */
    opt_server_port,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-server_port name",
    "Port to talk to PLplot server on (dp driver)" },
{
    "user",			/* user name on remote node */
    opt_user,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-user name",
    "User name on remote node (dp driver)" },
{
    "plserver",			/* PLplot server name */
    opt_plserver,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-plserver name",
    "Invoked name of PLplot server (tk or dp driver)" },
{
    "plwindow",			/* PLplot container window name */
    opt_plwindow,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-plwindow name",
    "Name of PLplot container window (tk or dp driver)" },
{
    "tcl_cmd",			/* TCL initialization command */
    opt_tcl_cmd,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-tcl_cmd command",
    "TCL command string run at startup (note: disabled)" },
{
    "auto_path",		/* Additional directory(s) to autoload */
    opt_auto_path,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-auto_path dir",
    "Additional directory(s) to autoload (tk or dp driver)" },
{
    "tk_file",      /* -file option for plserver */
    opt_tk_file,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG | PL_OPT_INVISIBLE,
    "-tk_file file",
    "file for plserver (tk or dp driver)" },
{
    "dpi",			/* Dots per inch */
    opt_dpi,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-dpi dpi",
    "Resolution, in dots per inch (e.g. -dpi 360x360)" },
{
    "compression",			/* compression */
    opt_dev_compression,
    NULL,
    NULL,
    PL_OPT_FUNC | PL_OPT_ARG,
    "-compression num",
    "Sets compression level in supporting devices" },
{
    "drvopt",			/* Driver specific options */
    opt_drvopt,
    NULL,
    NULL,
    PL_OPT_ARG | PL_OPT_FUNC,
    "-drvopt option[=value][,option[=value]]*",
    "Driver specific options" },
{
    NULL,			/* option */
    NULL,			/* handler */
    NULL,			/* client data */
    NULL,			/* address of variable to set */
    0,				/* mode flag */
    NULL,			/* short syntax */
    NULL }			/* long syntax */
};

static char *plplot_notes[] = {
"All parameters must be white-space delimited.  Some options are driver",
"dependent.  Please see the PLplot reference document for more detail.",
NULL};

/*--------------------------------------------------------------------------*\
 * Array of option tables and associated info.
 *
 * The user may merge up to PL_MAX_OPT_TABLES custom option tables (of type
 * PLOptionTable) with the internal one.  The resulting treatment is simple,
 * powerful, and robust.  The tables are parsed in the order of last added
 * first, to the internal table last.  If multiple options of the same name
 * occur, only the first parsed is "seen", thus, the user can easily
 * override any PLplot internal option merely by providing the same option.
 * This same precedence is followed when printing help and usage messages,
 * with each set of options given separately.  See example usage in
 * plrender.c.
\*--------------------------------------------------------------------------*/

typedef struct {
    PLOptionTable *options;
    char *name;
    char **notes;
} PLOptionInfo;

PLOptionInfo ploption_info_default = {
    ploption_table,
    "PLplot options",
    plplot_notes
};

#define PL_MAX_OPT_TABLES 10
PLOptionInfo ploption_info[PL_MAX_OPT_TABLES] = { 
    {
	ploption_table,
	"PLplot options",
	plplot_notes
    }
};

/* The structure that hold the driver specific command line options */

typedef struct DrvOptCmd {
  char *option;
  char *value;
  struct DrvOptCmd *next;
} DrvOptCmd;

/* the variable where opt_drvopt() stores the driver specific command line options */
static DrvOptCmd drv_opt;
  
static int  tables = 1;

/*--------------------------------------------------------------------------*\
 * plSetOpt()
 *
 * Process input strings, treating them as an option and argument pair.
 * Returns 1 on an error.
\*--------------------------------------------------------------------------*/

int
c_plsetopt(char *opt, char *optarg)
{
    return(plSetOpt(opt, optarg));
}

int
plSetOpt(char *opt, char *optarg)
{
    int mode = 0, argc = 2, status;
    char *argv[3];

    argv[0] = opt;
    argv[1] = optarg;
    argv[2] = NULL;
    mode =
	PL_PARSE_QUIET |
	PL_PARSE_NODELETE |
	PL_PARSE_NOPROGRAM |
	PL_PARSE_NODASH;

    status = plParseOpts(&argc, argv, mode);
    if (status) {
	fprintf( stderr, "plSetOpt: Unrecognized option %s\n", opt);
    }
    return status;
}

/*--------------------------------------------------------------------------*\
 * plMergeOpts()
 *
 * Merge user option table info structure with internal one.
\*--------------------------------------------------------------------------*/

int
plMergeOpts(PLOptionTable *options, char *name, char **notes)
{
    PLOptionTable *tab;

    pllib_init();

/* Check to make sure option table has been terminated correctly */

    for (tab = options; tab->opt; tab++)
	;

/* We've reached the last table entry.  All the subentries must be NULL or 0 */

    if ((tab->handler     != NULL) ||
	(tab->client_data != NULL) ||
	(tab->var         != NULL) ||
	(tab->mode        != 0) ||
	(tab->syntax      != NULL) ||
	(tab->desc        != NULL)) {

	plabort("plMergeOpts: input table improperly terminated");
	return 1;
    }

/* No room for more tables */

    if (tables++ >= PL_MAX_OPT_TABLES) {
	plabort("plMergeOpts: max tables limit exceeded, table not merged");
	return 1;
    }

    ploption_info[tables-1].options = options;
    ploption_info[tables-1].name    = name;
    ploption_info[tables-1].notes   = notes;

    return 0;
}

/*--------------------------------------------------------------------------*\
 * plClearOpts()
 *
 * Clear internal option table info structure.
\*--------------------------------------------------------------------------*/

void
plClearOpts(void)
{
    tables = 0;
}

/*--------------------------------------------------------------------------*\
 * plResetOpts()
 *
 * Reset internal option table info structure.
\*--------------------------------------------------------------------------*/

void
plResetOpts(void)
{
    ploption_info[0] = ploption_info_default;
    tables = 1;
}

/*--------------------------------------------------------------------------*\
 * plParseOpts()
 *
 * Process options list using current ploptions_info structure.
 * An error in parsing the argument list causes a program exit if
 * mode_full is set, otherwise the function returns with an error.
\*--------------------------------------------------------------------------*/

int
plParseOpts(int *p_argc, char **argv, PLINT mode)
{
    char **argsave, **argend;
    int	i, myargc, status = 0;

    pllib_init();

/* Initialize */

    mode_full      = mode & PL_PARSE_FULL;
    mode_quiet     = mode & PL_PARSE_QUIET;
    mode_nodelete  = mode & PL_PARSE_NODELETE;
    mode_showall   = mode & PL_PARSE_SHOWALL;
    mode_noprogram = mode & PL_PARSE_NOPROGRAM;
    mode_nodash    = mode & PL_PARSE_NODASH;
    mode_skip      = mode & PL_PARSE_SKIP;

    /* Initialize the driver specific option linked structure */
    drv_opt.option  = drv_opt.value  = NULL;
    drv_opt.next  = NULL;

    myargc = (*p_argc); 
    argend = argv + myargc;

/* If program name is first argument, save and advance */

    if ( ! mode_noprogram) {
	plsc->program = program = argv[0];
	--myargc; ++argv;
    }
    if (myargc == 0)
	return 0;

/* Process the command line */

    argsave = argv;
    for (; myargc > 0; --myargc, ++argv) {

    /* Allow for "holes" in argv list */

	if (*argv == NULL || *argv[0] == '\0')
	    continue;

    /* Loop over all options tables, starting with the last */

	for (i = tables-1; i >= 0; i--) {

	/* Check option table for option */

	    status = ParseOpt(&myargc, &argv, p_argc, &argsave,
			      ploption_info[i].options);

	    if ( ! status) break;
	}

    /* Handle error return as specified by the mode flag */

	if (status == -1) {

	/* No match.  Keep going if mode_skip is set, otherwise abort if
	   fully parsing, else return without error. */

	    if (mode_skip) {
		if ( ! mode_nodelete) 
		    *argsave++ = *argv;
		continue;
	    }
	    if ( ! mode_quiet && mode_full) {
		fprintf(stderr, "\nBad command line option \"%s\"\n", argv[0]);
		plOptUsage();
	    }
	    if (mode_full) exit(1);

	    status = 0;
	    break;

	} else if (status == 1) {

	/* Illegal or badly formed */

	    if ( ! mode_quiet) {
		fprintf(stderr, "\nBad command line option \"%s\"\n", argv[0]);
		plOptUsage();
	    }
	    if (mode_full) exit(1);

	    break;

	} else if (status == 2) {

	/* Informational option encountered (-h or -v) */

	    exit(0);
	}
    }

/* Compress and NULL-terminate argv */

    if ( ! mode_nodelete) {
	for (i = 0; i < myargc; i++)
	    *argsave++ = *argv++;

	if (argsave < argend)
	    *argsave = NULL;
    }

    return status;
}

/*--------------------------------------------------------------------------*\
 * ParseOpt()
 *
 * Parses & determines appropriate action for input flag.
\*--------------------------------------------------------------------------*/

static int
ParseOpt(int *p_myargc, char ***p_argv, int *p_argc, char ***p_argsave,
	 PLOptionTable *option_table)
{
    PLOptionTable *tab;
    char *opt;

/* Only handle actual flags and their arguments */

    if ( mode_nodash || (*p_argv)[0][0] == '-') {

	opt = (*p_argv)[0];
	if (*opt == '-') 
	    opt++;

	for (tab = option_table; tab->opt; tab++) {

	/* Skip if option not enabled */

	    if (tab->mode & PL_OPT_DISABLED) 
		continue;

	/* Try to match it */

	    if (*opt == *tab->opt && ! strcmp(opt, tab->opt)) {

	    /* Option matched, so remove from argv list if applicable. */

		if ( ! mode_nodelete) {
		    if (tab->mode & PL_OPT_NODELETE)
			(*(*p_argsave)++) = (**p_argv);
		    else
			--(*p_argc);
		}

	    /* Process option (and argument if applicable) */

		return (ProcessOpt(opt, tab, p_myargc, p_argv, p_argc));
	    }
	}
    }

    return -1;
}

/*--------------------------------------------------------------------------*\
 * ProcessOpt()
 *
 * Process option (and argument if applicable).
\*--------------------------------------------------------------------------*/

static int
ProcessOpt(char *opt, PLOptionTable *tab, int *p_myargc, char ***p_argv,
	   int *p_argc)
{
    int need_arg, res;
    char *optarg = NULL;

/* Get option argument if necessary */

    need_arg = PL_OPT_ARG | PL_OPT_INT | PL_OPT_FLOAT | PL_OPT_STRING;

    if (tab->mode & need_arg) {
	if (GetOptarg(&optarg, p_myargc, p_argv, p_argc))
	    return 1;
    }

/* Process argument */

    switch (tab->mode & 0xFF00) {

    case PL_OPT_FUNC:

    /* Call function handler to do the job */

	if (tab->handler == NULL) {
	    fprintf(stderr,
		    "ProcessOpt: no handler specified for option %s\n",
		    tab->opt);
	    return 1;
	}

        if (mode_nodelete && optarg) {

	/* Make a copy, since handler may mung optarg with strtok() */
	    char *copy = 
	      (char *) malloc((size_t)(1+strlen(optarg))*sizeof(char));
	    if (copy == NULL) {
	        plabort("ProcessOpt: out of memory");
		return 1;
	    }
	    strcpy(copy, optarg);
	    res = ((*tab->handler) (opt, copy, tab->client_data));
	    free((void *) copy);
	    return res;
	}
	else {
	  return ((*tab->handler) (opt, optarg, tab->client_data));
	}

    case PL_OPT_BOOL:

    /* Set *var as a boolean */

	if (tab->var == NULL) {
	    fprintf(stderr,
		    "ProcessOpt: no variable specified for option %s\n",
		    tab->opt);
	    return 1;
	}
	*(int *)tab->var = 1;
	break;

    case PL_OPT_INT:

    /* Set *var as an int */

	if (tab->var == NULL) {
	    fprintf(stderr,
		    "ProcessOpt: no variable specified for option %s\n",
		    tab->opt);
	    return 1;
	}
	*(int *)tab->var = atoi(optarg);
	break;

    case PL_OPT_FLOAT:

    /* Set *var as a float */

	if (tab->var == NULL) {
	    fprintf(stderr,
		    "ProcessOpt: no variable specified for option %s\n",
		    tab->opt);
	    return 1;
	}
	*(PLFLT *)tab->var = atof(optarg);
	break;

    case PL_OPT_STRING:

    /* Set var (can be NULL initially) to point to optarg string */

	*(char **)tab->var = (char *)optarg;
	break;

    default:

    /* Somebody messed up.. */

	fprintf(stderr,
		"ProcessOpt: invalid processing mode for option %s\n",
		tab->opt);
	return 1;
    }
    return 0;
}

/*--------------------------------------------------------------------------*\
 * GetOptarg()
 *
 * Retrieves an option argument.
 * If an error occurs here it is a true syntax error.
\*--------------------------------------------------------------------------*/

static int
GetOptarg(char **poptarg, int *p_myargc, char ***p_argv, int *p_argc)
{
    int result = 0;

    --(*p_myargc);

    if ((*p_myargc) <= 0)		/* oops, no more arguments */
	result = 1;

    if ( ! result) {
	(*p_argv)++;
	if ((*p_argv)[0][0] == '-' && isalpha((*p_argv)[0][1])) {

	    (*p_argv)--;		/* oops, next arg is a flag */
	    result = 1;
	}
    }

    if ( ! result) {			/* yeah, the user got it right */
	(*p_argc)--;
	*poptarg = (*p_argv)[0];
    }
    else {
	if ( ! mode_quiet) {
	    fprintf(stderr, "Argument missing for %s option.\n", (*p_argv)[0]);
	    plOptUsage();
	}
    }
    return result;
}

/*--------------------------------------------------------------------------*\
 * plSetUsage()
 *
 * Set the strings used in usage and syntax messages.
\*--------------------------------------------------------------------------*/

void
plSetUsage(char *program_string, char *usage_string)
{
    if (program_string != NULL)
	program = program_string;

    if (usage_string != NULL)
	usage = usage_string;
}

/*--------------------------------------------------------------------------*\
 * plOptUsage()
 *
 * Print usage & syntax message.
\*--------------------------------------------------------------------------*/

void
plOptUsage(void)
{
    if (usage == NULL)
	fprintf(stderr, "\nUsage:\n        %s [options]\n", program);
    else
	fputs(usage, stderr);

    Syntax();

    fprintf(stderr, "\n\nType %s -h for a full description.\n\n",
	    program);
}

/*--------------------------------------------------------------------------*\
 * Syntax()
 *
 * Print short syntax message.
\*--------------------------------------------------------------------------*/

static void
Syntax(void)
{
    PLOptionTable *tab;
    int i, col, len;

/* Loop over all options tables */

    for (i = tables-1; i >= 0; i--) {

    /* Introducer */

	if (ploption_info[i].name)
	    fprintf(stderr, "\n%s:", ploption_info[i].name);
	else
	    fputs("\nUser options:", stderr);

    /* Print syntax for each option */

	col = 80;
	for (tab = ploption_info[i].options; tab->opt; tab++) {
	    if (tab->mode & PL_OPT_DISABLED)
		continue;

	    if ( ! mode_showall && (tab->mode & PL_OPT_INVISIBLE))
		continue;

	    if (tab->syntax == NULL)
		continue;

	    len = 3 + strlen(tab->syntax);		/* space [ string ] */
	    if (col + len > 79) {
		fprintf(stderr, "\n   ");		/* 3 spaces */
		col = 3;
	    }
	    fprintf(stderr, " [%s]", tab->syntax);
	    col += len;
	}
	fprintf(stderr, "\n");
    }
}

/*--------------------------------------------------------------------------*\
 * Help()
 *
 * Print long help message.
\*--------------------------------------------------------------------------*/

static void
Help(void)
{
    PLOptionTable *tab;
    char **note;
    int i;
    FILE *outfile = stderr;

#ifdef HAVE_POPEN
    FILE *pager = NULL;
    if (getenv("PAGER") != NULL)
	pager = (FILE *) popen("$PAGER", "w");
    if (pager == NULL)
	pager = (FILE *) popen("more", "w");
    if (pager != NULL)
	outfile = pager;
#endif

/* Usage line */

    if (usage == NULL)
	fprintf(outfile, "\nUsage:\n        %s [options]\n", program);
    else
	fputs(usage, outfile);

/* Loop over all options tables */

    for (i = tables-1; i >= 0; i--) {

    /* Introducer */

	if (ploption_info[i].name)
	    fprintf(outfile, "\n%s:\n", ploption_info[i].name);
	else
	    fputs("\nUser options:\n", outfile);

    /* Print description for each option */

	for (tab = ploption_info[i].options; tab->opt; tab++) {
	    if (tab->mode & PL_OPT_DISABLED)
		continue;

	    if ( ! mode_showall && (tab->mode & PL_OPT_INVISIBLE))
		continue;

	    if (tab->desc == NULL)
		continue;

	    if (tab->mode & PL_OPT_INVISIBLE) 
		fprintf(outfile, " *  %-20s %s\n", tab->syntax, tab->desc);
	    else 
		fprintf(outfile, "    %-20s %s\n", tab->syntax, tab->desc);
	}

    /* Usage notes */

	if (ploption_info[i].notes) {
	    putc('\n', outfile);
	    for (note = ploption_info[i].notes; *note; note++) {
		fputs(*note, outfile);
		putc('\n', outfile);
	    }
	}
    }

#ifdef HAVE_POPEN
    if (pager != NULL)
	pclose(pager);
#endif
}

/*--------------------------------------------------------------------------*\
 * plParseDrvOpts
 * 
 * Parse driver specific options
\*--------------------------------------------------------------------------*/

int
plParseDrvOpts(DrvOpt *acc_opt) {
  DrvOptCmd *drvp;
  DrvOpt *t;
  int fl;
  char msg[80];

  if (!drv_opt.option)
    return 1;

  drvp = &drv_opt;
  do {
    t = acc_opt; fl = 0;
    while (t->opt) {
      if (strcmp(drvp->option, t->opt) == 0) {
	fl = 1;
	switch (t->type) {

	case DRV_STR:
	  *(char **)(t->var_ptr) = (drvp->value);
#ifdef DEBUG
	  fprintf(stderr,"plParseDrvOpts: %s %s\n", t->opt, *(char**)t->var_ptr);
#endif
	  break;

	case DRV_INT:
	  if (sscanf(drvp->value, "%d", (int *)t->var_ptr) != 1) {
	    sprintf(msg,"Incorrect argument to '%s' option", drvp->option);
	    plexit(msg);
	  }
#ifdef DEBUG
	  fprintf(stderr,"plParseDrvOpts: %s %d\n", t->opt, *(int *) t->var_ptr);
#endif  
	  break;

	case DRV_FLT:
	  if (sscanf(drvp->value, "%f", (float *)t->var_ptr) != 1) {
	    sprintf(msg,"Incorrect argument to '%s' option", drvp->option);
	    plexit(msg);
	  }
#ifdef DEBUG
	  fprintf(stderr,"plParseDrvOpts: %s %f\n", t->opt, *(float *) t->var_ptr);
#endif  
	  break;
	}
      }
    t++;
    }

    if (!fl) {
      sprintf(msg, "Option '%s' not recognized.\n\nRecognized options for this driver are:\n", drvp->option);
      plwarn(msg);
      plHelpDrvOpts(acc_opt);      
      plexit(""); 
    }
  }
  while((drvp = drvp->next))
      ;

  return 0;
}

/*--------------------------------------------------------------------------*\
 * plHelpDrvOpts
 * 
 * Give driver specific help
\*--------------------------------------------------------------------------*/

void
plHelpDrvOpts(DrvOpt *acc_opt) {
  DrvOpt *t;

  t = acc_opt;
  while(t->opt) {
    fprintf(stderr, "%s:\t%s\n", t->opt, t->hlp_msg);
    t++;
  }
}

/*--------------------------------------------------------------------------*\
 * Option handlers
\*--------------------------------------------------------------------------*/

/*--------------------------------------------------------------------------*\
 * opt_h()
 *
 * Performs appropriate action for option "h":
 * Issues help message
\*--------------------------------------------------------------------------*/

static int
opt_h(char *opt, char *optarg, void *client_data)
{
    if ( ! mode_quiet)
	Help();

    return 2;
}

/*--------------------------------------------------------------------------*\
 * opt_v()
 *
 * Performs appropriate action for option "v":
 * Issues version message
\*--------------------------------------------------------------------------*/

static int
opt_v(char *opt, char *optarg, void *client_data)
{
    if ( ! mode_quiet) 
	fprintf(stderr, "PLplot library version: %s\n", VERSION);

    return 2;
}

/*--------------------------------------------------------------------------*\
 * opt_verbose()
 *
 * Performs appropriate action for option "verbose":
 * Turn on verbosity flag
\*--------------------------------------------------------------------------*/

static int
opt_verbose(char *opt, char *optarg, void *client_data)
{
    plsc->verbose = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_debug()
 *
 * Performs appropriate action for option "debug":
 * Turn on debugging flag
\*--------------------------------------------------------------------------*/

static int
opt_debug(char *opt, char *optarg, void *client_data)
{
    plsc->debug = 1;
    plsc->verbose = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_hack()
 *
 * Performs appropriate action for option "hack":
 * Enables driver-specific hack(s)
\*--------------------------------------------------------------------------*/

static int
opt_hack(char *opt, char *optarg, void *client_data)
{
    plsc->hack = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_dev()
 *
 * Performs appropriate action for option "dev":
 * Sets output device keyword
\*--------------------------------------------------------------------------*/

static int
opt_dev(char *opt, char *optarg, void *client_data)
{
    plsdev(optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_o()
 *
 * Performs appropriate action for option "o":
 * Sets output file name
\*--------------------------------------------------------------------------*/

static int
opt_o(char *opt, char *optarg, void *client_data)
{
    plsfnam(optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_mar()
 *
 * Performs appropriate action for option "mar":
 * Sets relative margin width
\*--------------------------------------------------------------------------*/

static int
opt_mar(char *opt, char *optarg, void *client_data)
{
    plsdidev(atof(optarg), PL_NOTSET, PL_NOTSET, PL_NOTSET);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_a()
 *
 * Performs appropriate action for option "a":
 * Sets plot aspect ratio on page
\*--------------------------------------------------------------------------*/

static int
opt_a(char *opt, char *optarg, void *client_data)
{
    plsdidev(PL_NOTSET, atof(optarg), PL_NOTSET, PL_NOTSET);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_jx()
 *
 * Performs appropriate action for option "jx":
 * Sets relative justification in x
\*--------------------------------------------------------------------------*/

static int
opt_jx(char *opt, char *optarg, void *client_data)
{
    plsdidev(PL_NOTSET, PL_NOTSET, atof(optarg), PL_NOTSET);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_jy()
 *
 * Performs appropriate action for option "jy":
 * Sets relative justification in y
\*--------------------------------------------------------------------------*/

static int
opt_jy(char *opt, char *optarg, void *client_data)
{
    plsdidev(PL_NOTSET, PL_NOTSET, PL_NOTSET, atof(optarg));
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_ori()
 *
 * Performs appropriate action for option "ori":
 * Sets orientation
\*--------------------------------------------------------------------------*/

static int
opt_ori(char *opt, char *optarg, void *client_data)
{
    plsdiori(atof(optarg));
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_freeaspect()
 *
 * Performs appropriate action for option "freeaspect":
 * Allow aspect ratio to adjust to orientation swaps.
\*--------------------------------------------------------------------------*/

static int
opt_freeaspect(char *opt, char *optarg, void *client_data)
{
    plsc->freeaspect = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_portrait()
 *
 * Performs appropriate action for option "portrait":
 * Set portrait mode.  If plsc->portrait = 1, then the orientation for certain 
 * drivers is changed by 90 deg to portrait orientation from the default
 * landscape orientation used by PLplot while the  aspect ratio allowed to
 * adjust using freeaspect.
 * N.B. the driver list where this flag is honored is currently limited
 * to ljii, ljiip, psc, ps, and pstex.  A 90 deg rotation is just not
 * appropriate for certain other drivers.  These drivers where portrait
 * mode is ignored include display drivers (e.g., xwin, tk), drivers 
 * which are subequently going to be transformed to another form 
 * (e.g., meta or pbm), or drivers which are normally used for web 
 * publishing (e.g., png, jpeg).  That said, the case is not entirely clear
 * for all drivers so the list of drivers where portrait mode is honored
 * may increase in the future. To add to the list simply copy the small
 * bit of code from  ps.c that has to do with pls->portrait to the 
 * appropriate driver file.
\*--------------------------------------------------------------------------*/

static int
opt_portrait(char *opt, char *optarg, void *client_data)
{
    plsc->portrait = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_width()
 *
 * Performs appropriate action for option "width":
 * Sets pen width
\*--------------------------------------------------------------------------*/

static int
opt_width(char *opt, char *optarg, void *client_data)
{
    int width;

    width = atoi(optarg);
    if (width < 0) {
	fprintf(stderr, "?invalid width\n");
	return 1;
    }
    else {
	plwid(width);
	plsc->widthlock = 1;
    }
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_bg()
 *
 * Performs appropriate action for option "bg":
 * Sets background color
\*--------------------------------------------------------------------------*/

static int
opt_bg(char *opt, char *optarg, void *client_data)
{
    char *rgb;
    long bgcolor, r, g, b;

/* Always in hex!  Strip off leading "#" (TK-ism) if present. */

    if (*optarg == '#')
	rgb = optarg + 1;
    else
	rgb = optarg;

/* Get number in hex */

    bgcolor = strtol(rgb, NULL, 16);

/* Must be either a 3 or 6 digit hex number */
/* If 3 digits, each is "doubled" (i.e. ABC becomes AABBCC). */

    switch (strlen(rgb)) {
    case 3:
	r = (bgcolor & 0xF00) >> 8;
	g = (bgcolor & 0x0F0) >> 4;
	b = (bgcolor & 0x00F);

	r = r | (r << 4);
	g = g | (g << 4);	/* doubling */
	b = b | (b << 4);
	break;

    case 6:
	r = (bgcolor & 0xFF0000) >> 16;
	g = (bgcolor & 0x00FF00) >> 8;
	b = (bgcolor & 0x0000FF);
	break;

    default:
	fprintf(stderr, "Unrecognized background color value %s\n", rgb);
	return 1;
    }

    plscolbg(r, g, b);

    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_ncol0()
 *
 * Performs appropriate action for option "ncol0":
 * Sets number of colors to allocate in cmap 0 (upper bound).
\*--------------------------------------------------------------------------*/

static int
opt_ncol0(char *opt, char *optarg, void *client_data)
{
    plsc->ncol0 = atoi(optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_ncol1()
 *
 * Performs appropriate action for option "ncol1":
 * Sets number of colors to allocate in cmap 1 (upper bound).
\*--------------------------------------------------------------------------*/

static int
opt_ncol1(char *opt, char *optarg, void *client_data)
{
    plsc->ncol1 = atoi(optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_wplt()
 *
 * Performs appropriate action for option "wplt":
 * Sets (zoom) window into plot (e.g. "0,0,0.5,0.5")
\*--------------------------------------------------------------------------*/

static int
opt_wplt(char *opt, char *optarg, void *client_data)
{
    char *field;
    PLFLT xl, yl, xr, yr;

    strncpy(opttmp, optarg, OPTMAX-1);

    if ((field = strtok(opttmp, ",")) == NULL)
	return 1;

    xl = atof(field);

    if ((field = strtok(NULL, ",")) == NULL)
	return 1;

    yl = atof(field);

    if ((field = strtok(NULL, ",")) == NULL)
	return 1;

    xr = atof(field);

    if ((field = strtok(NULL, ",")) == NULL)
	return 1;

    yr = atof(field);

    plsdiplt(xl, yl, xr, yr);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_drvopt()
 *
 * Get driver specific options in the form <option[=value]>[,option[=value]]*
 * If "value" is not specified, it defaults to "1".
\*--------------------------------------------------------------------------*/

static int
opt_drvopt(char *opt, char *optarg, void *client_data)
{
  char t, *tt, *option, *value;
  int fl = 0;
  DrvOptCmd *drvp;

  option = (char *) malloc((size_t)(1+strlen(optarg))*sizeof(char));
  if (option == NULL)
    plexit("opt_drvopt: Out of memory!?");

  value = (char *) malloc((size_t)(1+1)*sizeof(char));
  if (value == NULL)
    plexit("opt_drvopt: Out of memory!?");

  drvp = &drv_opt;
  *option = *value = '\0';
  tt = option;
    while((t = *optarg++)) {
      switch (t) {
      case ',':
	if (fl)
	  fl = 0;
	else {
	  value[0] = '1';
	  value[1] = '\0';
	}
	
	*tt = '\0'; tt = option;
	drvp->option = plstrdup(option); /* it should not be release, because of familying */
	drvp->value = plstrdup(value); /* don't release */
	drvp->next = (DrvOptCmd *) malloc(sizeof(DrvOptCmd)); /* don't release */
	if (drvp->next == NULL)
	  plexit("opt_drvopt: Out of memory!?\n");

	drvp = drvp->next;
	break;

      case '=':
	fl = 1;
	*tt = '\0'; tt = value;
	break;

      default:
	*tt++ = t;
      }
    }

    *tt = '\0';
    if (!fl) {
      value[0] = '1';
      value[1] = '\0';
    }

    drvp->option = plstrdup(option); /* don't release */
    drvp->value = plstrdup(value); /* don't release */
    drvp->next = NULL;

#ifdef DEBUG
    fprintf(stderr, "\nopt_drvopt: -drvopt parsed options:\n");
    drvp = &drv_opt;
    do 
      fprintf(stderr, "%s %s\n", drvp->option, drvp->value);
    while(drvp = drvp->next);
    fprintf(stderr, "\n");
#endif

    free(option); free(value);

    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_fam()
 *
 * Performs appropriate action for option "fam":
 * Enables family output files
\*--------------------------------------------------------------------------*/

static int
opt_fam(char *opt, char *optarg, void *client_data)
{
    plsfam(1, -1, -1);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_fsiz()
 *
 * Performs appropriate action for option "fsiz":
 * Sets size of a family member file (may be somewhat larger since eof must
 * occur at a page break).  Also turns on familying.  Example usage:
 *
 *	-fsiz 5M	(5 MB)
 *	-fsiz 300K	(300 KB)
 *	-fsiz .3M	(same)
 *	-fsiz .5G	(half a GB)
 *
 * Note case of the trailing suffix doesn't matter.
 * If no suffix, defaults to MB.
\*--------------------------------------------------------------------------*/

static int
opt_fsiz(char *opt, char *optarg, void *client_data)
{
    PLINT bytemax;
    int len = strlen(optarg);
    char lastchar = optarg[len-1];
    PLFLT multiplier = 1.0e6;
    char *spec = (char*)malloc(len+1);

/* Interpret optional suffix */

    switch (lastchar) {
    case 'k':
    case 'K':
	multiplier = 1.0e3; len--;
	break;
    case 'm':
    case 'M':
	multiplier = 1.0e6; len--;
	break;
    case 'g':
    case 'G':
	multiplier = 1.0e9; len--;
	break;
    }
    strncpy(spec, optarg, len);
    spec[len] = '\0';

    bytemax = multiplier * atof(spec);
    if (bytemax == 0) {
	fprintf(stderr, "?invalid bytemax\n");
	return 1;
    }
    plsfam(1, -1, bytemax);

    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_fbeg()
 *
 * Performs appropriate action for option "fbeg":
 * Starts with the specified family member number.
\*--------------------------------------------------------------------------*/

static int
opt_fbeg(char *opt, char *optarg, void *client_data)
{
    plsc->member = atoi(optarg);

    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_finc()
 *
 * Performs appropriate action for option "finc":
 * Specify increment between family members.
\*--------------------------------------------------------------------------*/

static int
opt_finc(char *opt, char *optarg, void *client_data)
{
    plsc->finc = atoi(optarg);

    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_fflen()
 *
 * Performs appropriate action for option "fflen":
 * Specify minimum field length for family member number.
\*--------------------------------------------------------------------------*/

static int
opt_fflen(char *opt, char *optarg, void *client_data)
{
    plsc->fflen = atoi(optarg);

    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_np()
 *
 * Performs appropriate action for option "np":
 * Disables pause between pages
\*--------------------------------------------------------------------------*/

static int
opt_np(char *opt, char *optarg, void *client_data)
{
    plspause(0);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_nopixmap()
 *
 * Performs appropriate action for option "nopixmap":
 * Disables use of pixmaps in X drivers
\*--------------------------------------------------------------------------*/

static int
opt_nopixmap(char *opt, char *optarg, void *client_data)
{
    plsc->nopixmap = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_db()
 *
 * Performs appropriate action for option "db":
 * Double buffer X output (update only done on eop or Expose)
\*--------------------------------------------------------------------------*/

static int
opt_db(char *opt, char *optarg, void *client_data)
{
    plsc->db = 1;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_bufmax()
 *
 * Performs appropriate action for option "bufmax":
 * Sets size of data buffer for tk driver
\*--------------------------------------------------------------------------*/

static int
opt_bufmax(char *opt, char *optarg, void *client_data)
{
    plsc->bufmax = atoi(optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_server_name()
 *
 * Performs appropriate action for option "server_name":
 * Sets main window name of server (Tcl/TK/DP driver only)
\*--------------------------------------------------------------------------*/

static int
opt_server_name(char *opt, char *optarg, void *client_data)
{
    plsc->server_name = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_server_host()
 *
 * Performs appropriate action for option "server_host":
 * Sets host to run server on (Tcl/TK/DP driver only)
\*--------------------------------------------------------------------------*/

static int
opt_server_host(char *opt, char *optarg, void *client_data)
{
    plsc->server_host = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_server_port()
 *
 * Performs appropriate action for option "server_port":
 * Sets port to talk to server on (Tcl/TK/DP driver only)
\*--------------------------------------------------------------------------*/

static int
opt_server_port(char *opt, char *optarg, void *client_data)
{
    plsc->server_port = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_user()
 *
 * Performs appropriate action for option "user":
 * Sets user name on remote node (for remsh), dp driver only
\*--------------------------------------------------------------------------*/

static int
opt_user(char *opt, char *optarg, void *client_data)
{
    plsc->user = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_plserver()
 *
 * Performs appropriate action for option "plserver":
 * Sets name to use when invoking server (Tcl/TK/DP driver only)
\*--------------------------------------------------------------------------*/

static int
opt_plserver(char *opt, char *optarg, void *client_data)
{
    plsc->plserver = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_plwindow()
 *
 * Performs appropriate action for option "plwindow":
 * Sets PLplot window name
\*--------------------------------------------------------------------------*/

static int
opt_plwindow(char *opt, char *optarg, void *client_data)
{
    plsc->plwindow = (char *) malloc((size_t)(1+strlen(optarg))*sizeof(char));
    strcpy (plsc->plwindow, optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_tcl_cmd()
 *
 * Performs appropriate action for option "tcl_cmd":
 * Sets TCL command(s) to eval on startup
\*--------------------------------------------------------------------------*/

static int
opt_tcl_cmd(char *opt, char *optarg, void *client_data)
{
    plsc->tcl_cmd = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_auto_path()
 *
 * Performs appropriate action for option "auto_path":
 * Sets additional directories to autoload
\*--------------------------------------------------------------------------*/

static int
opt_auto_path(char *opt, char *optarg, void *client_data)
{
    plsc->auto_path = optarg;
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_px()
 *
 * Performs appropriate action for option "px":
 * Set packing in x
\*--------------------------------------------------------------------------*/

static int
opt_px(char *opt, char *optarg, void *client_data)
{
    plssub(atoi(optarg), -1);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_py()
 *
 * Performs appropriate action for option "py":
 * Set packing in y
\*--------------------------------------------------------------------------*/

static int
opt_py(char *opt, char *optarg, void *client_data)
{
    plssub(-1, atoi(optarg));
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_geo()
 *
 * Performs appropriate action for option "geo":
 * Set geometry for output window
 *   e.g.,  "-geometry 400x400+100+0"
 * or with offsets alone "-geometry +Xoff+Yoff"
 *   e.g., "-geometry +100+0"
\*--------------------------------------------------------------------------*/

static int
opt_geo(char *opt, char *optarg, void *client_data)
{
    char *field;
    PLFLT xdpi = 0., ydpi = 0.;
    PLINT xwid = 0, ywid = 0, xoff = 0, yoff = 0;
    
/* The TK driver uses the geometry string directly */    

    plsc->geometry = (char *) malloc((size_t)(1+strlen(optarg))*sizeof(char));
    strcpy (plsc->geometry, optarg);

/* Set up plplot dimensions */

    strncpy(opttmp, optarg, OPTMAX-1);
    if (strchr (opttmp, 'x')) {

    /* -geometry WxH or -geometry WxH+Xoff+Yoff */

	field = strtok (opttmp, "x");
	xwid = atoi (field);
	if (xwid == 0)
	    fprintf (stderr, "?invalid xwid\n");

	if ((field = strtok (NULL, "+")) == NULL)
	    return 1;

	ywid = atoi (field);
	if (ywid == 0)
	    fprintf (stderr, "?invalid ywid\n");

	field = strtok (NULL, "+");
    }
    else {

    /* -geometry +Xoff or -geometry +Xoff+Yoff only */

	field = strtok (opttmp, "+");
    }

    if (field != NULL) {
	xoff = atoi (field);
	if ((field = strtok (NULL, "+")) != NULL)
	    yoff = atoi (field);
    }

    plspage (xdpi, ydpi, xwid, ywid, xoff, yoff);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_tk_file()
 *
 * File name for plserver tk_file option
\*--------------------------------------------------------------------------*/
   
static int
opt_tk_file(char *opt, char *optarg, void *client_data)
{
    plsc->tk_file = (char *) malloc((size_t)(1+strlen(optarg))*sizeof(char));
    strcpy (plsc->tk_file, optarg);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_dpi()
 *
 * Performs appropriate action for option "dpi":
 * Set dpi resolution for output device
 *   e.g.,  "-dpi 600x300", will set X dpi to 600 and Y dpi to 300
 * 		or 
 *   e.g., "-dpi 1200"
 * Will set both X and Y dpi to 1200 dpi
\*--------------------------------------------------------------------------*/

static int
opt_dpi(char *opt, char *optarg, void *client_data)
{
    char *field;
    PLFLT xdpi = 0., ydpi = 0.;
    PLINT xwid = 0, ywid = 0, xoff = 0, yoff = 0;
    
    strncpy(opttmp, optarg, OPTMAX-1);
    if (strchr (opttmp, 'x')) {
	field = strtok (opttmp, "x");
	xdpi = atof (field);
	if (xdpi == 0)
	    fprintf (stderr, "?invalid xdpi\n");

	if ((field = strtok (NULL, " ")) == NULL)
	   return 1;
	   
        ydpi = atof (field);
        if (ydpi == 0)
	   fprintf (stderr, "?invalid ydpi\n");

    } else {
	xdpi = atof (opttmp);
	ydpi=xdpi;
	if (xdpi==0) return 1;
    }

    plspage (xdpi, ydpi, xwid, ywid, xoff, yoff);
    return 0;
}

/*--------------------------------------------------------------------------*\
 * opt_dev_compression()
 *
 * Sets device compression
\*--------------------------------------------------------------------------*/

static int
opt_dev_compression(char *opt, char *optarg, void *client_data)
{
    PLINT comp = 0;
   
    comp = atoi(optarg);
    if (comp == 0) {
	fprintf(stderr, "?invalid compression\n");
	return 1;
    }
    plscompression (comp);

    return 0;
}
