/* $Id: plplotP.h,v 1.1 2004/03/01 20:54:52 cozmic Exp $

    Copyright (C) 1993, 1994, 1995  by 
    Maurice J. LeBrun, Geoff Furnish, Tony Richardson.

    Internal (private) macros and prototypes for the PLplot package.  This
    header file must be included before all others, including system header
    files.  This file is typically needed when including driver specific
    header files (e.g. pltkd.h).

    This software may be freely copied, modified and redistributed without
    fee provided that this copyright notice is preserved intact on all
    copies and modified copies. 
 
    There is no warranty or other guarantee of fitness of this software.
    It is provided solely "as is". The author(s) disclaim(s) all
    responsibility and liability with respect to this software's usage or
    its effect upon hardware or computer systems. 
*/

#ifndef __PLPLOTP_H__
#define __PLPLOTP_H__

/*--------------------------------------------------------------------------*\
 * Select environment.  Must be done before anything else.
 *
 * Basically we want the widest range of system services that are available.
 * Fortunately on many systems, that is the default.  To get "everything",
 * one of the following must be defined, as appropriate:
 *
 * _GNU_SOURCE     on Linux (default) 
 * _OSF_SOURCE     on OSF1 (default) 
 * _HPUX_SOURCE    on HP (not default) 
 * _ALL_SOURCE     on AIX (no idea) 
 *
 * To see where these are set, do the following:
 *
 *    cd /usr/include; grep SOURCE *.h | fgrep 'define '
 *
 * and the file containing lots of these is the one you want (features.h on
 * Linux, standards.h on OSF1, etc).  Follow the logic to see what needs to be
 * defined to get "everything", i.e. POSIX.*, XOPEN, etc.
 *
 * Note that for specific functionality, we test using autoconf.  Still it's
 * best to stick to ANSI C, POSIX.1, and POSIX.2, in that order, for maximum
 * portability.
\*--------------------------------------------------------------------------*/

/* HPUX - if this is no longer needed, please remove it */
#ifdef _HPUX
#define _HPUX_SOURCE
#endif

/* A/IX - if this is no longer needed, please remove it */
#ifdef _AIX
#define _ALL_SOURCE
#endif

/* Add others here as needed. */

/*--------------------------------------------------------------------------*\
 *	Configuration settings
 * 
 * Some of the macros set during configuration are described here.
 *
 * If HAVE_TERMIOS_H is set, we employ POSIX.1 tty terminal I/O.  One purpose
 * of this is to select character-oriented (CBREAK) i/o in the tek driver and
 * all its variants.  It is usable without this but not as powerful.  The
 * reason for using this is that some supported systems are still not
 * POSIX.1 compliant (and some may never be).
 *
 * If STDC_HEADERS is defined, the system's libc is ANSI-compliant.
 * ANSI libc calls are used for: (a) setting up handlers to be called
 * before program exit (via the "atexit" call), and (b) for seek
 * operations.  Again, the code is usable without these.  An ANSI libc
 * should be available, given the requirement of an ANSI compiler.  Some
 * reasons why not: (a) the vendor didn't supply a complete ANSI
 * environment, or (b) the ANSI libc calls are buggy, or (c) you ported
 * gcc to your system but not glibc (for whatever reason).  Note: without
 * an ANSI C lib, if you ^C out of a program using one of the PLplot tek
 * drivers, your terminal may be left in a strange state.
\*--------------------------------------------------------------------------*/

#include "plConfig.h"
#ifdef caddr_t
#undef caddr_t
#ifndef __USE_BSD
typedef char * caddr_t;
#endif
#endif

/* System headers */

#include <ctype.h>
#include <math.h>
#include <string.h>
#include <limits.h>
#include <float.h>

#if HAVE_DIRENT_H
# include <dirent.h>
# define NAMLEN(dirent) strlen((dirent)->d_name)
#else
# define dirent direct
# define NAMLEN(dirent) (dirent)->d_namlen
# if HAVE_SYS_NDIR_H
#  include <sys/ndir.h>
# endif
# if HAVE_SYS_DIR_H
#  include <sys/dir.h>
# endif
# if HAVE_NDIR_H
#  include <ndir.h>
# endif
#endif

/*
 * Macros for file positioning.  I tried switching to f[sg]etpos() because I
 * like the semantics better, but ran into the problem that fpos_t is not
 * always a base type (it may be a struct).  This is a problem because the
 * metafile driver needs to write relative offsets into the file itself.  So
 * instead we use f{seek,tell} at a low level but keep the f[sg]etpos
 * semantics using these macros. 
 */

#ifdef STDC_FPOS_T
#undef STDC_FPOS_T
#endif

#ifdef STDC_FPOS_T
#define FPOS_T fpos_t
#define pl_fsetpos(a,b) fsetpos(a, b)
#define pl_fgetpos(a,b) fgetpos(a, b)

#else
#define FPOS_T long
#define pl_fsetpos(a,b) fseek(a, *b, 0)
#define pl_fgetpos(a,b) (-1L == (*b = ftell(a)))
#endif

/* Include all externally-visible definitions and prototypes */
/* plplot.h also includes some handy system header files */

#include "plplot.h"

/* plstream definition */

#include "plstrm.h"

/* If not including this file from inside of plcore.h, declare plsc */

#ifndef __PLCORE_H__
#ifdef __cplusplus
extern "C" {
#endif
extern PLStream	*plsc;
#ifdef __cplusplus
}
#endif
#include "pldebug.h"
#endif

/*--------------------------------------------------------------------------*\
 *                       Utility macros
\*--------------------------------------------------------------------------*/

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

/* Used to help ensure everything malloc'ed gets freed */

#define free_mem(a) \
    if (a != NULL) { free((void *) a); a = NULL; }

/* Allows multi-argument setup calls to not affect selected arguments */

#define plsetvar(a, b) \
    if (b != PL_NOTSET) a = b;

/* Lots of cool math macros */

#ifndef MAX
#define MAX(a,b)    (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a,b)    (((a) < (b)) ? (a) : (b))
#endif
#ifndef ABS
#define ABS(a)      ((a)<0 ? -(a) : (a))
#endif
#ifndef ROUND
#define ROUND(a)    (PLINT)((a)<0. ? ((a)-.5) : ((a)+.5))
#endif
#ifndef BETW
#define BETW(ix,ia,ib)  (((ix)<=(ia)&&(ix)>=(ib)) || ((ix)>=(ia)&&(ix)<=(ib)))
#endif
#ifndef SSQR
#define SSQR(a,b)       sqrt((a)*(a)+(b)*(b))
#endif
#ifndef SIGN
#define SIGN(a)         ((a)<0 ? -1 : 1)
#endif

/* A coordinate value that should never occur */

#define PL_UNDEFINED -9999999

/*--------------------------------------------------------------------------*\
 *                       PLPLOT control macros
\*--------------------------------------------------------------------------*/

/* Some constants */

#define PL_MAXPOLY	256	/* Max segments in polyline or polygon */
#define PL_NSTREAMS	100	/* Max number of concurrent streams. */
#define PL_RGB_COLOR	1<<7	/* A hack */

#define TEXT_MODE	0
#define GRAPHICS_MODE	1
#ifndef PI
#define PI		3.1415926535897932384
#endif

/* These define the virtual coordinate system used by the metafile driver.
   Others are free to use it, or some variation, or define their own. */

/* Note desktop monitors of reasonable quality typically have 0.25 mm spacing 
 * between dots which corresponds to 4.0 dots per mm.  The parameters here
 * roughly correspond to a 14" monitor at 1024x768 resolution, which should
 * work fine at other sizes/resolutions.  The number of virtual dots per mm is
 * scaled by a factor of 32, with pixels scaled accordingly.  The customary
 * x/y ratio of 4:3 is used.
 */

#define PIXELS_X	32768		/* Number of virtual pixels in x */
#define PIXELS_Y	24576		/* Number of virtual pixels in x */
#define DPMM		4.		/* dots per mm */
#define VDPMM	      (DPMM*32)		/* virtual dots per mm */
#define LPAGE_X	    (PIXELS_X/VDPMM)	/* virtual page length in x in mm (256) */
#define LPAGE_Y	    (PIXELS_Y/VDPMM)	/* virtual page length in y in mm (192) */

/* This defines the first argument of the plRotPhy invocation that is made
 * in a number of device drivers (e.g., found in ljii.c, ljiip.c, ps.c,
 * and pstex.c) to rotate them "permanently" from portrait mode to non-
 * portrait mode.  ORIENTATION of 1 corresponds to seascape mode (90 deg
 * clockwise rotation from portrait).  This is the traditional value 
 * effectively used in all older versions of PLplot. ORIENTATION of 3 
 * corresponds to landscape mode (90 deg *counter*-clockwise rotation from 
 * portrait) which is the new default non-portrait orientation. */

#define ORIENTATION	3

/* Switches for state function call. */

#define PLSTATE_WIDTH		1	/* pen width */
#define PLSTATE_COLOR0		2	/* change to color in cmap 0 */
#define PLSTATE_COLOR1		3	/* change to color in cmap 1 */
#define PLSTATE_FILL		4	/* set area fill attribute */
#define PLSTATE_CMAP0		5	/* change to cmap 0 */
#define PLSTATE_CMAP1		6	/* change to cmap 1 */

/* Bit switches used in the driver interface */

#define PLDI_MAP	0x01
#define PLDI_ORI	0x02
#define PLDI_PLT	0x04
#define PLDI_DEV	0x08

/* Default size for family files, in KB. */

#ifndef PL_FILESIZE_KB
#define PL_FILESIZE_KB 1000
#endif

/* Font file names. */

#define PLPLOT5_FONTS

#ifdef PLPLOT5_FONTS
#define PL_XFONT	"plxtnd5.fnt"
#define PL_SFONT	"plstnd5.fnt"
#else
#define PL_XFONT	"plxtnd4.fnt"
#define PL_SFONT	"plstnd4.fnt"
#endif

/*--------------------------------------------------------------------------*\
 * The following environment variables are defined:
 * 
 *	PLPLOT_BIN      # where to find executables
 *	PLPLOT_LIB      # where to find library files (fonts, maps, etc)
 *	PLPLOT_TCL      # where to find tcl scripts
 *
 *	PLPLOT_HOME     # basename of plplot hierarchy
 *
 * search order:
 *	1)	the most specific possible locators, one of
 *			$(PLPLOT_BIN)
 *			$(PLPLOT_LIB)
 *			$(PLPLOT_TCL)
 *		as appropriate
 *
 *	2)	the current directory
 *
 *	3)	one of  $(PLPLOT_HOME)/bin
 *			$(PLPLOT_HOME)/lib
 *			$(PLPLOT_HOME)/tcl
 *		as appropriate
 *
 *	4)	as appropriate, the compile-time (Makefile)
 *		BIN_DIR, LIB_DIR, TCL_DIR
 *
 *  8 Jun 1994  mj olesen (olesen@weber.me.queensu.ca)
 *
 * Other notes:
 *
 * In addition to the directories above, the following are also used:
 *
 * Lib file search path: PLLIBDEV (see plctrl.c).  This is checked last,
 * and is a system-dependent hardwired location.
 *
 * Tcl search path: $HOME/tcl is searched before the install location,
 * TCL_DIR.
\*--------------------------------------------------------------------------*/

#define PLPLOT_BIN_ENV          "PLPLOT_BIN"
#define PLPLOT_LIB_ENV          "PLPLOT_LIB"
#define PLPLOT_TCL_ENV          "PLPLOT_TCL"
#define PLPLOT_HOME_ENV         "PLPLOT_HOME"

/*--------------------------------------------------------------------------*\
 *		Function Prototypes
 *
 * These typically should not be called directly by the user.
\*--------------------------------------------------------------------------*/

#ifdef __cplusplus
extern "C" {
#endif

/* Determines interval between numeric labels */

void
pldtik(PLFLT vmin, PLFLT vmax, PLFLT *tick, PLINT *nsubt);

/* Determines precision of box labels */

void
pldprec(PLFLT vmin, PLFLT vmax, PLFLT tick, PLINT lf, 
	PLINT *mode, PLINT *prec, PLINT digmax, PLINT *scale);

/* Draws a polyline within the clip limits. */

void
plP_pllclp(PLINT *x, PLINT *y, PLINT npts,
	   PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax, 
	   void (*draw) (short *, short *, PLINT));

/* Fills a polygon within the clip limits. */

void
plP_plfclp(PLINT *x, PLINT *y, PLINT npts,
	   PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax, 
	   void (*draw) (short *, short *, PLINT));

  /* Clip a polygon to the 3d bounding plane */
int
plP_clip_poly(int Ni, PLFLT *Vi[3], int axis, PLFLT dir, PLFLT offset);

/* Pattern fills in software the polygon bounded by the input points. */

void
plfill_soft(short *x, short *y, PLINT npts);

/* In case of an abort this routine is called.  It just prints out an */
/* error message and tries to clean up as much as possible. */

void
plexit(char *errormsg);

/* Just a front-end to exit().  */

void
pl_exit(void);

/* A handy way to issue warnings, if need be. */

void
plwarn(char *errormsg);

/* Same as plwarn(), but appends ", aborting plot" to the error message */

void
plabort(char *errormsg);

/* Loads either the standard or extended font. */

void
plfntld(PLINT fnt);

/* Release memory for fonts. */

void
plfontrel(void);

/* A replacement for strdup(), which isn't portable. */

char *
plstrdup(const char *src);

/* Bin up cmap 1 space and assign colors to make inverse mapping easy. */

void
plcmap1_calc(void);

/* Draws a slanting tick at position (mx,my) (measured in mm) of */
/* vector length (dx,dy). */

void 
plstik(PLFLT mx, PLFLT my, PLFLT dx, PLFLT dy);

/* Prints out a "string" at reference position with physical coordinates */
/* (refx,refy). */

void
plstr(PLINT base, PLFLT *xform, PLINT refx, PLINT refy, const char *string);

/* Draws a tick parallel to x. */

void
plxtik(PLINT x, PLINT y, PLINT below, PLINT above);

/* Draws a tick parallel to y. */

void
plytik(PLINT x, PLINT y, PLINT left, PLINT right);

  /* Driver interface filter -- 
     passes all coordinates through a variety of filters. */

void
difilt(PLINT *, PLINT *, PLINT,
       PLINT *, PLINT *, PLINT *, PLINT *);

  /* Driver draws text */

void
plP_text(PLINT base, PLFLT just, PLFLT *xform, PLINT x, PLINT y,
		 PLINT refx, PLINT refy, const char *string);

  /* where should structure definitions that must be seen by drivers and core source files, be? */

  /* structure to be used by plcore.c and anydriver.c, related to plP_text() */

typedef struct {
  PLINT base; /* ref point at base(1) or center(0) of text. Currently plplot only use 0 */
  PLFLT just; /* continuos justification, 0 left, 0.5 center, 1 right */
  PLFLT *xform; /* transformation (rotation) matrix */
  PLINT x; /* raw reference point--after any transformation */
  PLINT y;
  PLINT refx; /* processed ref. point--after justification, displacement, etc, processing */
  PLINT refy;
  const char *string; /* text to draw */
}EscText;

/* 
 * structure that contains driver specific information, to be used by plargs.c and anydriver.c,
 * related to plParseDrvOpts() and plHelpDrvOpts()
 */

typedef struct {
  char *opt;     /* a string with the name of the option */
  PLINT type;    /* the type of the variable to be set, see bellow the available types */
  void *var_ptr; /* a pointer to the variable to be set */
  char *hlp_msg; /* help message of the option */
} DrvOpt;

  /* the available variable types, DrvOpt.type, for driver specific options */

enum {DRV_INT, DRV_FLT, DRV_STR};

  /* parse driver specific options, as in -drvopt <option[=value]>* */

int
plParseDrvOpts(DrvOpt *);

  /* give help on driver specific options */

void
plHelpDrvOpts(DrvOpt *);

  /*
   * structures to store contour lines
   */

#define LINE_ITEMS 20

typedef struct cont_line {
  PLFLT *x;
  PLFLT *y;
  PLINT npts;
  struct cont_line *next;
} CONT_LINE;

typedef struct cont_level {
  PLFLT level;
  struct cont_line *line; /* contour line */
  struct cont_level *next; /* contour level */
} CONT_LEVEL;

void
cont_store(PLFLT *x, PLFLT *y, PLFLT **f, PLINT nx, PLINT ny, PLINT kx, PLINT lx,
	   PLINT ky, PLINT ly, PLFLT *clevel, PLINT nlevel, CONT_LEVEL **contour);
void
cont_clean_store(CONT_LEVEL *ct);

/* Get x-y domain in world coordinates for 3d plots */

void
plP_gdom(PLFLT *p_xmin, PLFLT *p_xmax, PLFLT *p_ymin, PLFLT *p_ymax);

/* Get vertical (z) scale parameters for 3-d plot */

void
plP_grange(PLFLT *p_zscl, PLFLT *p_zmin, PLFLT *p_zmax);

/* Get parameters used in 3d plots */

void
plP_gw3wc(PLFLT *p_dxx, PLFLT *p_dxy, PLFLT *p_dyx, PLFLT *p_dyy,
	  PLFLT *p_dyz);

/* Get clip boundaries in physical coordinates */

void
plP_gclp(PLINT *p_ixmin, PLINT *p_ixmax, PLINT *p_iymin, PLINT *p_iymax);

/* Set clip boundaries in physical coordinates */

void
plP_sclp(PLINT ixmin, PLINT ixmax, PLINT iymin, PLINT iymax);

/* Get physical device limits in physical coordinates */

void
plP_gphy(PLINT *p_ixmin, PLINT *p_ixmax, PLINT *p_iymin, PLINT *p_iymax);

/* Get number of subpages on physical device and current subpage */

void
plP_gsub(PLINT *p_nx, PLINT *p_ny, PLINT *p_cs);

/* Set number of subpages on physical device and current subpage */

void
plP_ssub(PLINT nx, PLINT ny, PLINT cs);

/* Set up plot parameters according to the number of subpages. */

void
plP_subpInit(void);

/* Get number of pixels to a millimeter */

void
plP_gpixmm(PLFLT *p_x, PLFLT *p_y);

/* All the drivers call this to set physical pixels/mm. */

void
plP_setpxl(PLFLT xpmm0, PLFLT ypmm0);

/* Get background parameters for 3d plot. */

void
plP_gzback(PLINT **zbf, PLINT **zbc, PLFLT **zbt);

/* Move to physical coordinates (x,y). */

void
plP_movphy(PLINT x, PLINT y);

/* Draw to physical coordinates (x,y). */

void
plP_draphy(PLINT x, PLINT y);

/* Move to world coordinates (x,y). */

void
plP_movwor(PLFLT x, PLFLT y);

/* Draw to world coordinates (x,y). */

void
plP_drawor(PLFLT x, PLFLT y);

/* Draw polyline in physical coordinates. */

void
plP_draphy_poly(PLINT *x, PLINT *y, PLINT n);

/* Draw polyline in world coordinates. */

void
plP_drawor_poly(PLFLT *x, PLFLT *y, PLINT n);

/* Sets up physical limits of plotting device. */

void
plP_setphy(PLINT xmin, PLINT xmax, PLINT ymin, PLINT ymax);

/* Set up the subpage boundaries according to the current subpage selected */

void
plP_setsub(void);

/* Get the floating point precision (in number of places) in numeric labels. */

void
plP_gprec(PLINT *p_setp, PLINT *p_prec);

/* Computes the length of a string in mm, including escape sequences. */

PLFLT
plstrl(const char *string);

/* Similar to strpos, but searches for occurence of string str2. */

PLINT
plP_stindex(const char *str1, const char *str2);

/* Searches string str for first occurence of character chr.  */

PLINT
plP_strpos(char *str, int chr);

/* Searches string str for character chr (case insensitive). */

PLINT
plP_stsearch(const char *str, int chr);

	/* Conversion functions */

/* device coords to physical coords (x) */

PLINT
plP_dcpcx(PLFLT x);

/* device coords to physical coords (y) */

PLINT
plP_dcpcy(PLFLT y);

/* millimeters from bottom left-hand corner to physical coords (x) */

PLINT
plP_mmpcx(PLFLT x);

/* millimeters from bottom left-hand corner to physical coords (y) */

PLINT
plP_mmpcy(PLFLT y);

/* world coords to physical coords (x) */

PLINT
plP_wcpcx(PLFLT x);

/* world coords to physical coords (y) */

PLINT
plP_wcpcy(PLFLT y);

/* physical coords to device coords (x) */

PLFLT
plP_pcdcx(PLINT x);

/* physical coords to device coords (y) */

PLFLT
plP_pcdcy(PLINT y);

/* millimeters from bottom left corner to device coords (x) */

PLFLT
plP_mmdcx(PLFLT x);

/* millimeters from bottom left corner to device coords (y) */

PLFLT
plP_mmdcy(PLFLT y);

/* world coords into device coords (x) */

PLFLT
plP_wcdcx(PLFLT x);

/* world coords into device coords (y) */

PLFLT
plP_wcdcy(PLFLT y);

/* subpage coords to device coords (x) */

PLFLT
plP_scdcx(PLFLT x);

/* subpage coords to device coords (y) */

PLFLT
plP_scdcy(PLFLT y);

/* device coords to millimeters from bottom left-hand corner (x) */

PLFLT
plP_dcmmx(PLFLT x);

/* device coords to millimeters from bottom left-hand corner (y) */

PLFLT
plP_dcmmy(PLFLT y);

/* world coords into millimeters (x) */

PLFLT
plP_wcmmx(PLFLT x);

/* world coords into millimeters (y) */

PLFLT
plP_wcmmy(PLFLT y);

/* device coords to subpage coords (x) */

PLFLT
plP_dcscx(PLFLT x);

/* device coords to subpage coords (y) */

PLFLT
plP_dcscy(PLFLT y);

/* 3-d coords to 2-d projection (x) */

PLFLT
plP_w3wcx(PLFLT x, PLFLT y, PLFLT z);

/* 3-d coords to 2-d projection (y) */

PLFLT
plP_w3wcy(PLFLT x, PLFLT y, PLFLT z);

	/* Driver calls */

/* Initialize device. */

void
plP_init(void);

/* Draw line between two points */

void
plP_line(short *x, short *y);

/* Draw polyline */

void
plP_polyline(short *x, short *y, PLINT npts);

/* Fill polygon */

void
plP_fill(short *x, short *y, PLINT npts);

/* draw image */

void
plP_image(short *x, short *y, unsigned short *z, PLINT nx, PLINT ny, PLFLT xmin, PLFLT ymin, PLFLT dx, PLFLT dy, unsigned short zmin, unsigned short zmax);

/* End of page */

void
plP_eop(void);

/* End of page */

void
plP_bop(void);

/* Tidy up device (flush buffers, close file, etc.) */

void
plP_tidy(void);

/* Change state. */

void
plP_state(PLINT op);

/* Escape function, for driver-specific commands. */

void
plP_esc(PLINT op, void *ptr);

/* Set up plot window parameters. */

void
plP_swin(PLWindow *plwin);

/* Return file pointer to lib file. */

FILE *
plLibOpen(char *fn);

/* Does required startup initialization of library.  */

void
pllib_init(void);

/* Does preliminary setup of device driver. */

void
pllib_devinit(void);

/* Utility to copy one PLColor to another. */

void
pl_cpcolor(PLColor *to, PLColor *from);

/* Does required startup initialization of a stream.  */

void
plstrm_init(void);

/* Builds a list of the active devices/streams by device name */

void
plP_getinitdriverlist(char *names);

/* Checks a give list of device names against active streams and returns the number of matches */

PLINT 
plP_checkdriverinit( char *names);

  /* disable writing to plot buffer and pixmap */
void
NoBufferNoPixmap(void);

  /* restart writing to plot buffer and pixmap */
void
RestoreWrite2BufferPixmap(void);

void
grimage(short *x, short *y, unsigned short *z, PLINT nx, PLINT ny);

void
plimageslow(short *x, short *y, unsigned short *data, PLINT nx, PLINT ny, 
	    PLFLT xmin, PLFLT ymin, PLFLT dx, PLFLT dy,
	    unsigned short zmin,  unsigned short zmax);

typedef struct {
  PLFLT xmin, ymin, dx, dy;} IMG_DT;

#ifdef __cplusplus
}
#endif

#endif	/* __PLPLOTP_H__ */
