/* $Id: plcore.h,v 1.1 2004/03/01 20:54:51 cozmic Exp $

	Contains declarations for core plplot data structures.  This file
	should be included only by plcore.c.
*/

#ifndef __PLCORE_H__
#define __PLCORE_H__

#include "plplotP.h"
#include "drivers.h"
#include "plDevs.h"
#include "disptab.h"

#ifdef ENABLE_DYNDRIVERS
#include <ltdl.h>
typedef lt_ptr (*PLDispatchInit)( PLDispatchTable *pdt );
#else
typedef void (*PLDispatchInit)( PLDispatchTable *pdt );
#endif
    


/* Static function prototypes */

static void	grline		(short *, short *, PLINT);
static void	grpolyline	(short *, short *, PLINT);
static void	grfill		(short *, short *, PLINT);
static void	plSelectDev	(void);
static void	pldi_ini	(void);
static void	calc_diplt	(void);
static void	calc_didev	(void);
static void	calc_diori	(void);
static void	calc_dimap	(void);
static void	plgdevlst	(char **, char **, int *, int);

static void	plInitDispatchTable	(void);

static void	plLoadDriver	(void);

/* Static variables */

static PLINT xscl[PL_MAXPOLY], yscl[PL_MAXPOLY];

static PLINT initfont = 1;	/* initial font: extended by default */

static PLINT lib_initialized = 0;

/*--------------------------------------------------------------------------*\
 * Allocate a PLStream data structure (defined in plstrm.h).
 *
 * This struct contains a copy of every variable that is stream dependent.
 * Only the first [index=0] stream is statically allocated; the rest
 * are dynamically allocated when you switch streams (yes, it is legal
 * to only initialize the first element of the array of pointers).
\*--------------------------------------------------------------------------*/

static PLStream pls0;			/* preallocated stream */
static PLINT ipls;			/* current stream number */

static PLStream *pls[PL_NSTREAMS] = {&pls0};	/* Array of stream pointers */

/* Current stream pointer.  Global, for easier access to state info */

PLStream *plsc = &pls0;

/* Only now can we include this */

#include "pldebug.h"

/*--------------------------------------------------------------------------*\
 * Initialize dispatch table.
 *
 * Each device is selected by the appropriate define, passed in from the
 * makefile.  When installing plplot you may wish to exclude devices not 
 * present on your system in order to reduce screen clutter.
 *
 * If you hit a <CR> in response to the plinit() prompt, you get the FIRST
 * one active below, so arrange them accordingly for your system (i.e. all
 * the system-specific ones should go first, since they won't appear on
 * most systems.)
\*--------------------------------------------------------------------------*/

static PLDispatchTable **dispatch_table = 0;
static int npldrivers = 0;

static PLDispatchInit static_device_initializers[] = {
#ifdef PLD_mac
    plD_dispatch_init_mac8,
    plD_dispatch_init_mac1,
#endif
#ifdef PLD_next
    plD_dispatch_init_nx,
#endif
#ifdef PLD_os2pm
    plD_dispatch_init_os2,
#endif
#if defined(PLD_xwin) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_xw,
#endif
#if defined(PLD_gnome) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_gnome,
#endif
#if defined(PLD_tk) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_tk,
#endif
#if defined(PLD_linuxvga) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_vga,
#endif
#ifdef PLD_mgr
    plD_dispatch_init_mgr,
#endif
#ifdef PLD_win3
    plD_dispatch_init_win3,
#endif
#if defined (_MSC_VER) && defined (VGA)         /* graphics for msc */
    plD_dispatch_init_vga,
#endif
#ifdef PLD_bgi
    plD_dispatch_init_vga,
#endif
#ifdef PLD_gnusvga
    plD_dispatch_init_vga,
#endif
#ifdef PLD_tiff
    plD_dispatch_init_tiff,
#endif
#if defined(PLD_jpg)
    plD_dispatch_init_jpg,
#endif
#if defined(PLD_bmp)  && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_bmp,
#endif
#ifdef PLD_emxvga		       /* graphics for emx+gcc */
    plD_dispatch_init_vga,
#endif
#if defined(PLD_xterm) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_xterm,
#endif
#if defined(PLD_tek4010) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_tekt,
#endif
#if defined(PLD_tek4107) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_tek4107t,
#endif
#if defined(PLD_mskermit) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_mskermit,
#endif
#if defined(PLD_versaterm) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_versaterm,
#endif
#if defined(PLD_vlt) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_vlt,
#endif
#if defined(PLD_conex) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_conex,
#endif
#if defined(PLD_dg300) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_dg,
#endif
#if defined(PLD_plmeta) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_plm,
#endif
#if defined(PLD_tek4010f) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_tekf,
#endif
#if defined(PLD_tek4107f) && !defined(ENABLE_DYNDRIVERS) 
    plD_dispatch_init_tek4107f,
#endif
#if defined(PLD_ps) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_psm,
    plD_dispatch_init_psc,
#endif
#if defined(PLD_xfig) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_xfig,
#endif
#if defined(PLD_ljiip) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_ljiip,
#endif
#if defined(PLD_ljii) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_ljii,
#endif
#if defined( PLD_hp7470) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_hp7470,
#endif
#if defined( PLD_hp7580) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_hp7580,
#endif
#if defined( PLD_lj_hpgl) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_hpgl,
#endif
#if defined( PLD_imp) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_imp,
#endif
#if defined( PLD_pbm) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_pbm,
#endif
#if defined(PLD_png) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_png,
#endif
#if defined(PLD_jpeg) && !defined(ENABLE_DYNDRIVERS)
    //plD_dispatch_init_jpeg,
#endif
#if defined(PLD_pstex) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_pstex,
#endif
#if defined(PLD_ntk) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_ntk,
#endif
#if defined(PLD_cgm) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_cgm,
#endif
#if defined(PLD_mem) && !defined(ENABLE_DYNDRIVERS)
     plD_dispatch_init_mem,
#endif
#if defined(PLD_null) && !defined(ENABLE_DYNDRIVERS)
    //plD_dispatch_init_null,
#endif
#if defined(PLD_tkwin) && !defined(ENABLE_DYNDRIVERS)
    plD_dispatch_init_tkwin,
#endif
    NULL
};

static int nplstaticdevices = ( sizeof(static_device_initializers) /
                                sizeof(PLDispatchInit) ) - 1;
static int npldynamicdevices = 0;

/*--------------------------------------------------------------------------*\
 * Stuff to support the loadable device drivers.
\*--------------------------------------------------------------------------*/

#ifdef ENABLE_DYNDRIVERS
typedef struct {
    char *devnam;
    char *description;
    char *drvnam;
    char *tag;
    int drvidx;
} PLLoadableDevice;

typedef struct {
    char *drvnam;
    lt_dlhandle dlhand;
    
} PLLoadableDriver;

static PLLoadableDevice *loadable_device_list;
static PLLoadableDriver *loadable_driver_list;
#endif

static int nloadabledrivers = 0;

#endif	/* __PLCORE_H__ */
