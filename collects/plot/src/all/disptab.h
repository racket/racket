/* $Id: disptab.h,v 1.1 2004/03/01 20:54:46 cozmic Exp $

   Defines the data structure which holds the driver functions.
*/

#ifndef __DISPATCH_H__
#define __DISPATCH_H__

#include "plConfig.h"

struct PLStream_struct;

enum {
    plDevType_FileOriented = 0,
    plDevType_Interactive = 1,
    plDevType_Null = -1
};

/*--------------------------------------------------------------------------*\
 * Define structure containing pointers to device dependent functions.
 *
 * pl_MenuStr	Pointer to string that is printed in device menu. 
 *
 * pl_DevName	A short device "name" for device selection by name. 
 *
 * pl_type	0 for file-oriented device, 1 for interactive
 *		(the null driver uses -1 here)
 *
 * pl_seq       The sequence number for ordering the presentation list of the
 *              available drivers.  This is an ordering only, not an absolute
 *              position in the list.
 *
 * pl_init	Initialize device.  This routine may also prompt the user
 *		for certain device parameters or open a graphics file
 *		(see note).  Called only once to set things up.  Certain
 *		options such as familying and resolution (dots/mm) should
 *		be set up before calling this routine (note: some drivers
 *		ignore these).
 *
 * pl_line	Draws a line between two points. 
 *
 * pl_polyline	Draws a polyline (no broken segments).
 *
 * pl_eop	Finishes out current page (see note). 
 *
 * pl_bop	Set up for plotting on a new page. May also open a new
 *		a new graphics file (see note). 
 *
 * pl_tidy	Tidy up. May close graphics file (see note). 
 *
 * pl_state	Handle change in PLStream state
 *		(color, pen width, fill attribute, etc).
 *
 * pl_esc	Escape function for driver-specific commands.
 *
 *
 * Notes:
 *
 * Most devices allow multi-page plots to be stored in a single graphics
 * file, in which case the graphics file should be opened in the pl_init()
 * routine, closed in pl_tidy(), and page advances done by calling pl_eop
 * and pl_bop() in sequence. If multi-page plots need to be stored in
 * different files then pl_bop() should open the file and pl_eop() should
 * close it.  Do NOT open files in both pl_init() and pl_bop() or close
 * files in both pl_eop() and pl_tidy().
\*--------------------------------------------------------------------------*/

typedef void (*plD_init_fp)    (struct PLStream_struct *);
typedef void (*plD_line_fp)    (struct PLStream_struct *, short, short, short, short);
typedef void (*plD_polyline_fp)(struct PLStream_struct *, short *, short *, PLINT);
typedef void (*plD_eop_fp)     (struct PLStream_struct *);
typedef void (*plD_bop_fp)     (struct PLStream_struct *);
typedef void (*plD_tidy_fp)    (struct PLStream_struct *);
typedef void (*plD_state_fp)   (struct PLStream_struct *, PLINT);
typedef void (*plD_esc_fp)     (struct PLStream_struct *, PLINT, void *);

typedef struct {
    char *pl_MenuStr;
    char *pl_DevName;
    int  pl_type;
    int  pl_seq;
    plD_init_fp     pl_init;
    plD_line_fp     pl_line;
    plD_polyline_fp pl_polyline;
    plD_eop_fp      pl_eop;
    plD_bop_fp      pl_bop;
    plD_tidy_fp     pl_tidy;
    plD_state_fp    pl_state;
    plD_esc_fp     pl_esc;
} PLDispatchTable;

#endif /* __DISPATCH_H__ */
