/*
 * These definitions are for the Postscript (ps.c) and 
 * Postscript/LaTeX (pstex.c) drivers
 */

#ifndef __PS_H__
#define __PS_H__

/* top level declarations */

#define LINELENGTH      78
#define COPIES          1
#define XSIZE           540		/* 7.5 x 10 [inches]    */
#define YSIZE           720		/* (72 points = 1 inch) */
#define ENLARGE         5
#define XPSSIZE         ENLARGE*XSIZE
#define YPSSIZE         ENLARGE*YSIZE
#define XOFFSET         32		/* Margins --     */
#define YOFFSET         32		/* .5 inches each */
#define PSX             XPSSIZE-1
#define PSY             YPSSIZE-1
#define OF		pls->OutFile
#define MIN_WIDTH	1		/* Minimum pen width */
#define MAX_WIDTH	30		/* Maximum pen width */
#define DEF_WIDTH	3		/* Default pen width */

/* These are for covering the page with the background color */

#define XMIN		-XOFFSET*ENLARGE
#define XMAX		PSX+XOFFSET*ENLARGE
#define YMIN		-XOFFSET*ENLARGE
#define YMAX		PSY+XOFFSET*ENLARGE

/* Struct to hold device-specific info. */

typedef struct {
    PLFLT pxlx, pxly;
    PLINT xold, yold;

    PLINT xmin, xmax, xlen;
    PLINT ymin, ymax, ylen;

    PLINT xmin_dev, xmax_dev, xlen_dev;
    PLINT ymin_dev, ymax_dev, ylen_dev;

    PLFLT xscale_dev, yscale_dev;

    int llx, lly, urx, ury, ptcnt;
} PSDev;

void plD_init_pstex		(PLStream *);
void plD_line_pstex		(PLStream *, short, short, short, short);
void plD_polyline_pstex		(PLStream *, short *, short *, PLINT);
void plD_eop_pstex		(PLStream *);
void plD_bop_pstex		(PLStream *);
void plD_tidy_pstex		(PLStream *);
void plD_state_pstex		(PLStream *, PLINT);
void plD_esc_pstex		(PLStream *, PLINT, void *);

void plD_init_psm		(PLStream *);
void plD_init_psc		(PLStream *);
void plD_line_ps		(PLStream *, short, short, short, short);
void plD_polyline_ps	        (PLStream *, short *, short *, PLINT);
void plD_eop_ps		        (PLStream *);
void plD_bop_ps			(PLStream *);
void plD_tidy_ps		(PLStream *);
void plD_state_ps	        (PLStream *, PLINT);
void plD_esc_ps			(PLStream *, PLINT, void *);

#endif /* __PS_H__ */
