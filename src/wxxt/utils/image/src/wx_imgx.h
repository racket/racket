/*
 * File:     wx_imgx.h
 * Purpose:  
 *
 *                       wxWindows 1.50
 * Copyright (c) 2004-2005 PLT Scheme, Inc.
 * Copyright (c) 1993 Artificial Intelligence Applications Institute,
 *                   The University of Edinburgh
 *
 *                     Author: Julian Smart
 *                        Date: 7-9-93
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice, author statement and this permission
 * notice appear in all copies of this software and related documentation.
 *
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, EXPRESS,
 * IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF
 * MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
 *
 * IN NO EVENT SHALL THE ARTIFICIAL INTELLIGENCE APPLICATIONS INSTITUTE OR THE
 * UNIVERSITY OF EDINBURGH BE LIABLE FOR ANY SPECIAL, INCIDENTAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OF ANY KIND, OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER OR NOT ADVISED OF THE POSSIBILITY OF
 * DAMAGE, AND ON ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH
 * THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#ifndef wx_imgxh
#define wx_imgxh

#ifdef SVR4             /* SysV release 4 uses dirent */
#ifndef sgi             /* but Silicon Graphics doesn't */
#define DIRENT
#endif
#endif

#ifdef VMS
#include "unixlib.h"
// some names are different
#define rindex strrchr
#define unlink delete
#endif

/* include files */
#include <stdio.h>
#include <math.h>
#include <ctype.h>
#include <string.h>

#if !defined(__convexc__) && !defined(VMS) /* Convex doesn't have <memory.h> */
#include <memory.h>             /* for 'memset()' prototype */
#endif

/* neither IBM AOS 4.3, Convex, nor BSD 4.3 on VAX have <malloc.h> */
#if !defined(ibm032) && !defined(__convexc__) && \
    !(defined(vax) && !defined(ultrix)) && !defined(VMS)
#if defined(hp300) || defined(hp800)
#include <sys/malloc.h>                /* it's in 'sys' on HPs*/
#else
#include <stdlib.h>
#endif
#endif


/* Comment following out to avoid problems with GNU string.h conflict */
#ifndef wx_xview
#include <X11/Xos.h>
#endif
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include <X11/keysym.h>

#if defined(NEEDSTIME) || defined(NEEDSDIR)
#include <sys/types.h>    /* only include <sys/types.h> once */
#endif

#ifdef NEEDSTIME
#ifndef sgi              /* silicon graphics doesn't have timeb.h */
#include <sys/timeb.h>
#endif
#include <signal.h>
#if defined(sco) && !defined(NOTIMER)
#include <sys/itimer.h>
#endif
#ifndef  sigmask
#define  sigmask(m)      (1 << ((m)-1))
#endif
#endif

#ifdef NEEDSDIR
#ifdef sco
#include <sys/ndir.h>
#define lstat stat
#else
#ifndef ATT
#include <sys/dir.h>
#endif  /* ATT */
#endif  /* sco */
#include <sys/param.h>
#include <sys/stat.h>
#ifdef DIRENT
#include <dirent.h>
#endif
#endif

#ifdef NEEDSVARARGS
#include <varargs.h>
#endif

/* signal macros */
#ifdef SVR4
#define HOLD_SIG         sighold(SIGALRM)  /* block ALRM sig from occurring */
#define RELEASE_SIG      sigrelse(SIGALRM)
#define PAUSE_SIG        sigpause(SIGALRM) /* sleep until ALRM signal */
#else
#define HOLD_SIG         sigblock(sigmask(SIGALRM))
#define RELEASE_SIG      sigblock(0)
#define PAUSE_SIG        sigpause(0)
#endif


#ifdef i386
#define UNCOMPRESS    "/usr/local/bin/uncompress"   /* uncompress program */
#undef  HOLD_SIG
#define HOLD_SIG      /* don't know how to handle signals  MWS 10/18/90 */
#undef  RELEASE_SIG
#define RELEASE_SIG   /* */
#undef  PAUSE_SIG
#define PAUSE_SIG     /* */
#else
#define UNCOMPRESS "/usr/ucb/uncompress"   /* for uncompressing .Z files */
#endif

#define PROGNAME  "xv"             /* used in resource database */

#define DEFINFOGEOM "-10+10"       /* default position of info window */
#define DEFDIRGEOM  "-10-10"       /* default position of directory window */
#define DEFCTRLGEOM "+400+400"     /* default position of ctrl window */
#define DEFGAMGEOM  "+10-10"       /* default position of gamma window */

#define INFOWIDE 500               /* (fixed) size of info window */
#define INFOHIGH 250

#define CTRLWIDE 440               /* (fixed) size of control window */
#define CTRLHIGH 295

#define DIRWIDE  300               /* (fixed) size of directory window */
#define DIRHIGH  420

#define GAMWIDE  366               /* (fixed) size of Gamma window */
#define GAMHIGH  356

#define MAXNAMES 1024   /* max # of files (more than this?  Get REAL!) */

/* strings in the INFOBOX (used in SetISTR and GetISTR) */
#define NISTR         9    /* number of ISTRs */
#define ISTR_INFO     0
#define ISTR_WARNING  1
#define ISTR_FILENAME 2
#define ISTR_FORMAT   3
#define ISTR_RES      4
#define ISTR_CROP     5
#define ISTR_EXPAND   6
#define ISTR_COLOR    7
#define ISTR_COLOR2   8

/* potential values of 'infomode', used in info box drawing routines */
#define INF_NONE 0    /* empty box */
#define INF_STR  1    /* just ISTR_INFO */
#define INF_PART 2    /* filename, format, size and infostr */
#define INF_FULL 3    /* INF_PART + clipping, expansion, colorinfo */


/* buttons in the ctrl window */
#define NBUTTS  20
#define BNEXT   0
#define BPREV   1
#define BCROP   2
#define BUNCROP 3
#define BNORM   4
#define BMAX    5
#define BUP2    6
#define BDN2    7
#define BUP10   8
#define BDN10   9
#define BQUIT   10
#define B4BY3   11
#define BSAVE   12
#define BROTL   13
#define BINFO   14
#define BGAMMA  15
#define BASPECT 16
#define BROTR   17
#define BMAXPECT 18
#define BACROP   19

/* buttons in the 'save' window */
#define S_NBUTTS 4
#define S_BOPEN  0
#define S_BSAVE  1
#define S_BCANC  2
#define S_BQUIT  3


/* buttons in the 'gamma' window */
#define G_NBUTTS  17
#define G_BAPPLY  0
#define G_BNOGAM  1
#define G_BRESET  2
#define G_BDEF    3
#define G_BGTYPE  4
#define G_BCLOSE  5
#define G_BUP_BR  6
#define G_BDN_BR  7
#define G_BUP_CN  8
#define G_BDN_CN  9
#define G_BHSVRGB 10
#define G_B1      11
#define G_B2      12
#define G_B3      13
#define G_B4      14
#define G_BSET    15
#define G_BUNDO   16


/* definitions of first char of dirnames[i] (filetype) */
#define C_FIFO  'f'    /* FIFO special file */
#define C_CHR   'c'    /* character special file */
#define C_DIR   'd'    /* directory */
#define C_BLK   'b'    /* block special file */
#define C_LNK   'l'    /* symbolic link */
#define C_SOCK  's'    /* socket */
#define C_REG   ' '    /* regular file */


/* random string-placing definitions */
#define SPACING 3      /* vertical space between strings */
#define ASCENT   (mfinfo->ascent)
#define DESCENT  (mfinfo->descent)
#define CHIGH    (ASCENT + DESCENT)
#define LINEHIGH (CHIGH + SPACING)


#define STDINSTR "<stdin>"

typedef unsigned char byte;

typedef struct { Window win;            /* window ID */
		 int len;               /* length of major axis */
		 int vert;              /* true if vertical, else horizontal */
		 int active;            /* true if scroll bar can do anything*/
		 int min,max;           /* min/max values 'pos' can take */
		 int val;               /* 'value' of scrollbar */
		 int page;              /* amt val change on pageup/pagedown */
		 int tpos;              /* thumb pos. (pixels from tmin) */
		 int tmin,tmax;         /* min/max thumb offsets (from 0,0) */
		 int tsize;             /* size of thumb (in pixels) */
		 unsigned long fg,bg;   /* colors */
		 void (*drawobj)();     /* redraws obj controlled by scrl*/
		 int uplit, dnlit;      /* true if up&down arrows are lit */
	       } SCRL;

typedef struct { Window win;            /* parent window */
		 int x,y,w,h;           /* size of button rectangle */
		 int lit;               /* if true, invert colors */
		 int active;            /* if false, stipple gray */
		 int toggle;            /* if true, clicking toggles state */
		 unsigned long fg,bg;   /* colors */
		 char *str;             /* string in button */
	       } BUTT;


typedef struct { Window win;            /* window */
		 int x,y,w,h;           /* size of window */
		 unsigned long fg,bg;   /* colors */
		 char **str;            /* ptr to list of strings */
		 int   nstr;            /* number of strings */
		 int   selected;        /* number of 'selected' string */
		 int   nlines;          /* number of lines shown at once */
		 SCRL  scrl;            /* scrollbar that controls list */
		 int   filetypes;       /* true if filetype icons to be drawn*/
		 int   dirsonly;        /* if true, only dirs selectable */
	       } LIST;


typedef struct rbutt { Window        win;      /* parent window */
		       int           x,y;      /* position in parent */
		       char         *str;      /* the message string */
		       int           selected; /* selected or not */
		       int           active;   /* selectable? */
		       struct rbutt *next;     /* pointer to next in group */
		       unsigned long fg,bg;    /* colors */
		     } RBUTT;

/* 25-8 bit conversion stuff */
#define	MAX_CMAP_SIZE	256
#define	COLOR_DEPTH	8
#define	MAX_COLOR	256
#define	B_DEPTH		5		/* # bits/pixel to use */
#define	B_LEN		(1<<B_DEPTH)
#define	C_DEPTH		2
#define	C_LEN		(1<<C_DEPTH)	/* # cells/color to use */

typedef	struct colorbox {
  struct colorbox *next, *prev;
  int              rmin,rmax, gmin,gmax, bmin,bmax;
  int              total;
} CBOX;

typedef struct {
  int num_ents;
  int entries[MAX_CMAP_SIZE][2];
} CCELL;

extern byte           gamcr[];   /* gamma correction curve */
extern byte           fsgamcr[]; /* gamma correction curve (for FS dither) */
extern XPoint                ghand[];

/* MACROS */
#define CENTERX(f,x,str) ((x)-XTextWidth(f,str,strlen(str))/2)
#define CENTERY(f,y) ((y)-((f->ascent+f->descent)/2)+f->ascent)

/* RANGE forces a to be in the range b..c (inclusive) */
#define RANGE(a,b,c) { if (a<b) a=b;  if (a>c) a=c; }

/* PTINRECT returns '1' if x,y is in rect (inclusive) */
#define PTINRECT(x,y,rx,ry,rw,rh) \
           ((x)>=(rx) && (y)>=(ry) && (x)<=(rx)+(rw) && (y)<=(ry)+(rh))

/* MONO returns total intensity of r,g,b components */
#define MONO(rd,gn,bl) (((rd)*11 + (gn)*16 + (bl)*5) >> 5)  /*.33R+ .5G+ .17B*/

typedef struct { byte *pic;                  /* image data */
		 int   w, h;                 /* size */
		 int   type;                 /* PIC8 or PIC24 */

		 byte  r[256],g[256],b[256];
		                             /* colormap, if PIC8 */

		 int   frmType;              /* def. Format type to save in */
		 int   colType;              /* def. Color type to save in */
		 char  fullInfo[128];        /* Format: field in info box */
		 char  shrtInfo[128];        /* short format info */
		 char *comment;              /* comment text */
		 
		 int   numpages;             /* # of page files, if >1 */
		 char  pagebname[64];        /* basename of page files */
	       } PICINFO;

/* constants for setting radio buttons in dirW */
#define F_COLORS    0
#define F_FORMAT    1

#define F_FULLCOLOR 0
#define F_GREYSCALE 1
#define F_BWDITHER  2
#define F_REDUCED   3

#define F_GIF       0
#define F_PM        1
#define F_PBMRAW    2
#define F_PBMASCII  3
#define F_XBM       4
#define F_SUNRAS    5
#define F_BMP       6
#define F_PS        7
#define F_IRIS      8

#ifdef HAVE_JPEG
#define F_JPEG      9
#endif

#ifdef HAVE_TIFF
#ifdef HAVE_JPEG
#define F_TIFF      10
#else
#define F_TIFF      9
#endif
#endif

/* indicies into conv24MB */
#define CONV24_8BIT  0
#define CONV24_24BIT 1
#define CONV24_SEP1  2
#define CONV24_LOCK  3
#define CONV24_SEP2  4
#define CONV24_FAST  5
#define CONV24_SLOW  6
#define CONV24_BEST  7
#define CONV24_MAX   8

/* values 'picType' can take */
#define PIC8  CONV24_8BIT
#define PIC24 CONV24_24BIT

void InitFSDTables(void);
void xvDestroyImage(XImage *image);
void xvbzero(char *s, int len);

#endif // wx_imgx
