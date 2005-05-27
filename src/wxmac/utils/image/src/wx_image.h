/*
 * File:     wx_image.h
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

#ifndef wx_imageh
#define wx_imageh

#include "common.h"
#include "wx_setup.h"
#include "wx_canvs.h"
#include "wx_gdi.h"
#include "wx_imgx.h"

#ifdef wx_mac
#include "wximgfil.h" // brings in GIF handling
#endif

#define wxUNKNOWN 0
#ifndef wx_mac		// FIXME
#define wxGIF     1
#endif
#define wxPM      2
#define wxPBM     3
#define wxXBM     4

#ifdef wx_mac 
// Save (device dependent) wxBitmap as a DIB
Bool wxSaveBitmap(char *filename, wxBitmap *bitmap, wxColourMap *colourmap = NULL);
#endif

class wxImage: public wxObject
{
  int filetype;
  wxColourMap *cMap;
#ifdef wx_mac
  wxGIF *gifpic;
#endif
 public:

  /*
   * Public interface
   */

  wxImage(void);
  ~wxImage(void);

  Bool Load(char *file);
  Bool Destroy(void);
  void Draw(wxCanvas *canvas, int x = 0, int y = 0, int width = -1, int height = -1);
  void Resize(int, int);
  void GetSize(int *width, int *height);
  inline int GetType(void) { return filetype; }
  wxColourMap *GetColourMap(void);

  /*
   * X stuff
   */

#ifdef wx_x
  Display       *theDisp;
  int           theScreen;
  unsigned int  ncells, dispWIDE, dispHIGH, dispDEEP;
  Colormap      theCmap, LocalCmap;
  Window        rootW;
  unsigned long black, white, fg, bg, infofg, infobg;
  Visual        *theVisual;
#endif
  /* global vars used by LOAD routines */
  byte          *pic;                   /* ptr to loaded picture */
  byte *pic24;  /* Used by 25-8 bit conversion */
  unsigned int   pWIDE,pHIGH;           /* size of 'pic' */
  int            dEBUG;                 /* print debugging info */
  int            mono;                  /* true if displaying grayscale */


  /* more global variables, used by xv and xvmisc */
  byte          *cpic;         /* cropped version of pic */
  unsigned int  cWIDE, cHIGH,  /* size of cropped region */
                cXOFF, cYOFF;  /* offset of region from 0,0 of pic */

  byte          *epic;         /* expanded version of cpic */
                                   /* points to pic when at 1:1 expansion */
                                   /* this is converted to 'theImage' */
  unsigned int  eWIDE, eHIGH;  /* size of epic */
  unsigned int  normFact;      /* factor to shrink picture by for 'norm' */

#ifdef wx_x
  XImage        *theImage;     /* X version of epic */
#endif
  unsigned long freecols[256]; /* list of pixel values to free */
  int           nfcols;        /* number of colors to free */
  unsigned long cols[256];     /* maps pic pixel values to X pixel vals */
  int           fc2pcol[256];  /* maps freecols into pic pixel values */
  int           numcols;       /* # of desired colors in picture */
  int           ncols;         /* max # of (different) colors to alloc */

  int           expand,        /* expansion amount */
                bwidth,        /* border width of created windows */
                noglob,        /* force to only use colors it alloced */
                revvideo,      /* reverse video */
                perfect,       /* perfect color.  install own colormap */
                fixedaspect,   /* fixed aspect ratio */
                slow24,        /* use slow 24to8 algorithm */
                ninstall,      /* true if using icccm-complaint WM
                                  (a WM that will does install CMaps */
                useroot,       /* true if we should draw in rootW */
                noqcheck,      /* true if we should NOT do QuickCheck */
                rwcolor,       /* true if we should use R/W color cells */
                rwthistime,    /* true if we DID use R/W color cells */
                brokeFreeCols; /* true if server has broken XFreeColors */

  float         defaspect,     /* default aspect ratio to use */
                normaspect;    /* normal aspect ratio of this picture */



  /*************************** XVMISC.C ***************************/
#ifdef wx_x
  void Rotate(int);
  void SortColormap(void);
  void AllocColors(void);
  void AllocRWColors(void);
  void DoMonoAndRV(void);
  void FSDither(byte *, int, int, byte *);
  void CreateXImage(void);
  void FatalError(char *);
  void Timer(int);

  /*************************** XV24TO8.C **************************/
#endif
  int  Conv24to8(byte *, int, int, int);
  int Quick24to8(byte *p24, int w, int h);
  int QuickCheck(byte *pic24, int w, int h, int maxcol);
  void get_histogram(CBOX *box);
  CBOX *largest_box();
  void splitbox(CBOX *ptr);
  void shrinkbox(CBOX *box);
  int quant_fsdither();

#ifdef wx_x
  /**************************** XVGAM.C **************************/
  void GenerateGamma(void);
  void GenerateFSGamma(void);
  void GammifyColors(void);
  void HSVgamma(void);

  /**************************** XVGIF.C ***************************/
  int LoadGIF(char *, int);
  int WriteGIF(FILE *fp, byte *pic, int w, int h, byte *rmap, byte *gmap, byte *bmap, int numcols, int colorstyle);
#endif
  /**************************** XVPM.C ****************************/
  int LoadPM(char *, int);
  int WritePM(FILE *, byte *, int, int, byte *, byte *, byte *, int, int);
#if wx_x
  /**************************** XVPBM.C ***************************/
  int LoadPBM(char *, int);
  int WritePBM(FILE *, byte *, int, int, byte *, byte *, byte *, int, int, int);
  int loadpbm(FILE *fp, int w, int h, int raw);
  int loadpgm(FILE *fp, int w, int h, int maxv, int raw);
  int loadppm(FILE *fp, int w, int h, int maxv, int raw, int nc);
#endif
  /**************************** XVXBM.C ***************************/
  int LoadXBM(char *, int);
  int WriteXBM(FILE *, byte *, int, int, char *);

  /* xvbmp.c */
  int LoadBMP(char *fname, PICINFO *pinfo);
  int WriteBMP(FILE *fp, byte *pic824, int ptype, int w, int h, byte *rmap, byte *gmap, byte *bmap, int numcols, int colorstyle);
#if wx_x
  /* xvpcx.c */
  int LoadPCX(char *fname, PICINFO *pinfo);

   /* Taken from xvmisc.c */
  void RotatePic(byte *, unsigned int *, unsigned int *, int);
  void FloydDitherize8(byte *);
  void FloydDitherize1(XImage *);
  void FreeMostResources(void);

  /* Taken from xvgif.c */
  int  ReadCode();
  void DoInterlace(byte);
  int  GifError(char *);

  /* Taken from xv.c */
  int  openPic(char *file);
  void closePic();
  void FixAspect(int, int *, int *);
  void GetWindowPos(XWindowAttributes *);
  void SetWindowPos(XWindowAttributes *);
  int  rd_int(char *);
  int  rd_str(char *);
  int  rd_flag(char *);
  
#endif
};

#endif // wx_imageh
