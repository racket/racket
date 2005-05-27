/*
 * xvmisc.c - random routines that most of the work involved in putting
 *            a picture on an X display.
 *
 *  Resize(w,h)    -  generates epic, new Ximage,
 *  SortColormap() -  sorts the desired colormap into a 'most important color
 *                    to allocate first' order
 *  AllocColors()  -  Takes the desired colormap and sees what it can do about
 *                    getting it.
 *  Rotate()       -  rotates 'pic' 90-degrees clockwise
 *  FloydDitherize8 - Takes epic, and produces a b/w dithered version of it,
 *                    stored in 8-bit per pixel format
 *  FloydDitherize1 - Does the same, but the output is an XYBitmap (1 bit per
 *                    pixel, packed 8 pixels to a byte.
 *  CreateXImage    - Given epic, generate an ximage of it.  Handles displays
 *                    of different depths.
 *  CenterString    - civilized string drawing routine.  uses font 'mfinfo'
 *  ULineString     - draws underlined string.  uses font 'mfinfo'
 *  StringWidth     - civilized XTextWidth width interface, uses font 'mfinfo'
 *  FakeButtonPress - when a keyboard equiv is hit, fakes mouse click in button
 *  Timer           - sleeps for a given # of milliseconds
 */

/*
 * Copyright 1989, 1990 by the University of Pennsylvania
 *
 * Permission to use, copy, and distribute for non-commercial purposes,
 * is hereby granted without fee, providing that the above copyright
 * notice appear in all copies and that both the copyright notice and this
 * permission notice appear in supporting documentation.
 *
 * The software may be modified for your own purposes, but modified versions
 * may not be distributed.
 *
 * This software is provided "as is" without any express or implied warranty.
 */


#define NEEDSTIME

#include <stdlib.h>
#include "wx_image.h"
#include "wx_utils.h"
#include "../../../src/XWidgets/wxAllocColor.h"

#ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
#endif

/***********************************/
void wxImage::Resize(int w, int h)
{
  int          cy,ex,ey,*cxarr, *cxarrp;
  byte        *clptr,*elptr,*epptr;

  clptr = NULL;  cxarrp = NULL;  cy = 0;  /* shut up compiler */
#if 0
  /* force w,h into valid ranges */
  RANGE(w,1,(int)dispWIDE);  RANGE(h,1,(int)dispHIGH);
#endif

  /* if same size, and Ximage created, do nothing */
  if (w == (int)eWIDE && h == (int)eHIGH && theImage!=NULL) return;

  if (imgDEBUG) fprintf(stderr,"wxImage: Resize(%d,%d)  eSIZE=%d,%d  cSIZE=%d,%d\n",
		     w,h,eWIDE,eHIGH,cWIDE,cHIGH);

  if (w == (int)cWIDE && h == (int)cHIGH) {  /* 1:1 expansion.  point epic at cpic */
    if (epic != cpic && epic!=NULL) free(epic);
    epic = cpic;  eWIDE = cWIDE;  eHIGH = cHIGH;
  }

  else {  /* have to actually SCALE THE PIC.  Drats! */

    /* first, kill the old epic, if one exists */
    if (epic!=NULL && epic!=cpic) {
      free(epic);  epic = NULL;
    }

    /* create a new epic of the appropriate size */
    eWIDE = w;  eHIGH = h;
    {
      byte *ba;
      ba = (byte *)malloc(w*h);
      epic = ba;
    }
    if (epic==NULL) {
      sprintf(wxBuffer,"unable to malloc a %dx%d image\n",w,h);
      FatalError(wxBuffer);
    }

    /* the scaling routine.  not really all that scary after all... */

    /* OPTIMIZATON IDEA.  Malloc an eWIDE array of ints which will hold the
       values of the equation px = (pWIDE * ex) / eWIDE.  Faster than doing 
       a mul and a div for every point in picture */

    cxarr = (int *) malloc(eWIDE * sizeof(int));
    if (!cxarr) FatalError("unable to allocate cxarr");
    for (ex=0; ex < (int)eWIDE; ex++) {
      cxarr[ex] = (cWIDE * ex) / eWIDE;
    }

    elptr = epptr = epic;
    for (ey=0;  ey < (int)eHIGH;  ey++, elptr+=eWIDE) {
      cy = (cHIGH * ey) / eHIGH;
      epptr = elptr;
      clptr = cpic + (cy * cWIDE);
      for (ex=0, cxarrp = cxarr;  ex < (int)eWIDE;  ex++, epptr++) {
	*epptr = clptr[*cxarrp++];
      }
    }
    free(cxarr);
  }

  /* now make something displayable out of epic */
  CreateXImage();
}
                



/************************************************/
/* structure and routine used in SortColormap() */
/************************************************/

typedef struct thing 
    { byte r,g,b; 
      int oldindex; 
      int use; } CMAPENT;


static int CMAPcompare(CMAPENT *a,CMAPENT *b)
{
  return (b->use - a->use);
}

typedef int (*image_Compare_Proc)(const void *, const void *);

/***********************************/
void wxImage::SortColormap()
{
  byte *p;
  int   i, j, k, mdist, entry, mn, d, hist[256], trans[256];
  static CMAPENT c[256], c1[256], *cp, *cj, *ck;


  /* no point doing this if we're on a 1-bit display */
  if (ncols == 0) { numcols = 256; return; }
  
  /* initialize histogram and compute it */
  for (i=0; i<256; i++) {
    hist[i]=0;
  }
  for (i=pWIDE*pHIGH, p=pic; i; i--, p++) {
    hist[*p]++;
  }
  
  if (imgDEBUG>1) {
    fprintf(stderr,"Desired colormap\n");
    for (i=0; i<256; i++) {
      if (hist[i]) 
	fprintf(stderr,"(%3d  %02x,%02x,%02x)     ",
		i,r[i],g[i],b[i]);
    }
    fprintf(stderr,"\n\n");
  }

  /* Is the transparent index used? */
  if (transparent_index >= 0) {
    if (!hist[transparent_index])
      transparent_index = -1;
  }
  
  /* put the actually-used colors into the 'c' array in the order they occur */
  /* also, while we're at it, calculate numcols */
  for (i=numcols=0; i<256; i++) {
    if (hist[i]) { 
      cp = &c[numcols++];
      cp->r = r[i];  cp->g = g[i];  cp->b = b[i];
      cp->use = hist[i];  cp->oldindex = i;
    }
  }


  /* find most-used color, put that in c1[0] */
  entry = -1;  mdist = -1;
  for (i=0; i<numcols; i++) {
    if (c[i].use > mdist) { mdist = c[i].use;  entry=i; }
  }
  memcpy(&c1[0], &c[entry], sizeof(CMAPENT));
  c[entry].use = 0;   /* and mark it dealt with */
  
  
  /* sort rest of colormap, in order of decreasing 'distance' from already
     allocated elements.
  
     FURTHER MODIFICATION of algorithm.  The algorithm's performance
     utterly goes to hell as numcols increases.  (Probably on the order
     of O^3 performance).  Since I don't see a clever way of rewriting
     the algorithm for O^2 performance (which'd be acceptable), I'm going
     to make a trade-off.  I'll only run the algorithm for the first 32 colors
     (or so).  It can do that Real Fast.  Then I'll just stick the rest of
     the unsorted colors (if any), and tack them on the end, in order of
     amount of use.  This should give similar picture quality, with 
     much higher performance. */

  for (i=1; i<numcols && i<32; i++) {
    /* find the i'th most different color */
    entry = -1;  mdist = -1;
    for (j=0, cj=c; j<numcols; j++,cj++) {
      if (cj->use) {  /* this color has not been marked already */
	mn = 10000;
	for (k=0, ck=c1; k<i; k++,ck++) {
	  d = abs((int)(cj->r - ck->r)) + abs((int)(cj->g - ck->g)) + abs((int)(cj->b - ck->b));
	  if (mn>d) mn=d;
	}
	/* mn = minimum distance from c[j] to already used colors */
	/* we want to select the unused color that has the greatest mn */
	if (mn > mdist) { mdist = mn;  entry = j; }
      }
    }
    
    /* c[entry] is the next color to put in the map.  do so */
    memcpy(&c1[i], &c[entry], sizeof(CMAPENT));
    c[entry].use = 0;
  }
  
  /* tack rest of colors onto colormap in decreasing order of use */
  qsort((char *)c, numcols, sizeof(CMAPENT), (image_Compare_Proc)CMAPcompare);

  memcpy(&c1[i], c, (numcols - i) * sizeof(CMAPENT));


  /* build translation table */
  for (i=0; i<numcols; i++) {
    trans[ c1[i].oldindex ] = i;
  }

  /* modify 'pic' to reflect the new colormap */
  for (i=pWIDE*pHIGH, p=pic; i; i--, p++) {
    *p = trans[*p];
  }
  
  if (transparent_index >= 0)
    transparent_index = trans[transparent_index];
  
  /* and copy the new colormap into *the* colormap */
  for (i=0; i<numcols; i++) {
    r[i] = c1[i].r;  g[i] = c1[i].g;  b[i] = c1[i].b;
  }
  
  if (imgDEBUG>1) {
    fprintf(stderr,"Result of sorting colormap\n");
    for (i=0; i<numcols; i++) {
      fprintf(stderr,"(%3d  %02x,%02x,%02x)     ",i,r[i],g[i],b[i]);
    }
    fprintf(stderr,"\n\n");
    
    fprintf(stderr,"Translate table\n");
    for (i=0; i<numcols; i++) {
      fprintf(stderr,"%3d->%3d  ",i,trans[i]);
    }
    fprintf(stderr,"\n\n");
  }
  
}



#define NOPIX 0xffffffff    

/***********************************/
void wxImage::AllocColors()
{
  int      i, j, unique, p2alloc, p3alloc;
  Colormap cmap;
  XColor   defs[256];
  XColor   ctab[256];
  int      dc;


  nfcols = unique = p2alloc = p3alloc = 0;
  rwthistime = 0;


  if (ncols == 0) {
    return;
  }


  /* FIRST PASS COLOR ALLOCATION:  
     for each color in the 'desired colormap', try to get it via
     XAllocColor().  If for any reason it fails, mark that pixel
     'unallocated' and worry about it later.  Repeat. */

  /* attempt to allocate first ncols entries in colormap 
     note: On displays with less than 8 bits per RGB gun, it's quite
     possible that different colors in the original picture will be
     mapped to the same color on the screen.  X does this for you
     silently.  However, this is not-desirable for this application, 
     because when I say 'allocate me 32 colors' I want it to allocate
     32 different colors, not 32 instances of the same 4 shades... */
  
  for (i=0; i<numcols; i++) {
    cols[i] = NOPIX;
  }
  
  cmap = theCmap;
  for (i=0; i<numcols && unique<ncols; i++) {
    defs[i].red   = r[i]<<8;
    defs[i].green = g[i]<<8;
    defs[i].blue  = b[i]<<8;
    defs[i].flags = DoRed | DoGreen | DoBlue;
    
    if (XAllocColor(theDisp, cmap, &defs[i])) { 
      unsigned long pixel, *fcptr;
      
      pixel = cols[i] = defs[i].pixel;
      
      /* see if the newly allocated color is new and different */
      for (j=0, fcptr=freecols; j<nfcols && *fcptr!=pixel; j++,fcptr++) {
      }
      if (j==nfcols) unique++;
      
      fc2pcol[nfcols] = i;
      freecols[nfcols++] = pixel;
    }

    else {
      /* the allocation failed.  If we want 'perfect' color, and we haven't 
	 already created our own colormap, we'll want to do so */
      if (perfect && !LocalCmap) {
	LocalCmap = XCopyColormapAndFree(theDisp,theCmap);
//	XSetWindowColormap(theDisp,mainW, LocalCmap);
	cmap = LocalCmap;
	i--;	  /* redo the allocation request */
      }

      else
	/* either we don't care about perfect color, or we do care, have
	   allocated our own colormap, and have STILL run out of colors
	   (possible, even on an 8 bit display), just mark pixel as
	   unallocated.  We'll deal with it later */
	cols[i] = NOPIX;
    }
  }  /* FIRST PASS */
  
  
  
  if (nfcols==numcols) {
    return;
  }
  


  /* SECOND PASS COLOR ALLOCATION:
     Allocating 'exact' colors failed.  Now try to allocate 'closest'
     colors.

     Read entire X colormap (or first 256 entries) in from display.
     for each unallocated pixel, find the closest color that actually
     is in the X colormap.  Try to allocate that color (read only).
     If that fails, the THIRD PASS will deal with it */

  /* read entire colormap (or first 256 entries) into 'ctab' */
  dc = (ncells<256) ? ncells : 256;
  for (i=0; i<dc; i++) {
    ctab[i].pixel = (unsigned long) i;
  }

  XQueryColors(theDisp, cmap, ctab, dc);

  for (i=0; i<numcols && unique<ncols; i++) {
    if (cols[i]==NOPIX) {  /* an unallocated pixel */
      int           d, mdist, close;
      unsigned long ri,gi,bi;

      mdist = 100000;   close = -1;
      ri = r[i];  gi = g[i];  bi = b[i];
      
      for (j=0; j<dc; j++) {
	d = abs((int)(ri - (ctab[j].red>>8))) +
	    abs((int)(gi - (ctab[j].green>>8))) +
	    abs((int)(bi - (ctab[j].blue>>8)));
	if (d<mdist) { mdist=d; close=j; }
      }

      if (close<0) FatalError("This Can't Happen! (How reassuring.)");
      if (XAllocColor(theDisp, cmap, &ctab[close])) { 
	memcpy(&defs[i], &ctab[close], sizeof(XColor));
	cols[i] = ctab[close].pixel;
	fc2pcol[nfcols] = i;
	freecols[nfcols++] = cols[i];
	p2alloc++;
	unique++;
      }
    }
  }


  /* THIRD PASS COLOR ALLOCATION:
     We've alloc'ed all the colors we can.  Now, we have to map any
     remaining unalloced pixels into either A) the colors that we DID get
     (noglob), or B) the colors found in the X colormap */

  for (i=0; i<numcols; i++) {
    if (cols[i] == NOPIX) {  /* an unallocated pixel */
      int           d, k, mdist, close;
      unsigned long ri,gi,bi;

      mdist = 100000;   close = -1;
      ri = r[i];  gi = g[i];  bi = b[i];
      
      if (!noglob) {   /* search the entire X colormap */
	for (j=0; j<dc; j++) {
	  d = abs((int)(ri - (ctab[j].red>>8))) +
	      abs((int)(gi - (ctab[j].green>>8))) +
	      abs((int)(bi - (ctab[j].blue>>8)));
	  if (d<mdist) { mdist=d; close=j; }
	}
	if (close<0) FatalError("This Can't Happen! (How reassuring.)");
	memcpy(&defs[i], &ctab[close], sizeof(XColor));
	cols[i] = defs[i].pixel;
	p3alloc++;
      }
	  
      else {                     /* only search the alloc'd colors */
	for (j=0; j<nfcols; j++) {
	  k = fc2pcol[j];
	  d = abs((int)(ri - (defs[k].red>>8))) +
	      abs((int)(gi - (defs[k].green>>8))) +
	      abs((int)(bi - (defs[k].blue>>8)));
	  if (d<mdist) { mdist=d;  close=k; }
	}
	if (close<0) FatalError("This Can't Happen! (How reassuring.)");
	memcpy(&defs[i], &defs[close], sizeof(XColor));
	cols[i] = defs[i].pixel;
      }
    }
  }  /* THIRD PASS */
}



/***********************************/
void wxImage::AllocRWColors()
{
  int i,j;
  Colormap cmap;
  XColor   defs[256];

  nfcols = 0;   rwthistime = 1;

  if (ncols == 0) {
    rwthistime = 0;
    return;
  }


  cmap = theCmap;

  for (i=0; i<numcols; i++) {
    cols[i] = NOPIX;
  }

  for (i=0; i<numcols && i<ncols; i++) {
    unsigned long pmr[1], pix[1];
    if (XAllocColorCells(theDisp, cmap, False, pmr, 0, pix, 1)) {
      defs[i].pixel = cols[i] = pix[0];
      defs[i].red   = r[i]<<8;
      defs[i].green = g[i]<<8;
      defs[i].blue  = b[i]<<8;
      defs[i].flags = DoRed | DoGreen | DoBlue;

      fc2pcol[nfcols]    = i;
      freecols[nfcols++] = pix[0];
      }

    else {
      if (perfect && !LocalCmap) {
	LocalCmap = XCopyColormapAndFree(theDisp,theCmap);
//	XSetWindowColormap(theDisp,mainW, LocalCmap);
	cmap = LocalCmap;
	i--;	  /* redo the allocation request */
      }

      else cols[i] = NOPIX;
    }
  }  /* for (i=0; ... */



  if (nfcols==numcols) {
  }

  else {
    /* Failed to allocate all colors in picture.  Map remaining desired 
       colors into closest allocated desired colors */

      if (nfcols==0) {
	AllocColors();
	return;
      }
	
      for (i=0; i<numcols; i++) {
	if (cols[i]==NOPIX) {  /* an unallocated pixel */
	  int           k, d, mdist, close;
	  unsigned long ri,gi,bi;

	  mdist = 100000;   close = -1;
	  ri = r[i];  gi = g[i];  bi = b[i];

	  for (j=0; j<nfcols; j++) {
	    k = fc2pcol[j];
	    d = abs((int)(ri - (defs[k].red>>8))) + abs((int)(gi - (defs[k].green>>8))) +
	        abs((int)(bi - (defs[k].blue>>8)));
	    if (d<mdist) { mdist=d; close=k; }
	  }

	  if (close<0) FatalError("This Can't Happen! (How reassuring.)");
	  cols[i] = defs[close].pixel;
	}
      }
    }

  /* load up the allocated colorcells */
  for (i=0; i<nfcols; i++) {
    j = fc2pcol[i];
    defs[i].pixel = freecols[i];
    defs[i].red   = r[j]<<8;
    defs[i].green = g[j]<<8;
    defs[i].blue  = b[j]<<8;
    defs[i].flags = DoRed | DoGreen | DoBlue;
    /* fprintf(stderr,"StoreColors: %3d = %3d,%3d,%3d\n",
       defs[i].pixel,r[j],g[j],b[j]); */
  }
  XStoreColors(theDisp, cmap, defs, nfcols);
  XStoreColor(theDisp, cmap, &defs[0]);   /* bug in XStoreColors call */
}



/***********************************/
void wxImage::DoMonoAndRV()
{
  int i;

  /* operate on original colors, before any gamma correction */
  for (i=0; i<numcols; i++) {
    r[i] = rorg[i];  g[i] = gorg[i];  b[i] = borg[i];
  }

  if (mono || ncols==0)  /* if monochrome, mono-ify the desired colormap */
    for (i=0; i<numcols; i++) {
      r[i] = g[i] = b[i] = MONO(r[i],g[i],b[i]);
    }

  if (revvideo)  /* reverse the desired colormaps */
    for (i=0; i<numcols; i++) {
      r[i] = 255-r[i];  g[i] = 255-g[i];  b[i] = 255-b[i];
    }
}

/***********************************/
#if 0
void wxImage::Rotate(int dir)
{
  int i;

  /* dir=0: clockwise, else counter-clockwise */

  RotatePic(pic, &pWIDE, &pHIGH, dir);

  /* rotate clipped version and modify 'clip' coords */
  if (cpic != pic && cpic != NULL) {
    if (!dir) {
      i = pWIDE - (cYOFF + cHIGH);      /* have to rotate offsets */
      cYOFF = cXOFF;
      cXOFF = i;
    }
    else {
      i = pHIGH - (cXOFF + cWIDE);
      cXOFF = cYOFF;
      cYOFF = i;
    }
    RotatePic(cpic, &cWIDE, &cHIGH,dir);
  }
  else { cWIDE = pWIDE;  cHIGH = pHIGH; }

  /* rotate expanded version */
  if (epic != cpic && epic != NULL) {
    RotatePic(epic, &eWIDE, &eHIGH,dir);
  }
  else { eWIDE = cWIDE;  eHIGH = cHIGH; }

  CreateXImage();
//  WRotate();
}
#endif


/************************/
#if 0
void wxImage::RotatePic(byte *pic, unsigned int *wp, unsigned int *hp, int dir)
{
  /* rotates a w*h array of bytes 90 deg clockwise (dir=0) 
     or counter-clockwise (dir != 0).  swaps w and h */

  byte *pic1, *pix1, *pix;
  int          i,j;
  unsigned int w,h;

  w = *wp;  h = *hp;  
  pix1 = pic1 = (byte *) malloc(w*h);
  if (!pic1) FatalError("Not enough memory to rotate!");

  /* do the rotation */
  if (dir==0) {
    for (i=0; i < (int)w; i++) {        /* CW */
      for (j=h-1, pix=pic+(h-1)*w + i; j>=0; j--, pix1++, pix-=w) {
	*pix1 = *pix;
      }
    }
  }
  else {
    for (i=w-1; i>=0; i--) {     /* CCW */
      for (j=0, pix=pic+i; j < (int)h; j++, pix1++, pix+=w) {
	*pix1 = *pix;
      }
    }
  }


  /* copy the rotated buffer into the original buffer */
  memcpy(pic, pic1, w*h);

  free(pic1);

  /* swap w and h */
  *wp = h;  *hp = w;
}
#endif
  

/************************/
void wxImage::FloydDitherize8(byte *image)
{
  /* takes epic, and builds a black&white dithered version of it.
     stores result in 8bit Pixmap format in 'image' */

  int i;
  byte *p;

  FSDither(epic, eWIDE, eHIGH, image);

  /* set to 'black' and 'white' instead of '0' and '1' */
  if (black != 0 || white != 1) {
    for (i=eWIDE*eHIGH, p=image; i>0; i--, p++) {
      if (*p) *p = white;  else *p = black;
    }
  }
}



/************************/
void wxImage::FloydDitherize1(XImage * /* ximage */)
{
  /* same as FloydDitherize8, but output is a 1-bit per pixel XYBitmap,
     packed 8 pixels per byte */

  register short *dp;
  register byte   pix8, bit;
  short          *dithpic;
  int             i, j, err, bperln, order;
  byte           *pp, *image, w, blck, w8, b8;


  image  = (byte *) theImage->data;
  bperln = theImage->bytes_per_line;
  order  = theImage->bitmap_bit_order;

  if (imgDEBUG) fprintf(stderr,"Ditherizing1...");

  dithpic = (short *) malloc(eWIDE * eHIGH * sizeof(short));
  if (dithpic == NULL) FatalError("not enough memory to ditherize");

  w = white&0x1;  blck=black&0x1;
  w8 = w<<7;  b8 = blck<<7;        /* b/w bit in high bit */
  
  /* copy r[epic] into dithpic so that we can run the algorithm */
  pp = epic;  dp = dithpic;
  for (i=eHIGH * eWIDE; i>0; i--) {
    *dp++ = fsgamcr[r[*pp++]];
  }

  dp = dithpic;
  pp = image;

  for (i=0; i<(int)eHIGH; i++) {
    pp = image + i*bperln;

    if (order==LSBFirst) {
      bit = pix8 = 0;
      for (j=0; j<(int)eWIDE; j++,dp++) {
	if (*dp<128) { err = *dp;     pix8 |= b8; }
	        else { err = *dp-255; pix8 |= w8; }

	if (bit==7) {
	  *pp++ = pix8;  bit=pix8=0;
	}
	else { pix8 >>= 1;  bit++; }

	if (j < (int)eWIDE-1) dp[1] += ((err*7)/16);

	if (i < (int)eHIGH-1) {
	  dp[eWIDE] += ((err*5)/16);
	  if (j > 0)       dp[eWIDE-1] += ((err*3)/16);
	  if (j < (int)eWIDE-1) dp[eWIDE+1] += (err/16);
	}
      }
      if (bit) *pp++ = pix8>>(7-bit);  /* write partial byte at end of line */
    }

    else {   /* order==MSBFirst */
      bit = pix8 = 0;
      for (j=0; j < (int)eWIDE; j++,dp++) {
	if (*dp<128) { err = *dp;     pix8 |= blck; }
	        else { err = *dp-255; pix8 |= w; }

	if (bit==7) {
	  *pp++ = pix8;  bit=pix8=0;
	}
	else { pix8 <<= 1; bit++; }

	if (j < (int)eWIDE-1) dp[1] += ((err*7)/16);

	if (i < (int)eHIGH-1) {
	  dp[eWIDE] += ((err*5)/16);
	  if (j>0)       dp[eWIDE-1] += ((err*3)/16);
	  if (j < (int)eWIDE-1) dp[eWIDE+1] += (err/16);
	}
      }
      if (bit) *pp++ = pix8<<(7-bit);  /* write partial byte at end of line */
    }
  }

  if (imgDEBUG) fprintf(stderr,"done\n");

  free(dithpic);
}



/************************/
void wxImage::FSDither(byte *inpic, int w, int h, byte *outpic)
{
  /* takes inpic, and builds a black&white dithered version of it.
     stores result as 1 byte per pixel format in 'image'
     black = 0;  white = 1;
     temporarily mallocs a w*h array of SHORTS.
     (need to be signed, also to avoid overflow problems.)  */

  /* floyd-steinberg dithering.
   *
   * ----   x    7/16
   * 3/16  5/16  1/16
   *
   */

  short *dp, *dithpic;
  int    i, j, err, w1, h1;
  byte  *pp, rgb[256];

  if (imgDEBUG) fprintf(stderr,"Ditherizing...");

  /* first thing to do is build rgb[], which will hold the B/W intensity
     of the colors in the r,g,b arrays */
  for (i=0; i<256; i++) {
    rgb[i] = MONO(r[i], g[i], b[i]);
  }


  dithpic = (short *) malloc(w*h * sizeof(short));
  if (dithpic == NULL) FatalError("not enough memory to ditherize");

  w1 = w-1;  h1 = h-1;

  /* copy rgb[inpic] into dithpic so that we can run the algorithm */
  pp = inpic;  dp = dithpic;
  for (i=w*h; i>0; i--) {
    *dp++ = fsgamcr[rgb[*pp++]];
  }

  dp = dithpic;  pp = outpic;
  for (i=0; i<h; i++) {
    for (j=0; j<w; j++,dp++,pp++) {
      if (*dp<128) { err = *dp;     *pp = 0; }
              else { err = *dp-255; *pp = 1; }

      if (j<w1) dp[1] += ((err*7)/16);

      if (i<h1) {
        dp[w] += ((err*5)/16);
        if (j>0)  dp[w1] += ((err*3)/16);
        if (j<w1) dp[w+1] += (err/16);
      }
    }
  }

  if (imgDEBUG) fprintf(stderr,"done\n");

  free(dithpic);
}

/***********************************/
void wxImage::CreateXImage()
{
  /*
   * this has to do the tricky bit of converting the data in 'epic'
   * into something usable for X.
   *
   * Algorithm notes:
   *   if dispDEEP is 8, nothing has to be done other than create an
   *      Ximage (ZPixmap, depth=8) and point it at the 'epic' data.
   *
   *   if dispDEEP is 1, format'll be an XYBitmap, special case code
   *   
   *   if dispDEEP is 4, format'll be a ZPixmap, 4 or 8 bits per pixel
   *
   *   if dispDEEP is 6, format'll be a ZPixmap, 8 bits per pixel
   *
   *   if dispDEEP is 16, format'll be a ZPixmap.  16 bits per pixel
   *
   *   if dispDEEP is 24, format'll be a ZPixmap.  24 bits per pixel
   *
   *   if dispDEEP is 32, format'll be a ZPixmap.  32 bits per pixel
   *
   *   any other value of dispDEEP will use a XYPixmap of the appropriate
   *   depth, and some slug-like general-case code  DOESN'T YET!!
   */

  if (imgDEBUG) 
    fprintf(stderr,"Creating a %dx%d Ximage, %d bits deep\n",
	    eWIDE, eHIGH, dispDEEP);

  /* destroy old image and imagedata, if there is one */
  if (theImage) xvDestroyImage(theImage);
  theImage = NULL;

  if (!epic) {
    /* fprintf(stderr,"CreateXImage called while epic was null\n"); */
    Resize(eWIDE,eHIGH);
    return;
  }

  if (transparent_index >= 0) {
    byte *pp = epic;
    int i, j;

    theMask = wxiAllocMask(eWIDE, eHIGH);
    for (j = 0; j < (int)eHIGH; j++) {
      for (i = 0; i < (int)eWIDE; i++, pp++) {
	if (*pp == transparent_index)
	  wxiSetMask(theMask, i, j, 0);
	else
	  wxiSetMask(theMask, i, j, 1);
      }
    }
  }  

  if (numcols)
  switch (dispDEEP) 
    {
    case 8:
      {
      byte  *imagedata, *ip, *pp;
      int i;

      imagedata = (byte *) malloc(eWIDE*eHIGH);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      
      if (ncols==0) FloydDitherize8(imagedata);
      else {
	for (i=eWIDE*eHIGH, pp=epic, ip=imagedata; i>0; i--,pp++,ip++) {
	  *ip = (byte) cols[*pp];
	}
      }

      theImage = XCreateImage(theDisp,theVisual,dispDEEP,ZPixmap,0,
			      (char *) imagedata, eWIDE, eHIGH, 8, 0);
      if (!theImage) FatalError("couldn't create theImage!");
      }

    return;

    /*********************************/

    case 1:
      {
      byte  *imagedata;

      theImage = XCreateImage(theDisp, theVisual, dispDEEP, XYPixmap, 0, NULL, 
			      eWIDE, eHIGH, 8, 0);
      if (!theImage) FatalError("couldn't create theImage!");
      imagedata = (byte *) malloc(theImage->bytes_per_line * eHIGH);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      theImage->data = (char *) imagedata;
      FloydDitherize1(theImage);
      }

    return;
      
    /*********************************/
      
    case 4: {
      byte  *imagedata, *ip, *pp;
      byte *lip;
      int  bperline, half, j, i;

      theImage = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			      eWIDE, eHIGH, 8, 0);
      if (!theImage) {
	// FatalError("couldn't create theImage!");
	return;
      }

      bperline = theImage->bytes_per_line;
      imagedata = (byte *) malloc(bperline * eHIGH);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      theImage->data = (char *) imagedata;

      if (ncols==0) {            /* ditherize */
	byte *dith;
	dith = (byte *) malloc(eWIDE * eHIGH);
	if (!dith) FatalError("can't create dithered image");
	FloydDitherize8(dith);

	if (theImage->bits_per_pixel == 4) {
	  for (i=0, pp=dith, lip=imagedata; i < (int)eHIGH; i++, lip+=bperline) {
	    for (j=0, ip=lip, half=0; j < (int)eWIDE; j++,pp++,half++) {
	      if (half&1) { *ip = *ip + ((*pp&0x0f)<<4);  ip++; }
	      else *ip = *pp&0x0f;
	    }
	  }
	}
	else if (theImage->bits_per_pixel == 8)
	  memcpy(imagedata, dith, eWIDE*eHIGH);
	
	else {
	  // FatalError("This display is too bizarre.  Can't create XImage.");
	  theImage = NULL;
	  return;
	}

	free(dith);
      }

      else {     /* don't ditherize */
	if (theImage->bits_per_pixel == 4) {
	  for (i=0, pp=epic, lip=imagedata; i < (int)eHIGH; i++, lip+=bperline) {
	    for (j=0, ip=lip, half=0; j < (int)eWIDE; j++,pp++,half++) {
	      if (half&1) { *ip = *ip + ((cols[*pp]&0x0f)<<4);  ip++; }
	      else *ip = cols[*pp]&0x0f;
	    }
	  }
	}
	else if (theImage->bits_per_pixel == 8) {
	  for (i=eWIDE*eHIGH, pp=epic, ip=imagedata; i>0; i--,pp++,ip++) {
	    *ip = (byte) cols[*pp];
	  }
	}
	else {
	  theImage = NULL;
	  return;
	  // FatalError("This display's too bizarre.  Can't create XImage.");
	}
      }
      
      }

    return;
      
    /*********************************/
      
    case 6: {
      byte  *imagedata, *ip, *pp;
      int  bperline, i;

      theImage = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0, NULL, 
			      eWIDE, eHIGH, 8, 0);
      if (!theImage) {
	return;
	// FatalError("couldn't create theImage!");
      }

      if (theImage->bits_per_pixel != 8) {
	theImage = NULL;
	return;
	// FatalError("This display's too bizarre.  Can't create XImage.");
      }

      bperline = theImage->bytes_per_line;
      imagedata = (byte *) malloc(bperline * eHIGH);
      if (!imagedata) FatalError("couldn't malloc imagedata");
      theImage->data = (char *) imagedata;

      if (ncols==0) FloydDitherize8(imagedata);
      else {
	for (i=eWIDE*eHIGH, pp=epic, ip=imagedata; i>0; i--,pp++,ip++) {
	  *ip = (byte) cols[*pp];
	}
      }
      
      }
    return;
      
    }

  theImage = XCreateImage(theDisp, theVisual, dispDEEP, ZPixmap, 0,
			  NULL, eWIDE, eHIGH, 8, 0);
  {
    char *data;
    data = (char *)malloc(eHIGH * theImage->bytes_per_line);
    theImage->data = data;
  }

  {
    byte *pp = epic;
    int i, j;
    unsigned long wite;
    wite = WhitePixelOfScreen(DefaultScreenOfDisplay(theDisp));
    
    for (j = 0; j < (int)eHIGH; j++) {
      for (i = 0; i < (int)eWIDE; i++, pp++) {
	unsigned long pixel;
	if (numcols)
	  pixel = cols[*pp];
	else {
	  XColor c;

	  c.red   = (*pp++)<<8;
	  c.green = (*pp++)<<8;
	  c.blue  = (*pp)<<8;
	  c.flags = DoRed | DoGreen | DoBlue;
	
	  if (wxAllocColor(theDisp, theCmap, &c))
	    pixel = c.pixel;
	  else
	    pixel = wite;
	}

	XPutPixel(theImage, i, j, pixel);
      }
    }
  }
}

/***********************************/
void wxImage::FatalError (char *identifier)
{
  fprintf(stderr, "wxImage: %s\n", identifier);
  exit(-1);
}

/***********************************/
void wxImage::FreeMostResources()
{
  /* called when the program exits.  frees everything explictly created
     EXCEPT allocated colors.  This is used when 'useroot' is in operation,
     as we have to keep the alloc'd colors around, but we don't want anything
     else to stay */

  if (!theDisp) return;   /* called before connection opened */

  XFlush(theDisp);
}

void xvDestroyImage(XImage *image)
{
  /* called in place of XDestroyImage().  Explicitly destroys *BOTH* the
     data and the structure.  XDestroyImage() doesn't seem to do this on all
     systems.  Also, can be called with a NULL image pointer */

  if (image) {
    /* free data by hand, since XDestroyImage is vague about it */
    if (image->data) free(image->data);
    image->data = NULL;
    XDestroyImage(image);
  }
}

void xvbzero(char *s, int len)
{
  for ( ; len>0; len--) {
    *s++ = 0;
  }
}
