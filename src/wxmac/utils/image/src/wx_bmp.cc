/*
 * xvbmp.c - i/o routines for .BMP files (MS Windows 3.x)
 *
 * LoadBMP(fname, numcols)
 * WriteBMP(fp, pic, ptype, w, h, r, g, b, numcols, style);
 */

/* ================
 * Copyright 1989, 1990, 1991, 1992, 1993 by John Bradley
 * 
 * Permission to use, copy, and distribute XV in its entirety, for 
 * non-commercial purposes, is hereby granted without fee, provided that
 * this license information and copyright notice appear in all copies.
 * 
 * Note that distributing XV 'bundled' in with ANY product is considered
 * to be a 'commercial purpose'.
 *
 * Also note that any copies of XV that are distributed MUST be built
 * and/or configured to be in their 'unregistered copy' mode, so that it
 * is made obvious to the user that XV is shareware, and that they should
 * consider donating, or at least reading this License Info.
 * 
 * The software may be modified for your own purposes, but modified
 * versions may NOT be distributed without prior consent of the author.
 * 
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the author be held liable for any damages
 * arising from the use of this software.
 * 
 * If you would like to do something with XV that this copyright
 * prohibits (such as distributing it with a commercial product, 
 * using portions of the source in some other program, etc.), please
 * contact the author (preferably via email).  Arrangements can
 * probably be worked out.
 *
 * XV is shareware for PERSONAL USE only.  You may use XV for your own
 * amusement, and if you find it nifty, useful, generally cool, or of
 * some value to you, your non-deductable donation would be greatly
 * appreciated.  $25 is the suggested donation, though, of course,
 * larger donations are quite welcome.  Folks who donate $25 or more
 * can receive a Real Nice bound copy of the XV manual for no extra
 * charge.
 * 
 * Commercial, government, and institutional users MUST register their
 * copies of XV, for the exceedingly REASONABLE price of just $25 per
 * workstation/X terminal.  Site licenses are available for those who
 * wish to run XV on a large number of machines.  Contact the author
 * for more details.
 *
 * The author may be contacted via:
 *    US Mail:  John Bradley
 *              1053 Floyd Terrace
 *              Bryn Mawr, PA  19010
 *
 *    Phone:    (215) 898-8813
 *    EMail:    bradley@cis.upenn.edu
 */


#include "common.h"
#include <stdlib.h>
#include "wx_image.h"
#include "wx_utils.h"

/* comments on error handling:
   a truncated file is not considered a Major Error.  The file is loaded, the
   rest of the pic is filled with 0's.

   a file with garbage characters in it is an unloadable file.  All allocated
   stuff is tossed, and LoadPBM returns non-zero

   not being able to malloc is a Fatal Error.  The program is aborted. */


#define BI_RGB  0
#define BI_RLE8 1
#define BI_RLE4 2

static long filesize;

  static int  loadBMP1(FILE *, byte *, int, int);
  static int  loadBMP4(FILE *, byte *, int, int, int);
  static int  loadBMP8(FILE *, byte *, int, int, int);
  static int  loadBMP24(FILE *, byte *, int, int);
  static unsigned int getshort(FILE *);
  static unsigned int getint(FILE *);
  static void putshort(FILE *, int);
  static void putint(FILE *, int);
  static void writeBMP1(FILE *, byte *, int, int);
  static void writeBMP4(FILE *, byte *, int, int);
  static void writeBMP8(FILE *, byte *, int, int);
  static void writeBMP24(FILE *, byte *, int, int);
  static int  bmpError(char *, char *);


/*******************************************/
int wxImage::LoadBMP(char *fname, PICINFO *pinfo)
/*******************************************/
{
  FILE         *fp;
  int          i, c, c1, rv;
  unsigned int bfSize, bfOffBits, biSize, biWidth, biHeight, biPlanes;
  unsigned int biBitCount, biCompression, biSizeImage, biXPelsPerMeter;
  unsigned int biYPelsPerMeter, biClrUsed, biClrImportant;
  char         *cmpstr;
  byte         *lpic24, *pic8;
  char          buf[512];

  /* returns '1' on success */

  pic8 = lpic24 = (byte *) NULL;

#ifdef wx_mac
  fp=fopen(fname,"rb");
#else
  fp=fopen(fname,"r");
#endif
  if (!fp) return (bmpError(fname, "couldn't open file"));
  
  fseek(fp, 0L, 2);      /* figure out the file size */
  filesize = ftell(fp);
  fseek(fp, 0L, 0);


  /* read the file type (first two bytes) */
  c = getc(fp);  c1 = getc(fp);
  if (c!='B' || c1!='M') { bmpError(fname,"file type != 'BM'"); goto ERROR; }

  bfSize = getint(fp);
  getshort(fp);         /* reserved and ignored */
  getshort(fp);
  bfOffBits = getint(fp);

  biSize          = getint(fp);
  biWidth         = getint(fp);
  biHeight        = getint(fp);
  biPlanes        = getshort(fp);
  biBitCount      = getshort(fp);
  biCompression   = getint(fp);
  biSizeImage     = getint(fp);
  biXPelsPerMeter = getint(fp);
  biYPelsPerMeter = getint(fp);
  biClrUsed       = getint(fp);
  biClrImportant  = getint(fp);
  

  if (dEBUG>1) {
    fprintf(stderr,"\nLoadBMP:\tbfSize=%d, bfOffBits=%d\n",bfSize,bfOffBits);
    fprintf(stderr,"\t\tbiSize=%d, biWidth=%d, biHeight=%d, biPlanes=%d\n",
	    biSize, biWidth, biHeight, biPlanes);
    fprintf(stderr,"\t\tbiBitCount=%d, biCompression=%d, biSizeImage=%d\n",
	    biBitCount, biCompression, biSizeImage);
    fprintf(stderr,"\t\tbiX,YPelsPerMeter=%d,%d  biClrUsed=%d, biClrImp=%d\n",
	    biXPelsPerMeter, biYPelsPerMeter, biClrUsed, biClrImportant);
  }

  if (ferror(fp)) { bmpError(fname,"EOF reached in file header"); goto ERROR; }


  /* error checking */
  if ((biBitCount!=1 && biBitCount!=4 && biBitCount!=8 && biBitCount!=24) || 
      biPlanes!=1 || biCompression>BI_RLE4) {

    sprintf(buf,"Bogus BMP File!  (bitCount=%d, Planes=%d, Compression=%d)",
	    biBitCount, biPlanes, biCompression);

    bmpError(fname, buf);
    goto ERROR;
  }

  if (((biBitCount==1 || biBitCount==24) && biCompression != BI_RGB) ||
      (biBitCount==4 && biCompression==BI_RLE8) ||
      (biBitCount==8 && biCompression==BI_RLE4)) {

    sprintf(buf,"Bogus BMP File!  (bitCount=%d, Compression=%d)",
	    biBitCount, biCompression);

    bmpError(fname, buf);
    goto ERROR;
  }


  
  /* skip ahead to colormap, using biSize */
  c = biSize - 40;    /* 40 bytes read from biSize to biClrImportant */
  for (i=0; i<c; i++) {
    getc(fp);
  }


  /* load up colormap, if any */
  if (biBitCount!=24) {
    int i, cmaplen, c;
    
    if ((biBitCount < 16) && biClrUsed)
      cmaplen = biClrUsed;
    else
      cmaplen = 1 << biBitCount;
    numcols = cmaplen;
    for (i=0; i<cmaplen; i++) {
      c = getc(fp);
      pinfo->b[i] = c;
      c = getc(fp);
      pinfo->g[i] = c;
      c = getc(fp);
      pinfo->r[i] = c;
      getc(fp);         /* unused */
    }
    /* In case the file is corrupt, fill out the colormap
       with black: */
    while (i < 256) {
      pinfo->r[i] = 0;
      pinfo->g[i] = 0;
      pinfo->b[i] = 0;
      i++;
    }

    if (ferror(fp)) 
      { bmpError(fname,"EOF reached in BMP colormap"); goto ERROR; }

    if (dEBUG>1) {
      fprintf(stderr,"LoadBMP:  BMP colormap:  (RGB order)\n");
      for (i=0; i<cmaplen; i++) {
	fprintf(stderr,"%02x%02x%02x  ", pinfo->r[i],pinfo->g[i],pinfo->b[i]);
      }
      fprintf(stderr,"\n\n");
    }
  }


  /* create pic8 or pic24 */

  if (biBitCount==24) {
    void *v;
    v = new WXGC_ATOMIC char[biWidth * biHeight * 3];
    lpic24 = (byte *) v;
    if (!lpic24) {
      fclose(fp);
      return (bmpError(fname, "couldn't malloc 'pic24'"));
    }
  }
  else {
    void *v;
    v = new WXGC_ATOMIC char[biWidth * biHeight];
    pic8 = (byte *) v;
    if (!pic8) {
      fclose(fp);
      return(bmpError(fname, "couldn't malloc 'pic8'"));
    }
  }

  /* load up the image */
  if      (biBitCount == 1) rv = loadBMP1(fp,pic8,biWidth,biHeight);
  else if (biBitCount == 4) rv = loadBMP4(fp,pic8,biWidth,biHeight,
					  biCompression);
  else if (biBitCount == 8) rv = loadBMP8(fp,pic8,biWidth,biHeight,
					  biCompression);
  else                      rv = loadBMP24(fp,lpic24,biWidth,biHeight);

  if (rv) bmpError(fname, "File appears truncated.  Winging it.\n");

  fclose(fp);


  if (biBitCount == 24) {
    pinfo->pic  = lpic24;
    pinfo->type = PIC24;
  }
  else {
    pinfo->pic  = pic8;
    pinfo->type = PIC8;
  }

  cmpstr = "";
  if      (biCompression == BI_RLE4) cmpstr = ", RLE4 compressed";
  else if (biCompression == BI_RLE8) cmpstr = ", RLE8 compressed";

  pinfo->w = biWidth;  pinfo->h = biHeight;
  pinfo->frmType = F_BMP;
  pinfo->colType = F_FULLCOLOR;

  sprintf(pinfo->fullInfo, "BMP, %d bit%s per pixel%s.  (%ld bytes)",
	  biBitCount,  (biBitCount == 1) ? "" : "s",
	  cmpstr, filesize);
  sprintf(pinfo->shrtInfo, "%dx%d BMP.", biWidth, biHeight);
  pinfo->comment = (char *) NULL;
//  cout << pinfo->fullInfo << "\n";

  return 1;


 ERROR:
  fclose(fp);
  return 0;
}  


/*******************************************/
static int loadBMP1(FILE *fp, byte *pic8, int w, int h)
{
  int   i,j,c,bitnum,padw;
  long pp;

  c = 0;
  padw = ((w + 31)/32) * 32;  /* 'w', padded to be a multiple of 32 */

  for (i=h-1; i>=0; i--) {
    pp = (i * w);
    for (j=bitnum=0; j<padw; j++,bitnum++) {
      if ((bitnum&7) == 0) { /* read the next byte */
	c = getc(fp);
	bitnum = 0;
      }
      
      if (j<w) {
	pic8[pp++] = (c & 0x80) ? 1 : 0;
	c <<= 1;
      }
    }
    if (ferror(fp)) break;
  }

  return (ferror(fp));
}  



/*******************************************/
static int loadBMP4(FILE *fp, byte *pic8, int w, int h, int comp)
{
  int   i,j,c,c1,x,y,nybnum,padw,rv;
  long pp;


  rv = 0;
  c = c1 = 0;

  if (comp == BI_RGB) {   /* read uncompressed data */
    padw = ((w + 7)/8) * 8; /* 'w' padded to a multiple of 8pix (32 bits) */

    for (i=h-1; i>=0; i--) {
      pp = (i * w);
      for (j=nybnum=0; j<padw; j++,nybnum++) {
		if ((nybnum & 1) == 0) { /* read next byte */
		  c = getc(fp);
		  nybnum = 0;
		}

		if (j<w) {
		  pic8[pp++] = (c & 0xf0) >> 4;
		  c <<= 4;
		}
      }
      if (ferror(fp)) break;
    }
  }

  else if (comp == BI_RLE4) {  /* read RLE4 compressed data */
    x = y = 0;  
    pp = x + (h-y-1)*w;

    while (y<h) {
      c = getc(fp);  if (c == EOF) { rv = 1;  break; }

      if (c) {                                   /* encoded mode */
	c1 = getc(fp);
	for (i=0; i<c; i++,x++,pp++)  {
	  pic8[pp] = (i&1) ? (c1 & 0x0f) : ((c1>>4)&0x0f);
	}
      }

      else {    /* c==0x00  :  escape codes */
	c = getc(fp);  if (c == EOF) { rv = 1;  break; }

	if      (c == 0x00) {                    /* end of line */
	  x=0;  y++;  pp = x + (h-y-1)*w;
	} 

	else if (c == 0x01) break;               /* end of pic8 */

	else if (c == 0x02) {                    /* delta */
	  c = getc(fp);  x += c;
	  c = getc(fp);  y += c;
	  pp = x + (h-y-1)*w;
	}

	else {                                   /* absolute mode */
	  for (i=0; i<c; i++, x++, pp++) {
	    if ((i&1) == 0) c1 = getc(fp);
	    pic8[pp] = (i&1) ? (c1 & 0x0f) : ((c1>>4)&0x0f);
	  }
	  
	  if (((c&3)==1) || ((c&3)==2)) getc(fp);  /* read pad byte */
	}
      }  /* escape processing */
      if (ferror(fp)) break;
    }  /* while */
  }
  
  else {
    fprintf(stderr,"unknown BMP compression type 0x%0x\n", comp);
  }

  if (ferror(fp)) rv = 1;
  return rv;
}  



/*******************************************/
static int loadBMP8(FILE *fp, byte *pic8, int w, int h, int comp)
{
  int   i,j,c,c1,padw,x,y,rv;
  long pp;

  rv = 0;

  if (comp == BI_RGB) {   /* read uncompressed data */
    padw = ((w + 3)/4) * 4; /* 'w' padded to a multiple of 4pix (32 bits) */

    for (i=h-1; i>=0; i--) {
      pp = (i * w);

      for (j=0; j<padw; j++) {
		c = getc(fp);  
		if (c==EOF) 
			rv = 1;
		if (j<w) 
			pic8[pp++] = c;
      }
      if (ferror(fp)) break;
    }
  }

  else if (comp == BI_RLE8) {  /* read RLE8 compressed data */
    x = y = 0;  
    pp = x + (h-y-1)*w;

    while (y<h) {
      c = getc(fp);  if (c == EOF) { rv = 1;  break; }

      if (c) {                                   /* encoded mode */
	c1 = getc(fp);
	for (i=0; i<c; i++,x++,pp++) {
	  pic8[pp] = c1;
	}
      }

      else {    /* c==0x00  :  escape codes */
	c = getc(fp);  if (c == EOF) { rv = 1;  break; }

	if      (c == 0x00) {                    /* end of line */
	  x=0;  y++;  pp = x + (h-y-1)*w;
	} 

	else if (c == 0x01) break;               /* end of pic8 */

	else if (c == 0x02) {                    /* delta */
	  c = getc(fp);  x += c;
	  c = getc(fp);  y += c;
	  pp = x + (h-y-1)*w;
	}

	else {                                   /* absolute mode */
	  for (i=0; i<c; i++, x++, pp++) {
	    c1 = getc(fp);
	    pic8[pp] = c1;
	  }
	  
	  if (c & 1) getc(fp);  /* odd length run: read an extra pad byte */
	}
      }  /* escape processing */
      if (ferror(fp)) break;
    }  /* while */
  }
  
  else {
    fprintf(stderr,"unknown BMP compression type 0x%0x\n", comp);
  }

  if (ferror(fp)) rv = 1;
  return rv;
}  



/*******************************************/
static int loadBMP24(FILE *fp, byte *pic24, int w, int h)
{
  int   i,j,padb;
  long pp;
  int c;

  padb = (4 - ((w*3) % 4)) & 0x03;  /* # of pad bytes to read at EOscanline */

  for (i=h-1; i>=0; i--) {
    pp = (i * w * 3);
    
    for (j=0; j<w; j++) {
      c = getc(fp);   /* red   */
      pic24[pp++] = c;
      c = getc(fp);   /* green */
      pic24[pp++] = c;
      c = getc(fp);   /* blue  */
      pic24[pp++] = c;
    }

    for (j=0; j<padb; j++) {
      getc(fp);
    }

    if (ferror(fp)) break;
  }

  return (ferror(fp));
}  



/*******************************************/
static unsigned int getshort(FILE *fp)
{
  int c, c1;
  c = getc(fp);  c1 = getc(fp);
  return ((unsigned int) c) + (((unsigned int) c1) << 8);
}


/*******************************************/
static unsigned int getint(FILE *fp)
{
  int c, c1, c2, c3;
  c = getc(fp);  c1 = getc(fp);  c2 = getc(fp);  c3 = getc(fp);
  return ((unsigned int) c) +
         (((unsigned int) c1) << 8) + 
	 (((unsigned int) c2) << 16) +
	 (((unsigned int) c3) << 24);
}


/*******************************************/
static void putshort(FILE *fp, int i)
{
  int c, c1;

  c = ((unsigned int ) i) & 0xff;  c1 = (((unsigned int) i)>>8) & 0xff;
  putc(c, fp);   putc(c1,fp);
}


/*******************************************/
static void putint(FILE *fp, int i)
{
  int c, c1, c2, c3;
  c  = ((unsigned int ) i)      & 0xff;  
  c1 = (((unsigned int) i)>>8)  & 0xff;
  c2 = (((unsigned int) i)>>16) & 0xff;
  c3 = (((unsigned int) i)>>24) & 0xff;

  putc(c, fp);   putc(c1,fp);  putc(c2,fp);  putc(c3,fp);
}




static byte pc2nc[256],r1[256],g1[256],b1[256];


/*******************************************/
int wxImage::WriteBMP(FILE *fp, byte *pic824, int ptype, int w, int h, byte *rmap, byte *gmap, byte *bmap, int numcols, int colorstyle)
{
  /*
   * if PIC8, and colorstyle == F_FULLCOLOR, F_GREYSCALE, or F_REDUCED,
   * the program writes an uncompressed 4- or 8-bit image (depending on
   * the value of numcols)
   *
   * if PIC24, and colorstyle == F_FULLCOLOR, program writes an uncompressed
   *    24-bit image
   * if PIC24 and colorstyle = F_GREYSCALE, program writes an uncompressed
   *    8-bit image
   * note that PIC24 and F_BWDITHER/F_REDUCED won't happen
   *
   * if colorstyle == F_BWDITHER, it writes a 1-bit image 
   *
   */

  int i,j, nc, nbits, bperlin, cmaplen;
  byte *graypic, graymap[256];
  long sp, dp;

  nc = nbits = cmaplen = 0;
  graypic = NULL;

  if (ptype == PIC24 && colorstyle == F_GREYSCALE) {
    /* generate a faked 8-bit per pixel image with a grayscale cmap,
       so that it can just fall through existing 8-bit code */
    void *_gp;

    _gp = new WXGC_ATOMIC  char[w*h];
    graypic = (byte *) _gp;
    if (!graypic) wxFatalError("unable to malloc in WriteBMP()");

    for (i=0,sp=0,dp=0; i<w*h; i++,sp+=3, dp++) {
      graypic[dp] = MONO(pic824[sp],pic824[sp+1],pic824[sp+2]);
    }

    for (i=0; i<256; i++) {
      graymap[i] = i;
    }
    rmap = gmap = bmap = graymap;
    numcols = 256;
    ptype = PIC8;

    pic824 = graypic;
  }


  if (ptype == PIC24) {  /* is F_FULLCOLOR */
    nbits = 24;
    cmaplen = 0;
    nc = 0;
  }

  else if (ptype == PIC8) {
    /* we may have duplicate colors in the colormap, and we'd prefer not to.
     * build r1,g1,b1 (a contiguous, minimum set colormap), and pc2nc[], a
     * array that maps 'pic8' values (0-numcols) into corresponding values
     * in the r1,g1,b1 colormaps (0-nc)
     */

    for (i=0; i<256; i++) { pc2nc[i] = r1[i] = g1[i] = b1[i] = 0; }

    nc = 0;
    for (i=0; i<numcols; i++) {
      /* see if color #i is a duplicate */
      for (j=0; j<i; j++) {
	if (rmap[i] == rmap[j] && gmap[i] == gmap[j] && 
	    bmap[i] == bmap[j]) break;
      }

      if (j==i) {  /* wasn't found */
	pc2nc[i] = nc;
	r1[nc] = rmap[i];
	g1[nc] = gmap[i];
	b1[nc] = bmap[i];
	nc++;
      }
      else pc2nc[i] = pc2nc[j];
    }

    /* determine how many bits per pixel we'll be writing */
    if (colorstyle == F_BWDITHER || nc <= 2) nbits = 1;
    else if (nc<=16) nbits = 4;
    else nbits = 8;

    cmaplen = 1<<nbits;                      /* # of entries in cmap */
  }


  bperlin = ((w * nbits + 31) / 32) * 4;   /* # bytes written per line */

  putc('B', fp);  putc('M', fp);           /* BMP file magic number */

  /* compute filesize and write it */
  i = 14 +                /* size of bitmap file header */
      40 +                /* size of bitmap info header */
      (cmaplen * 4) +     /* size of colormap */
      bperlin * h;        /* size of image data */

  putint(fp, i);
  putshort(fp, 0);        /* reserved1 */
  putshort(fp, 0);        /* reserved2 */
  putint(fp, 14 + 40 + (cmaplen * 4));  /* offset from BOfile to BObitmap */

  putint(fp, 40);         /* biSize: size of bitmap info header */
  putint(fp, w);          /* biWidth */
  putint(fp, h);          /* biHeight */
  putshort(fp, 1);        /* biPlanes:  must be '1' */
  putshort(fp, nbits);    /* biBitCount: 1,4,8, or 24 */
  putint(fp, BI_RGB);     /* biCompression:  BI_RGB, BI_RLE8 or BI_RLE4 */
  putint(fp, bperlin*h);  /* biSizeImage:  size of raw image data */
  putint(fp, 75 * 39);    /* biXPelsPerMeter: (75dpi * 39" per meter) */
  putint(fp, 75 * 39);    /* biYPelsPerMeter: (75dpi * 39" per meter) */
  putint(fp, nc);         /* biClrUsed: # of colors used in cmap */
  putint(fp, nc);         /* biClrImportant: same as above */


  /* write out the colormap */
  for (i=0; i<cmaplen; i++) {
    if (colorstyle == F_GREYSCALE) {
      j = MONO(r1[i],g1[i],b1[i]);
      putc(j,fp);  putc(j,fp);  putc(j,fp);  putc(0,fp);
    }
    else {
      putc(b1[i],fp);
      putc(g1[i],fp);
      putc(r1[i],fp);
      putc(0,fp);
    }
  }

  /* write out the image */
  if      (nbits ==  1) writeBMP1 (fp, pic824, w, h);
  else if (nbits ==  4) writeBMP4 (fp, pic824, w, h);
  else if (nbits ==  8) writeBMP8 (fp, pic824, w, h);
  else if (nbits == 24) writeBMP24(fp, pic824, w, h);

  if (ferror(fp)) return -1;
  
  return 0;
}


	  
	  
/*******************************************/
static void writeBMP1(FILE *fp, byte *pic8, int w, int h)
{
  int   i,j,c,bitnum,padw;
  long pp;

  padw = ((w + 31)/32) * 32;  /* 'w', padded to be a multiple of 32 */

  for (i=h-1; i>=0; i--) {
    pp = (i * w);  

    for (j=bitnum=c=0; j<=padw; j++,bitnum++) {
      if (bitnum == 8) { /* write the next byte */
	putc(c,fp);
	bitnum = c = 0;
      }
      
      c <<= 1;

      if (j<w) {
	c |= (pc2nc[pic8[pp++]] & 0x01);
      }
    }
  }
}  



/*******************************************/
static void writeBMP4(FILE *fp, byte *pic8, int w, int h)
{
  int   i,j,c,nybnum,padw;
  long pp;


  padw = ((w + 7)/8) * 8; /* 'w' padded to a multiple of 8pix (32 bits) */

  for (i=h-1; i>=0; i--) {
    pp = (i * w);

    for (j=nybnum=c=0; j<=padw; j++,nybnum++) {
      if (nybnum == 2) { /* write next byte */
	putc((c&0xff), fp);
	nybnum = c = 0;
      }

      c <<= 4;

      if (j<w) {
	c |= (pc2nc[pic8[pp]] & 0x0f);
	pp++;
      }
    }
  }
}  



/*******************************************/
static void writeBMP8(FILE *fp, byte *pic8, int w, int h)
{
  int   i,j,padw;
  long pp;

  padw = ((w + 3)/4) * 4; /* 'w' padded to a multiple of 4pix (32 bits) */

  for (i=h-1; i>=0; i--) {
    pp = (i * w);

    for (j=0; j<w; j++) {
      putc(pc2nc[pic8[pp++]], fp);
    }
    for ( ; j<padw; j++) {
      putc(0, fp);
    }
  }
}  


/*******************************************/
static void writeBMP24(FILE *fp, byte *pic24, int w, int h)
{
  int   i,j,padb;
  long pp;

  padb = (4 - ((w*3) % 4)) & 0x03;  /* # of pad bytes to write at EOscanline */

  for (i=h-1; i>=0; i--) {
    pp = (i * w * 3);

    for (j=0; j<w; j++) {
      putc(pic24[pp++], fp);
      putc(pic24[pp++], fp);
      putc(pic24[pp++], fp);
    }

    for (j=0; j<padb; j++) {
      putc(0, fp);
    }
  }
}  






/*******************************************/
static int bmpError(char *fname, char *st)
{
  fprintf(stderr, "wxImage: %s: %s\n", fname, st);
  return 0;
}


#ifdef wx_mac
unsigned short Mac_xform24(unsigned char c);

unsigned short Mac_xform24(unsigned char c) 
{
	return c | (c << 8);
}
// Create an Image, create a bitMap (Gworld for the data), copy the image to the
// bitmap, delete the image
Bool wxLoadBMPIntoBitmap(char *fileName, wxBitmap *bm, wxColourMap **pal)
{
	PICINFO		picinfo;		// defined in wx_imgx.h
	wxImage *xbmImage;
	xbmImage = new wxImage();
	if (xbmImage->LoadBMP(fileName, &picinfo) == 1) {
		// CreateOffScreenPixMap(&colorPort, gifImage);
		Rect bounds = {0, 0, picinfo.h, picinfo.w};
		GDHandle savegw;
		CGrafPtr saveport;
		QDErr err;
		GWorldPtr	newGWorld;
		RGBColor	cpix;
		int y, x;
		unsigned int abyte;
		long buf;

		GetGWorld(&saveport, &savegw);

		err = NewGWorld(&newGWorld, 32, &bounds, NULL, NULL, 0);
		if (err) {
			bm->SetOk(FALSE);
			return FALSE;
		}
		LockPixels(GetGWorldPixMap(newGWorld));
		SetGWorld(newGWorld, 0);
		bm->x_pixmap = newGWorld;

		buf = 0;
 		GetForeColor(&cpix);	// probably 0,0,0
 		::EraseRect(&bounds);
 		if (picinfo.type == PIC8) {
 			for (y = 0; y < picinfo.h; y++) {
			  for (x = 0; x < picinfo.w; x++) {
			    abyte = picinfo.pic[buf++];
			    cpix.red = Mac_xform24(picinfo.r[abyte]);
			    cpix.green = Mac_xform24(picinfo.g[abyte]);
			    cpix.blue = Mac_xform24(picinfo.b[abyte]);
			    ::SetCPixel(x, y, &cpix);
			  }
 			}
 		}
 		else { // must be 24 bit?
 			for (y = 0; y < picinfo.h; y++) {
 				for (x = 0; x < picinfo.w; x++) {
 					cpix.blue = Mac_xform24(picinfo.pic[buf++]);
 					cpix.green = Mac_xform24(picinfo.pic[buf++]);
 					cpix.red = Mac_xform24(picinfo.pic[buf++]);
 					::SetCPixel(x, y, &cpix);
 				}
 			}
		}
		SetGWorld(saveport, savegw);
			
		//  bm->pixmap = colorPort->portPixMap;
		bm->SetWidth(picinfo.w);
		bm->SetHeight(picinfo.h);
		bm->SetDepth(picinfo.type==PIC8 ? 8 : 24);
		bm->SetOk(TRUE);
		DELETE_OBJ xbmImage;
		return TRUE;
	}
	else {
		bm->SetOk(FALSE);
		return FALSE;
	}
}
#endif
