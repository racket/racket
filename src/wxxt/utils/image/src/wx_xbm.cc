/*
 * xvxbm.c - load routine for X11 Bitmap format pictures
 *
 * LoadXBM(fname)  -  loads an X11 Bitmap file\
 * WriteXBM(fp, pic, w, h)
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


#include <stdlib.h>
#include "wx_image.h"

#ifdef MZ_PRECISE_GC
END_XFORM_ARITH;
#endif

/*
 * File Format:
 *   (format identifier:  "#define" as first couple chars in file)
 *
 * looks for first line beginning with '#define'
 *   reads "#define identifier width"  (identifier is ignored)
 * looks for next line beginning with '#define'
 *   reads "#define identifier height" (identifier is ignored)
 * looks for next occurence of characters '0x'
 *   read next two chars as two hex digits
 *   move forward to next occurence of '0x'
 *   repeat
 */
 

/*******************************************/
int wxImage::LoadXBM(char *fname, int /* nc */)
/*******************************************/
{
  FILE  *fp;
  int    c, c1;
  int    i, j, k, bit, w, h;
  byte  *pix;
  long   filesize;
  char   line[256];
  byte   hex[256];

  k = 0;

  fp=fopen(fname,"r");
  if (!fp) return 1;

  /* figure out the file size (for Informational Purposes Only) */
  fseek(fp, 0L, 2);
  filesize = ftell(fp);
  fseek(fp, 0L, 0);


  /* read width:  skip lines until we hit a #define */
  while (1) {
    if (!fgets(line,256,fp)) {
      fclose(fp);
      return 1;
      // return(XBMError("EOF reached in header info."));
    }

    if (strncmp(line,"#define",7)==0) {
      if (sscanf(line,"#define %*s %d", &w) != 1) {
	fclose(fp);
        return 1;
	// return(XBMError("Unable to read 'width'"));
      }
      else break;
    }
  }


  /* read height:  skip lines until we hit another #define */
  while (1) {
    if (!fgets(line,256,fp)) {
      fclose(fp);
      return 1;
      // return(XBMError("EOF reached in header info."));
    }

    if (strncmp(line,"#define",7)==0) {
      if (sscanf(line,"#define %*s %d", &h) != 1) {
	fclose(fp);
        return 1;
	// return(XBMError("Unable to read 'height'"));
      }
      else break;
    }
  }



  /* scan forward until we see the first '0x' */
  c = getc(fp);  c1 = getc(fp);
  while (c1!=EOF && !(c=='0' && c1=='x') ) { c = c1;  c1 = getc(fp); }

  if (c1==EOF) {
    fclose(fp);
    return 1;
    // return(XBMError("No bitmap data found"));
  }


  /* load up the stuff XV expects us to load up */

//  SetISTR(ISTR_FORMAT,"X11 Bitmap  (%ld bytes)", filesize);

  {
    byte *ba;
    ba = (byte *) calloc(w*h,1);
    pic = ba;
  }
  if (!pic) FatalError("couldn't malloc 'pic'");

  pWIDE = w;  pHIGH = h;

  /* B/W bitmaps have a two entry colormap */
  r[0] = g[0] = b[0] = 255;     /* 0 = white */
  r[1] = g[1] = b[1] = 0;       /* 1 = black */


  /* initialize the 'hex' array for zippy ASCII-hex -> int conversion */

  for (i=0; i<256; i++) { hex[i]=0; }
  for (i='0'; i<='9'; i++) { hex[i] = i - '0'; }
  for (i='a'; i<='f'; i++) { hex[i] = i + 10 - 'a'; }
  for (i='A'; i<='F'; i++) { hex[i] = i + 10 - 'A'; }

  /* read/convert the image data */

  for (i=0, pix=pic; i<h; i++) {
    for (j=0,bit=0; j<w; j++, pix++, bit = ++bit&7) {

      if (!bit) {
	/* get next byte from file.  we're already positioned at it */
	c = getc(fp);  c1 = getc(fp);
	if (c<0 || c1<0) { 
	  /* EOF: break out of loop */	  
	  c=c1='0'; i=h; j=w;
//	  XBMError("The file would appear to be truncated.");
	}

	k = (hex[c] << 4) + hex[c1];

	/* advance to next '0x' */
	c = getc(fp);  c1 = getc(fp);
	while (c1!=EOF && !(c=='0' && c1=='x') ) { c = c1;  c1 = getc(fp); }
      }

      *pix = (k&1) ? 1 : 0;
      k = k >> 1;
    }
  }

  fclose(fp);

  return 0;
}  



/*******************************************/
int wxImage::WriteXBM(FILE *fp, byte *pic, int w, int h, char *fname)
{
  /* pic is expected to be an array of w*h bytes.  '0' is considered 'black'
     non-zero is considered white.  Some sort of stippling algorithm should've
     been called already to produce pic, otherwise the output won't be at all
     useful */

  int   i,j,k,bit,len,nbytes;
  byte *pix;
  char name[256], *foo;

  /* figure out a reasonable basename */
  strcpy(name,fname);
  foo = strchr(name,'.');
  if (foo) *foo='\0';                 /* truncated name at first '.' */

  fprintf(fp,"#define %s_width %d\n",name,w);  
  fprintf(fp,"#define %s_height %d\n",name,h);
  fprintf(fp,"static char %s_bits[] = {\n",name);

  fprintf(fp," ");

  nbytes = h * ((w+7)/8);   /* # of bytes to write */

  for (i=0, len=1, pix=pic; i<h; i++) {
    for (j=bit=k=0; j<w; j++,pix++) {
      k = (k>>1);
      if (*pix) k |= 0x80;
      bit++;
      if (bit==8) {
	fprintf(fp,"0x%02x",(byte) ~k);
	nbytes--;  len += 4;
	if (nbytes) { fprintf(fp,",");  len++; }
	if (len>72) { fprintf(fp,"\n ");  len=1; }
	bit = k = 0;
      }
    }

    if (bit) {
      k = k >> (8-bit);
      fprintf(fp,"0x%02x",(byte) ~k);
      nbytes--;  len += 4;
      if (nbytes) { fprintf(fp,",");  len++; }
      if (len>72) { fprintf(fp,"\n ");  len=1; }
    }
  }

  fprintf(fp,"};\n");

  if (ferror(fp)) return -1;
  return 0;
}
