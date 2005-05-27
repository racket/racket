/*
 * xvgifwr.c  -  handles writing of GIF files.  based on flgife.c and
 *               flgifc.c from the FBM Library, by Michael Maudlin
 *
 * Contains: 
 *   WriteGIF(fp, pic, w, h, rmap, gmap, bmap, numcols, colorstyle)
 *
 * Note: slightly brain-damaged, in that it'll only write non-interlaced 
 *       GIF files (in the interests of speed, or something)
 *
 */



/*****************************************************************
 * Portions of this code Copyright (C) 1989 by Michael Mauldin.
 * Permission is granted to use this file in whole or in part provided
 * that you do not sell it for profit and that this copyright notice
 * and the names of all authors are retained unchanged.
 *
 * Authors:  Michael Mauldin (mlm@cs.cmu.edu)
 *           David Rowley (mgardi@watdcsu.waterloo.edu)
 *
 * Based on: compress.c - File compression ala IEEE Computer, June 1984.
 *
 *	Spencer W. Thomas       (decvax!harpo!utah-cs!utah-gr!thomas)
 *	Jim McKie               (decvax!mcvax!jim)
 *	Steve Davies            (decvax!vax135!petsd!peora!srd)
 *	Ken Turkowski           (decvax!decwrl!turtlevax!ken)
 *	James A. Woods          (decvax!ihnp4!ames!jaw)
 *	Joe Orost               (decvax!vax135!petsd!joe)
 *****************************************************************/
 
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

typedef long int        count_int;

static int  Width, Height;
static int  curx, cury;
static long CountDown;
static int  Interlace;
static byte bw[2] = {0, 0xff};

static void putword(int, FILE *);
static void compress(int, FILE *, byte *, int);
static void output(int);
static void cl_block(void);
static void cl_hash(count_int);
static void char_init(void);
static void char_out(int);
static void flush_char(void);

/*************************************************************/
int wxImage::WriteGIF(FILE *fp, byte *pic, int w, int h, byte *rmap, byte *gmap, byte *bmap, int numcols, int colorstyle)
{
  int RWidth, RHeight;
  int LeftOfs, TopOfs;
  int Resolution, ColorMapSize, InitCodeSize, Background, BitsPerPixel;
  int i,j;


  /* if writing B/W stipple... */
  if (colorstyle==2) {
    rmap = gmap = bmap = bw;
    numcols = 2;
  }

  Interlace = 0;
  Background = 0;

  /* figure out 'BitsPerPixel' */
  for (i=1; i<8; i++) {
    if ( (1<<i) >= numcols)
      break;
  }
  
  BitsPerPixel = i;

  ColorMapSize = 1 << BitsPerPixel;
	
  RWidth  = Width  = w;
  RHeight = Height = h;
  LeftOfs = TopOfs = 0;
	
  Resolution = BitsPerPixel;

  CountDown = w * h;    /* # of pixels we'll be doing */

  if (BitsPerPixel <= 1) InitCodeSize = 2;
                    else InitCodeSize = BitsPerPixel;

  curx = cury = 0;

  if (!fp) {
    fprintf(stderr,  "WriteGIF: file not open for writing\n" );
    return (1);
  }

  if (imgDEBUG) 
    fprintf(stderr,"WrGIF: pic=%lx, w,h=%dx%d, numcols=%d, Bits%d,Cmap=%d\n",
	    (long)pic, w,h,numcols,BitsPerPixel,ColorMapSize);

  fwrite("GIF87a", 1, 6, fp);    /* the GIF magic number */

  putword(RWidth, fp);           /* screen descriptor */
  putword(RHeight, fp);

  i = 0x80;	                 /* Yes, there is a color map */
  i |= (8-1)<<4;                 /* OR in the color resolution (hardwired 8) */
  i |= (BitsPerPixel - 1);       /* OR in the # of bits per pixel */
  fputc(i,fp);          

  fputc(Background, fp);         /* background color */

  fputc(0, fp);                  /* future expansion byte */


  if (colorstyle == 1) {         /* greyscale */
    for (i=0; i<ColorMapSize; i++) {
      j = MONO(rmap[i], gmap[i], bmap[i]);
      fputc(j, fp);
      fputc(j, fp);
      fputc(j, fp);
    }
  }
  else {
    for (i=0; i<ColorMapSize; i++) {       /* write out Global colormap */
      fputc(rmap[i], fp);
      fputc(gmap[i], fp);
      fputc(bmap[i], fp);
    }
  }

  fputc( ',', fp );              /* image separator */

  /* Write the Image header */
  putword(LeftOfs, fp);
  putword(TopOfs,  fp);
  putword(Width,   fp);
  putword(Height,  fp);
  if (Interlace) fputc(0x40, fp);   /* Use Global Colormap, maybe Interlace */
            else fputc(0x00, fp);

  fputc(InitCodeSize, fp);
  compress(InitCodeSize+1, fp, pic, w*h);

  fputc(0,fp);                      /* Write out a Zero-length packet (EOF) */
  fputc(';',fp);                    /* Write GIF file terminator */

  return (0);
}




/******************************/
static void putword(int w, FILE *fp)
{
  /* writes a 16-bit integer in GIF order (LSB first) */
  fputc(w & 0xff, fp);
  fputc((w>>8)&0xff, fp);
}




/***********************************************************************/


static unsigned long cur_accum = 0;
static int           cur_bits = 0;




// #define min(a,b)        ((a>b) ? b : a)

#define GIFBITS	12
#define MSDOS	1

#define HSIZE  5003            /* 80% occupancy */

typedef unsigned char   char_type;


static int n_bits;                   /* number of bits/code */
static int maxbits = GIFBITS;           /* user settable max # bits/code */
static int maxcode;                  /* maximum code, given n_bits */
static int maxmaxcode = 1 << GIFBITS;   /* NEVER generate this */

#define MAXCODE(n_bits)     ( (1 << (n_bits)) - 1)

static  count_int      htab [HSIZE];
static  unsigned short codetab [HSIZE];
#define HashTabOf(i)   htab[i]
#define CodeTabOf(i)   codetab[i]

static int hsize = HSIZE;            /* for dynamic table sizing */

/*
 * To save much memory, we overlay the table used by compress() with those
 * used by decompress().  The tab_prefix table is the same size and type
 * as the codetab.  The tab_suffix table needs 2**GIFBITS characters.  We
 * get this from the beginning of htab.  The output stack uses the rest
 * of htab, and contains characters.  There is plenty of room for any
 * possible stack (stack used to be 8000 characters).
 */

#define tab_prefixof(i) CodeTabOf(i)
#define tab_suffixof(i)        ((char_type *)(htab))[i]
#define de_stack               ((char_type *)&tab_suffixof(1<<GIFBITS))

static int free_ent = 0;                  /* first unused entry */

/*
 * block compression parameters -- after all codes are used up,
 * and compression rate changes, start over.
 */
static int clear_flg = 0;

static long int in_count = 1;            /* length of input */
static long int out_count = 0;           /* # of codes output (for debugging) */

/*
 * compress stdin to stdout
 *
 * Algorithm:  use open addressing double hashing (no chaining) on the 
 * prefix code / next character combination.  We do a variant of Knuth's
 * algorithm D (vol. 3, sec. 6.4) along with G. Knott's relatively-prime
 * secondary probe.  Here, the modular division first probe is gives way
 * to a faster exclusive-or manipulation.  Also do block compression with
 * an adaptive reset, whereby the code table is cleared when the compression
 * ratio decreases, but after the table fills.  The variable-length output
 * codes are re-sized at this point, and a special CLEAR code is generated
 * for the decompressor.  Late addition:  construct the table according to
 * file size for noticeable speed improvement on small files.  Please direct
 * questions about this implementation to ames!jaw.
 */

static int g_init_bits;
static FILE *g_outfile;

static int ClearCode;
static int EOFCode;


/********************************************************/
static void compress(int init_bits, FILE *outfile, byte *data, int len)
{
  register long fcode;
  register int i = 0;
  register int c;
  register int ent;
  register int disp;
  register int hsize_reg;
  register int hshift;

  /*
   * Set up the globals:  g_init_bits - initial number of bits
   *                      g_outfile   - pointer to output file
   */
  g_init_bits = init_bits;
  g_outfile   = outfile;

  /* initialize 'compress' globals */
  maxbits = GIFBITS;
  maxmaxcode = 1<<GIFBITS;
  memset((char *) htab, 0, sizeof(htab));
  memset((char *) codetab, 0, sizeof(codetab));
  hsize = HSIZE;
  free_ent = 0;
  clear_flg = 0;
  in_count = 1;
  out_count = 0;
  cur_accum = 0;
  cur_bits = 0;


  /*
   * Set up the necessary values
   */
  out_count = 0;
  clear_flg = 0;
  in_count = 1;
  maxcode = MAXCODE(n_bits = g_init_bits);

  ClearCode = (1 << (init_bits - 1));
  EOFCode = ClearCode + 1;
  free_ent = ClearCode + 2;

  char_init();
  ent = *data++;  len--;

  hshift = 0;
  for ( fcode = (long) hsize;  fcode < 65536L; fcode *= 2L ) {
    hshift++;
  }
  hshift = 8 - hshift;                /* set hash code range bound */

  hsize_reg = hsize;
  cl_hash( (count_int) hsize_reg);            /* clear hash table */

  output(ClearCode);
    
  while (len) {
    c = *data++;  len--;
    in_count++;

    fcode = (long) ( ( (long) c << maxbits) + ent);
    i = (((int) c << hshift) ^ ent);    /* xor hashing */

    if ( HashTabOf (i) == fcode ) {
      ent = CodeTabOf (i);
      continue;
    }

    else if ( (long)HashTabOf (i) < 0 )      /* empty slot */
      goto nomatch;

    disp = hsize_reg - i;           /* secondary hash (after G. Knott) */
    if ( i == 0 )
      disp = 1;

probe:
    if ( (i -= disp) < 0 )
      i += hsize_reg;

    if ( HashTabOf (i) == fcode ) {
      ent = CodeTabOf (i);
      continue;
    }

    if ( (long)HashTabOf (i) > 0 ) 
      goto probe;

nomatch:
    output(ent);
    out_count++;
    ent = c;

    if ( free_ent < maxmaxcode ) {
      CodeTabOf (i) = free_ent++; /* code -> hashtable */
      HashTabOf (i) = fcode;
    }
    else
      cl_block();
  }

  /* Put out the final code */
  output(ent);
  out_count++;
  output(EOFCode);
}


/*****************************************************************
 * TAG( output )
 *
 * Output the given code.
 * Inputs:
 *      code:   A n_bits-bit integer.  If == -1, then EOF.  This assumes
 *              that n_bits =< (long)wordsize - 1.
 * Outputs:
 *      Outputs code to the file.
 * Assumptions:
 *      Chars are 8 bits long.
 * Algorithm:
 *      Maintain a BITS character long buffer (so that 8 codes will
 * fit in it exactly).  Use the VAX insv instruction to insert each
 * code in turn.  When the buffer fills up empty it and start over.
 */

static
unsigned long masks[] = { 0x0000, 0x0001, 0x0003, 0x0007, 0x000F,
                                  0x001F, 0x003F, 0x007F, 0x00FF,
                                  0x01FF, 0x03FF, 0x07FF, 0x0FFF,
                                  0x1FFF, 0x3FFF, 0x7FFF, 0xFFFF };

static void output(int code)
{
  cur_accum &= masks[cur_bits];

  if (cur_bits > 0)
    cur_accum |= ((long)code << cur_bits);
  else
    cur_accum = code;
	
  cur_bits += n_bits;

  while( cur_bits >= 8 ) {
    char_out( (unsigned int) (cur_accum & 0xff) );
    cur_accum >>= 8;
    cur_bits -= 8;
  }

  /*
   * If the next entry is going to be too big for the code size,
   * then increase it, if possible.
   */

  if (free_ent > maxcode || clear_flg) {

    if( clear_flg ) {
      maxcode = MAXCODE (n_bits = g_init_bits);
      clear_flg = 0;
    }
    else {
      n_bits++;
      if ( n_bits == maxbits )
	maxcode = maxmaxcode;
      else
	maxcode = MAXCODE(n_bits);
    }
  }
	
  if( code == EOFCode ) {
    /* At EOF, write the rest of the buffer */
    while( cur_bits > 0 ) {
      char_out( (unsigned int)(cur_accum & 0xff) );
      cur_accum >>= 8;
      cur_bits -= 8;
    }

    flush_char();
	
    fflush( g_outfile );

    if( ferror( g_outfile ) )
    {
      fprintf(stderr, "Unable to write GIF file\n");
      exit(1);
    }
  }
}


/********************************/
static void cl_block ()             /* table clear for block compress */
{
  /* Clear out the hash table */

  cl_hash ( (count_int) hsize );
  free_ent = ClearCode + 2;
  clear_flg = 1;

  output(ClearCode);
}


/********************************/
static void cl_hash(register count_int hsize)          /* reset code table */
{
  register count_int *htab_p = htab+hsize;
  register long i;
  register long m1 = -1;

  i = hsize - 16;
  do {                            /* might use Sys V memset(3) here */
    *(htab_p-16) = m1;
    *(htab_p-15) = m1;
    *(htab_p-14) = m1;
    *(htab_p-13) = m1;
    *(htab_p-12) = m1;
    *(htab_p-11) = m1;
    *(htab_p-10) = m1;
    *(htab_p-9) = m1;
    *(htab_p-8) = m1;
    *(htab_p-7) = m1;
    *(htab_p-6) = m1;
    *(htab_p-5) = m1;
    *(htab_p-4) = m1;
    *(htab_p-3) = m1;
    *(htab_p-2) = m1;
    *(htab_p-1) = m1;
    htab_p -= 16;
  } while ((i -= 16) >= 0);

  for ( i += 16; i > 0; i-- ) {
    *--htab_p = m1;
  }
}


/******************************************************************************
 *
 * GIF Specific routines
 *
 ******************************************************************************/

/*
 * Number of characters so far in this 'packet'
 */
static int a_count;

/*
 * Set up the 'byte output' routine
 */
static void char_init()
{
	a_count = 0;
}

/*
 * Define the storage for the packet accumulator
 */
static char accum[ 256 ];

/*
 * Add a character to the end of the current packet, and if it is 254
 * characters, flush the packet to disk.
 */
static void char_out(int c)
{
  accum[ a_count++ ] = c;
  if( a_count >= 254 ) 
    flush_char();
}

/*
 * Flush the packet to disk, and reset the accumulator
 */
static void flush_char()
{
  if( a_count > 0 ) {
    fputc( a_count, g_outfile );
    fwrite( accum, 1, a_count, g_outfile );
    a_count = 0;
  }
}	
