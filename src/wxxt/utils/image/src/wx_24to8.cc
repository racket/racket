/*
 * xv24to8.c  -  contains the 24-to-8-bit Conv24to8 procedure
 *
 * The Conv24to8 procedure takes a pointer to a 24-bit image (loaded
 * previously).  The image will be a w * h * 3 byte array of
 * bytes.  The image will be arranged with 3 bytes per pixel (in order
 * R, G, and B), pixel 0 at the top left corner.  (As normal.)
 * The procedure also takes a maximum number of colors to use (numcols)
 *
 * The Conv24to8 procedure will set up the following:  it will allocate and
 * create 'pic', a pWIDE*pHIGH 8-bit picture.  it will set up pWIDE and pHIGH.
 * it will load up the r[], g[], and b[] colormap arrays.  it will NOT 
 * calculate numcols, since the cmap sort procedure has to be called anyway
 *
 * Conv24to8 returns '0' if successful, '1' if it failed (presumably on a 
 * malloc())
 *
 * contains:
 *   Cont24to8()
 *   InitFSDTables()
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

static int   num_colors, WIDE, HIGH;
static int   histogram[B_LEN][B_LEN][B_LEN];

CBOX   *freeboxes, *usedboxes;
CCELL **ColorCells;

static void assign_color(CBOX *ptr,byte *rp,byte *gp, byte *bp);
static CCELL *create_colorcell(int r1, int g1, int b1,
			       byte *r, byte *g, byte *b);
static void   map_colortable(byte *r, byte *g, byte *b);

static byte   tbl1[256],     /* tables used in F-S Dithering */
              tbl3[256],     /* contain i/16, 3i/16, 5i/16, 7i/16, */
              tbl5[256],     /* (i=0-255) respectively */
              tbl7[256];


/****************************/
int wxImage::Conv24to8(byte *p, int w, int h, int nc)
/****************************/
{
  int   i;
  CBOX *box_list, *ptr;

  /* copy arguments to local-global variables */
  pic24 = p;  pWIDE = WIDE = w;  pHIGH = HIGH = h;  num_colors = nc;


  /* allocate pic immediately, so that if we can't allocate it, we don't
     waste time running this algorithm */

  {
    byte *ba;
    ba = (byte *) malloc(WIDE * HIGH);
    pic = ba;
  }
  if (pic == NULL) {
    fprintf(stderr,"Conv24to8() - failed to allocate picture\n");
    return(1);
  }


  /* quick check:  if we're going to display a greyscale or 1-bit image,
     DON'T run this annoyingly time-consuming code.  Just convert the 24-bit
     color image to an 8-bit greyscale.  This takes virtually no time, by
     comparision, and it has the same effect. */

  if (mono || nc==0) {
    byte *pp, *p24;

    for (i=0; i<256; i++) {
      r[i]=g[i]=b[i]=i;   /* greyscale colormap */
    }
    pp = pic;  p24 = pic24;
    for (i=WIDE*HIGH; i>0; i--, pp++, p24+=3) {
      *pp = (p24[0]*11 + p24[1]*16 + p24[2]*5) >> 5;  /* pp=.33R+.5G+.17B */
    }

    return(0);
  }

  if (!noqcheck && QuickCheck(pic24,w,h,nc)) { 
    /* see if it's a <256 color RGB pic */
    return 0;   
  }
  else if (!slow24) {
    return(Quick24to8(pic24,w,h));
  }

  /**** STEP 1:  create empty boxes ****/

  usedboxes = NULL;
  freeboxes = (CBOX *) malloc(num_colors * sizeof(CBOX));
  box_list = freeboxes;

  if (box_list == NULL) {
    return(1);
  }

  for (i=0; i<num_colors; i++) {
    freeboxes[i].next = &freeboxes[i+1];
    freeboxes[i].prev = &freeboxes[i-1];
  }
  freeboxes[0].prev = NULL;
  freeboxes[num_colors-1].next = NULL;


  /**** STEP 2: get histogram, initialize first box ****/

  ptr = freeboxes;
  freeboxes = ptr->next;
  if (freeboxes) freeboxes->prev = NULL;

  ptr->next = usedboxes;
  usedboxes = ptr;
  if (ptr->next) ptr->next->prev = ptr;
	
  get_histogram(ptr);


  /**** STEP 3: continually subdivide boxes until no more free boxes remain */

  while (freeboxes) {
    ptr = largest_box();
    if (ptr) splitbox(ptr);
    else break;
  }

  
  /**** STEP 4: assign colors to all boxes ****/

  for (i=0, ptr=usedboxes; i<num_colors && ptr; i++, ptr=ptr->next) {
    assign_color(ptr, &r[i], &g[i], &b[i]);
  }

  /* We're done with the boxes now */
  num_colors = i;
  free(box_list);
  box_list = freeboxes = usedboxes = NULL;
 

  /**** STEP 5: scan histogram and map all values to closest color */

  /* 5a: create cell list as described in Heckbert[2] */

  ColorCells = (CCELL **) calloc(C_LEN*C_LEN*C_LEN, sizeof(CCELL *));


  /* 5b: create mapping from truncated pixel space to color table entries */

  map_colortable(r, g, b);


  /**** STEP 6: scan image, match input values to table entries */

  i=quant_fsdither();

  /* free everything that can be freed */
  free(ColorCells);

  return i;
}


/****************************/
void wxImage::get_histogram(CBOX *box)
/****************************/
{
  int   i,j,rC,gC,bC,*ptr;
  byte *p;

  box->rmin = box->gmin = box->bmin = 999;
  box->rmax = box->gmax = box->bmax = -1;
  box->total = WIDE * HIGH;

  /* zero out histogram */
  ptr = &histogram[0][0][0];
  for (i=B_LEN*B_LEN*B_LEN; i>0; i-- ) { *ptr++ = 0; }

  /* calculate histogram */
  p = pic24;
  for (i=0; i<HIGH; i++) {
    for (j=0; j<WIDE; j++) {
      rC = (*p++) >> (COLOR_DEPTH-B_DEPTH);
      gC = (*p++) >> (COLOR_DEPTH-B_DEPTH);
      bC = (*p++) >> (COLOR_DEPTH-B_DEPTH);

      if (rC < box->rmin) box->rmin = rC;
      if (rC > box->rmax) box->rmax = rC;

      if (gC < box->gmin) box->gmin = gC;
      if (gC > box->gmax) box->gmax = gC;

      if (bC < box->bmin) box->bmin = bC;
      if (bC > box->bmax) box->bmax = bC;
      histogram[rC][gC][bC]++;
    }
  }
}



/******************************/
CBOX *wxImage::largest_box()
/******************************/
{
  CBOX *tmp, *ptr;
  int   size = -1;

  tmp = usedboxes;
  ptr = NULL;

  while (tmp) {
    if ( (tmp->rmax > tmp->rmin  ||
	  tmp->gmax > tmp->gmin  ||
	  tmp->bmax > tmp->bmin)  &&  tmp->total > size ) {
      ptr = tmp;
      size = tmp->total;
    }
    tmp = tmp->next;
  }
  return(ptr);
}



/******************************/
void wxImage::splitbox(CBOX *ptr)
/******************************/
{
  int   hist2[B_LEN], first, last, i, rdel, gdel, bdel;
  CBOX *new_cbox;
  int  *iptr, *histp, ir, ig, ib;
  int  rmin,rmax,gmin,gmax,bmin,bmax;
  enum {RED,GREEN,BLUE} which;

  /*
   * see which axis is the largest, do a histogram along that
   * axis.  Split at median point.  Contract both new boxes to
   * fit points and return
   */

  first = last = 0;   /* shut RT hcc compiler up */

  rmin = ptr->rmin;  rmax = ptr->rmax;
  gmin = ptr->gmin;  gmax = ptr->gmax;
  bmin = ptr->bmin;  bmax = ptr->bmax;

  rdel = rmax - rmin;
  gdel = gmax - gmin;
  bdel = bmax - bmin;

  if      (rdel>=gdel && rdel>=bdel) which = RED;
  else if (gdel>=bdel)               which = GREEN;
  else                               which = BLUE;

  /* get histogram along longest axis */
  switch (which) {

  case RED:
    histp = &hist2[rmin];
    for (ir=rmin; ir<=rmax; ir++) {
      *histp = 0;
      for (ig=gmin; ig<=gmax; ig++) {
	iptr = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++) {
	  *histp += *iptr;
	  ++iptr;
	}
      }
      ++histp;
    }
    first = rmin;  last = rmax;
    break;

  case GREEN:
    histp = &hist2[gmin];
    for (ig=gmin; ig<=gmax; ig++) {
      *histp = 0;
      for (ir=rmin; ir<=rmax; ir++) {
	iptr = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++) {
	  *histp += *iptr;
	  ++iptr;
	}
      }
      ++histp;
    }
    first = gmin;  last = gmax;
    break;

  case BLUE:
    histp = &hist2[bmin];
    for (ib=bmin; ib<=bmax; ib++) {
      *histp = 0;
      for (ir=rmin; ir<=rmax; ir++) {
	iptr = &histogram[ir][gmin][ib];
	for (ig=gmin; ig<=gmax; ig++) {
	  *histp += *iptr;
	  iptr += B_LEN;
	}
      }
      ++histp;
    }
    first = bmin;  last = bmax;
    break;
  }


  /* find median point */
  {
    int sum, sum2;

    histp = &hist2[first];

    sum2 = ptr->total/2;
    histp = &hist2[first];
    sum = 0;
        
    for (i=first; i<=last && (sum += *histp++)<sum2; i++) {
    }
    if (i==first) i++;
  }


  /* Create new box, re-allocate points */
  
  new_cbox = freeboxes;
  freeboxes = new_cbox->next;
  if (freeboxes) freeboxes->prev = NULL;

  if (usedboxes) usedboxes->prev = new_cbox;
  new_cbox->next = usedboxes;
  usedboxes = new_cbox;

  {
    int sum1,sum2,j;
    
    histp = &hist2[first];
    sum1 = 0;
    for (j = first; j < i; ++j) { sum1 += *histp++; }
    sum2 = 0;
    for (j = i; j <= last; ++j) { sum2 += *histp++; }
    new_cbox->total = sum1;
    ptr->total = sum2;
  }


  new_cbox->rmin = rmin;  new_cbox->rmax = rmax;
  new_cbox->gmin = gmin;  new_cbox->gmax = gmax;
  new_cbox->bmin = bmin;  new_cbox->bmax = bmax;

  switch (which) {
  case RED:    new_cbox->rmax = i-1;  ptr->rmin = i;  break;
  case GREEN:  new_cbox->gmax = i-1;  ptr->gmin = i;  break;
  case BLUE:   new_cbox->bmax = i-1;  ptr->bmin = i;  break;
  }

  shrinkbox(new_cbox);
  shrinkbox(ptr);
}


/****************************/
void wxImage::shrinkbox(CBOX *box)
/****************************/
{
  int *histp,ir,ig,ib;
  int  rmin,rmax,gmin,gmax,bmin,bmax;

  rmin = box->rmin;  rmax = box->rmax;
  gmin = box->gmin;  gmax = box->gmax;
  bmin = box->bmin;  bmax = box->bmax;

  if (rmax>rmin) {
    for (ir=rmin; ir<=rmax; ir++) {
      for (ig=gmin; ig<=gmax; ig++) {
	histp = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++) {
	  if (*histp++ != 0) {
	    box->rmin = rmin = ir;
	    goto have_rmin;
	  }
	}
      }
    }

  have_rmin:
    if (rmax>rmin)
      for (ir=rmax; ir>=rmin; --ir) {
	for (ig=gmin; ig<=gmax; ig++) {
	  histp = &histogram[ir][ig][bmin];
	  for (ib=bmin; ib<=bmax; ib++) {
	    if (*histp++ != 0) {
	      box->rmax = rmax = ir;
	      goto have_rmax;
	    }
	  }
	}
      }
  }


 have_rmax:

  if (gmax>gmin) {
    for (ig=gmin; ig<=gmax; ig++) {
      for (ir=rmin; ir<=rmax; ir++) {
	histp = &histogram[ir][ig][bmin];
	for (ib=bmin; ib<=bmax; ib++) {
	  if (*histp++ != 0) {
	    box->gmin = gmin = ig;
	    goto have_gmin;
	  }
	}
      }
    }

  have_gmin:
    if (gmax>gmin)
      for (ig=gmax; ig>=gmin; --ig) {
	for (ir=rmin; ir<=rmax; ir++) {
	  histp = &histogram[ir][ig][bmin];
	  for (ib=bmin; ib<=bmax; ib++) {
	    if (*histp++ != 0) {
	      box->gmax = gmax = ig;
	      goto have_gmax;
	    }
	  }
	}
      }
  }


 have_gmax:

  if (bmax>bmin) {
    for (ib=bmin; ib<=bmax; ib++) {
      for (ir=rmin; ir<=rmax; ir++) {
	histp = &histogram[ir][gmin][ib];
	for (ig=gmin; ig<=gmax; ig++) {
	  if (*histp != 0) {
	    box->bmin = bmin = ib;
	    goto have_bmin;
	  }
	  histp += B_LEN;
	}
      }
    }

  have_bmin:
    if (bmax>bmin)
      for (ib=bmax; ib>=bmin; --ib) {
	for (ir=rmin; ir<=rmax; ir++) {
	  histp = &histogram[ir][gmin][ib];
	  for (ig=gmin; ig<=gmax; ig++) {
	    if (*histp != 0) {
	      bmax = ib;
	      goto have_bmax;
	    }
	    histp += B_LEN;
	  }
	}
      }
  }

 have_bmax: return;
}



/*******************************/
static void assign_color(CBOX *ptr,byte *rp,byte *gp, byte *bp)
/*******************************/
{
  *rp = ((ptr->rmin + ptr->rmax) << (COLOR_DEPTH - B_DEPTH)) / 2;
  *gp = ((ptr->gmin + ptr->gmax) << (COLOR_DEPTH - B_DEPTH)) / 2;
  *bp = ((ptr->bmin + ptr->bmax) << (COLOR_DEPTH - B_DEPTH)) / 2;
}



/*******************************/
CCELL *create_colorcell(int r1, int g1, int b1,
			byte *r,byte *g, byte *b)
/*******************************/
{
  register int    i,tmp, dist;
  register CCELL *ptr;
  register byte  *rp,*gp,*bp;
  int             ir,ig,ib, mindist;

  ir = r1 >> (COLOR_DEPTH-C_DEPTH);
  ig = g1 >> (COLOR_DEPTH-C_DEPTH);
  ib = b1 >> (COLOR_DEPTH-C_DEPTH);

  r1 &= ~1 << (COLOR_DEPTH-C_DEPTH);
  g1 &= ~1 << (COLOR_DEPTH-C_DEPTH);
  b1 &= ~1 << (COLOR_DEPTH-C_DEPTH);

  ptr = (CCELL *) malloc(sizeof(CCELL));
  *(ColorCells + ir*C_LEN*C_LEN + ig*C_LEN + ib) = ptr;
  ptr->num_ents = 0;

  /* step 1: find all colors inside this cell, while we're at
     it, find distance of centermost point to furthest
     corner */

  mindist = 99999999;

  rp=r;  gp=g;  bp=b;
  for (i=0; i<num_colors; i++,rp++,gp++,bp++) {
    if( *rp>>(COLOR_DEPTH-C_DEPTH) == ir  &&
        *gp>>(COLOR_DEPTH-C_DEPTH) == ig  &&
        *bp>>(COLOR_DEPTH-C_DEPTH) == ib) {

      ptr->entries[ptr->num_ents][0] = i;
      ptr->entries[ptr->num_ents][1] = 0;
      ++ptr->num_ents;

      tmp = *rp - r1;
      if (tmp < (MAX_COLOR/C_LEN/2)) tmp = MAX_COLOR/C_LEN-1 - tmp;
      dist = tmp*tmp;

      tmp = *gp - g1;
      if (tmp < (MAX_COLOR/C_LEN/2)) tmp = MAX_COLOR/C_LEN-1 - tmp;
      dist += tmp*tmp;

      tmp = *bp - b1;
      if (tmp < (MAX_COLOR/C_LEN/2)) tmp = MAX_COLOR/C_LEN-1 - tmp;
      dist += tmp*tmp;

      if (dist < mindist) mindist = dist;
    }
  }

  /* step 3: find all points within that distance to box */

  rp=r;  gp=g;  bp=b;
  for (i=0; i<num_colors; i++,rp++,gp++,bp++) {
    if (*rp >> (COLOR_DEPTH-C_DEPTH) != ir  ||
	*gp >> (COLOR_DEPTH-C_DEPTH) != ig  ||
	*bp >> (COLOR_DEPTH-C_DEPTH) != ib) {

      dist = 0;

      if ((tmp = r1 - *rp)>0 || (tmp = *rp - (r1 + MAX_COLOR/C_LEN-1)) > 0 )
	dist += tmp*tmp;

      if( (tmp = g1 - *gp)>0 || (tmp = *gp - (g1 + MAX_COLOR/C_LEN-1)) > 0 )
	dist += tmp*tmp;

      if( (tmp = b1 - *bp)>0 || (tmp = *bp - (b1 + MAX_COLOR/C_LEN-1)) > 0 )
	dist += tmp*tmp;

      if( dist < mindist ) {
	ptr->entries[ptr->num_ents][0] = i;
	ptr->entries[ptr->num_ents][1] = dist;
	++ptr->num_ents;
      }
    }
  }

  /* sort color cells by distance, use cheap exchange sort */
  {
    int n, next_n;

    n = ptr->num_ents - 1;
    while (n>0) {
      next_n = 0;
      for (i=0; i<n; ++i) {
	if (ptr->entries[i][1] > ptr->entries[i+1][1]) {
	  tmp = ptr->entries[i][0];
	  ptr->entries[i][0] = ptr->entries[i+1][0];
	  ptr->entries[i+1][0] = tmp;
	  tmp = ptr->entries[i][1];
	  ptr->entries[i][1] = ptr->entries[i+1][1];
	  ptr->entries[i+1][1] = tmp;
	  next_n = i;
	}
      }
      n = next_n;
    }
  }
  return (ptr);
}




/***************************/
static void map_colortable(byte *r, byte *g, byte *b)
/***************************/
{
  int    ir,ig,ib, *histp;
  CCELL *cell;

  histp  = &histogram[0][0][0];
  for (ir=0; ir<B_LEN; ir++) {
    for (ig=0; ig<B_LEN; ig++) {
      for (ib=0; ib<B_LEN; ib++) {
	if (*histp==0) *histp = -1;
	else {
	  int	i, j, tmp, d2, dist;
	  
	  cell = *(ColorCells +
		   ( ((ir>>(B_DEPTH-C_DEPTH)) << C_DEPTH*2)
		   + ((ig>>(B_DEPTH-C_DEPTH)) << C_DEPTH)
		   +  (ib>>(B_DEPTH-C_DEPTH)) ) );
		
	  if (cell==NULL)
	    cell = create_colorcell(ir<<(COLOR_DEPTH-B_DEPTH),
				    ig<<(COLOR_DEPTH-B_DEPTH),
				    ib<<(COLOR_DEPTH-B_DEPTH),
				    r, g, b);

	  dist = 9999999;
	  for (i=0; i<cell->num_ents && dist>cell->entries[i][1]; i++) {
	    j = cell->entries[i][0];
	    d2 = r[j] - (ir << (COLOR_DEPTH-B_DEPTH));
	    d2 *= d2;
	    tmp = g[j] - (ig << (COLOR_DEPTH-B_DEPTH));
	    d2 += tmp*tmp;
	    tmp = b[j] - (ib << (COLOR_DEPTH-B_DEPTH));
	    d2 += tmp*tmp;
	    if( d2 < dist ) { dist = d2;  *histp = j; }
	  }
	}
	histp++;
      }
    }
  }
}



/*****************************/
int wxImage::quant_fsdither()
/*****************************/
{
  register int  *thisptr, *nextptr;
  int           *thisline, *nextline, *tmpptr;
  int            r1, g1, b1, r2, g2, b2;
  int            i, j, imax, jmax, oval;
  byte          *inptr, *outptr, *tmpbptr;
  int            lastline, lastpixel;

  imax = HIGH - 1;
  jmax = WIDE - 1;
  
  thisline = (int *) malloc(WIDE * 3 * sizeof(int));
  nextline = (int *) malloc(WIDE * 3 * sizeof(int));

  if (thisline == NULL || nextline == NULL) {
    fprintf(stderr,"unable to allocate stuff for the 'dither' routine\n");
    return 1;
  }


  inptr  = (byte *) pic24;
  outptr = (byte *) pic;

  /* get first line of picture */
  for (j=WIDE * 3, tmpptr=nextline, tmpbptr=inptr; j; j--) {
    *tmpptr++ = (int) *tmpbptr++;
  }

  for (i=0; i<HIGH; i++) {
    /* swap thisline and nextline */
    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;
    lastline = (i==imax);

    /* read in next line */
    for (j=WIDE * 3, tmpptr=nextline; j; j--) {
      *tmpptr++ = (int) *inptr++;
    }

    /* dither this line and put it into the output picture */
    thisptr = thisline;  nextptr = nextline;

    for (j=0; j<WIDE; j++) {
      lastpixel = (j==jmax);

      r2 = *thisptr++;  g2 = *thisptr++;  b2 = *thisptr++;

      if (r2<0) r2=0;  else if (r2>=MAX_COLOR) r2=MAX_COLOR-1;
      if (g2<0) g2=0;  else if (g2>=MAX_COLOR) g2=MAX_COLOR-1;
      if (b2<0) b2=0;  else if (b2>=MAX_COLOR) b2=MAX_COLOR-1;

      r1 = r2;  g1 = g2;  b1 = b2;

      r2 >>= (COLOR_DEPTH-B_DEPTH);
      g2 >>= (COLOR_DEPTH-B_DEPTH);
      b2 >>= (COLOR_DEPTH-B_DEPTH);

      if ( (oval=histogram[r2][g2][b2]) == -1) {
	int ci, cj, dist, d2, tmp;
	CCELL *cell;

	cell = *( ColorCells + 
		( ((r2>>(B_DEPTH-C_DEPTH)) << C_DEPTH*2)
	        + ((g2>>(B_DEPTH-C_DEPTH)) << C_DEPTH )
		+  (b2>>(B_DEPTH-C_DEPTH)) ) );
	      
	if (cell==NULL) cell = create_colorcell(r1,g1,b1, r, g, b);

	dist = 9999999;
	for (ci=0; ci<cell->num_ents && dist>cell->entries[ci][1]; ci++) {
	  cj = cell->entries[ci][0];
	  d2 = (r[cj] >> (COLOR_DEPTH-B_DEPTH)) - r2;
	  d2 *= d2;
	  tmp = (g[cj] >> (COLOR_DEPTH-B_DEPTH)) - g2;
	  d2 += tmp*tmp;
	  tmp = (b[cj] >> (COLOR_DEPTH-B_DEPTH)) - b2;
	  d2 += tmp*tmp;
	  if (d2<dist) { dist = d2;  oval = cj; }
	}
	histogram[r2][g2][b2] = oval;
      }

      *outptr++ = oval;

      r1 -= r[oval];  g1 -= g[oval];  b1 -= b[oval];
      /* can't use tables because r1,g1,b1 go negative */

      if (!lastpixel) {
	thisptr[0] += (r1*7)/16;
	thisptr[1] += (g1*7)/16;
	thisptr[2] += (b1*7)/16;
      }

      if (!lastline) {
	if (j) {
	  nextptr[-3] += (r1*3)/16;
	  nextptr[-2] += (g1*3)/16;
	  nextptr[-1] += (b1*3)/16;
	}

	nextptr[0] += (r1*5)/16;
	nextptr[1] += (g1*5)/16;
	nextptr[2] += (b1*5)/16;

	if (!lastpixel) {
	  nextptr[3] += r1/16;
	  nextptr[4] += g1/16;
	  nextptr[5] += b1/16;
	}
	nextptr += 3;
      }
    }
  }
  free(thisline);  free(nextline);
  return 0;
}





/************************************/
int wxImage::Quick24to8(byte *p24, int w, int h)
{

  /* floyd-steinberg dithering.
   *
   * ----   x    7/16
   * 3/16  5/16  1/16
   *
   */

  /* called after 'pic' has been alloced, pWIDE,pHIGH set up, mono/1-bit
     checked already */

  byte *pp;
  int  r1, g1, b1;
  int  *thisline, *nextline, *thisptr, *nextptr, *tmpptr;
  int  i, j, rerr, gerr, berr, pwide3;
  int  imax, jmax;

  pp = pic;  pwide3 = w * 3;  imax = h-1;  jmax = w-1;

  /* load up colormap, 3 bits R, 3 bits G, 2 bits B  (RRRGGGBB) */
  for (i=0; i<256; i++) {
    r[i] =  ((i&0xe0) * 255) / 0xe0;  
    g[i] =  ((i&0x1c) * 255) / 0x1c;
    b[i] =  ((i&0x03) * 255) / 0x03;
  }

  thisline = (int *) malloc(pwide3 * sizeof(int));
  nextline = (int *) malloc(pwide3 * sizeof(int));
  if (!thisline || !nextline) {
    fprintf(stderr,"Unable to allocate memory in Quick24to8()\n");
    return(1);
    }

  /* get first line of picture */
  for (j=pwide3, tmpptr=nextline; j; j--) { *tmpptr++ = (int) *p24++; }

  for (i=0; i<h; i++) {
    tmpptr = thisline;  thisline = nextline;  nextline = tmpptr;   /* swap */

    if (i!=imax)   /* get next line */
      for (j=pwide3, tmpptr=nextline; j; j--) {
	*tmpptr++ = (int) *p24++;
      }

    for (j=0, thisptr=thisline, nextptr=nextline; j<w; j++,pp++) {
      r1 = *thisptr++;  g1 = *thisptr++;  b1 = *thisptr++;
      RANGE(r1,0,255);  RANGE(g1,0,255);  RANGE(b1,0,255);  

      rerr = r1 & 0x1f;  gerr = g1 & 0x1f;  berr = b1 & 0x3f;
      *pp = (r1&0xe0) | ((g1>>3)&0x1c) | (b1>>6); 

      if (j!=jmax) {  /* adjust RIGHT pixel */
	thisptr[0] += tbl7[rerr];
	thisptr[1] += tbl7[gerr];
	thisptr[2] += tbl7[berr];
      }
      
      if (i!=imax) {	/* do BOTTOM pixel */
	nextptr[0] += tbl5[rerr];
	nextptr[1] += tbl5[gerr];
	nextptr[2] += tbl5[berr];

	if (j>0) {  /* do BOTTOM LEFT pixel */
	  nextptr[-3] += tbl3[rerr];
	  nextptr[-2] += tbl3[gerr];
	  nextptr[-1] += tbl3[berr];
	}

	if (j!=jmax) {  /* do BOTTOM RIGHT pixel */
	  nextptr[3] += tbl1[rerr];
	  nextptr[4] += tbl1[gerr];
	  nextptr[5] += tbl1[berr];
	}
	nextptr += 3;
      }
    }
  }

  return 0;
}
      


/****************************/
void InitFSDTables()
/****************************/
{
  int i;
  for (i=0; i<256; i++) {     /* initialize Floyd-Steinberg division tables */
    tbl1[i] = i/16;
    tbl3[i] = (3*i)/16;
    tbl5[i] = (5*i)/16;
    tbl7[i] = (7*i)/16;
  }
}

/****************************/
int wxImage::QuickCheck(byte *pic24, int w, int h, int maxcol)
{
  /* scans picture until it finds more than 'maxcol' different colors.  If it
     finds more than 'maxcol' colors, it returns '0'.  If it DOESN'T, it does
     the 24-to-8 conversion by simply sticking the colors it found into
     a colormap, and changing instances of a color in pic24 into colormap
     indicies (in pic) */

  unsigned long colors[256],col;
  int           i, nc, low, high, mid;
  byte         *p, *pix;

  if (maxcol>256) maxcol = 256;

  /* put the first color in the table by hand */
  nc = 0;  mid = 0;  

  for (i=w*h,p=pic24; i; i--) {
    col  = (*p++ << 16);  
    col += (*p++ << 8);
    col +=  *p++;

    /* binary search the 'colors' array to see if it's in there */
    low = 0;  high = nc-1;
    while (low <= high) {
      mid = (low+high)/2;
      if      (col < colors[mid]) high = mid - 1;
      else if (col > colors[mid]) low  = mid + 1;
      else break;
    }

    if (high < low) { /* didn't find color in list, add it. */
      if (nc>=maxcol) return 0;
      memmove((char *)&colors[low], (char *)&colors[low+1], (nc - low) * sizeof(unsigned long));
      colors[low] = col;
      nc++;
    }
  }


  /* run through the data a second time, this time mapping pixel values in
     pic24 into colormap offsets into 'colors' */

  for (i=w*h,p=pic24, pix=pic; i; i--,pix++) {
    col  = (*p++ << 16);  
    col += (*p++ << 8);
    col +=  *p++;

    /* binary search the 'colors' array.  It *IS* in there */
    low = 0;  high = nc-1;
    while (low <= high) {
      mid = (low+high)/2;
      if      (col < colors[mid]) high = mid - 1;
      else if (col > colors[mid]) low  = mid + 1;
      else break;
    }

    if (high < low) {
      fprintf(stderr,"QuickCheck:  impossible!\n");
      exit(1);
    }
    *pix = mid;
  }

  /* and load up the 'desired colormap' */
  for (i=0; i<nc; i++) {
    r[i] =  colors[i]>>16;  
    g[i] = (colors[i]>>8) & 0xff;
    b[i] =  colors[i]     & 0xff;
  }

  return 1;
}
