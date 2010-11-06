/* plimage() 
 *
 * Author: Alessandro Mirone, Nov 2001
 * Adapted: Joao Cardoso
 */

#include "plplotP.h"

/* Get better names, those are too criptic!
 * 
 * ZEROW2B: zero writing to buffer ?
 * ZEROW2D: zero writing to display ?
 * ONEW2B: set writing to buffer ?
 * ONEW2D: set writing to display ?
 */

void
NoBufferNoPixmap()
{
  PLINT op = ZEROW2B; 

  plsc->plbuf_write = 0; /* TODO: store previous state */
  plP_esc(PLESC_EXPOSE, NULL);
  plP_esc(PLESC_IMAGEOPS, &op);
}

void
RestoreWrite2BufferPixmap()
{
  PLINT op = ONEW2B;

  plsc->plbuf_write = 1; /* TODO: revert from previous state */
  plP_esc(PLESC_IMAGEOPS, &op);
}

void
disabledisplay()
{
  PLINT op = ZEROW2D;

  plP_esc(PLESC_IMAGEOPS, &op);
}

void
enabledisplay()
{
  PLINT op = ONEW2D;

  plP_esc(PLESC_IMAGEOPS, &op);
  plP_esc(PLESC_EXPOSE, NULL); 
}



void
plimageslow(short *x, short *y, unsigned short *data, PLINT nx, PLINT ny, 
	    PLFLT xmin, PLFLT ymin, PLFLT dx, PLFLT dy,
	    unsigned short zmin, unsigned short zmax)
{
  PLINT ix, iy, i;
  PLFLT xf[4], yf[4];
  short xs[5], ys[5];
  int corners[4];
  unsigned short col;

  for (ix = 0; ix < nx ; ix++) {
    for (iy = 0; iy < ny ; iy++) {

      col = data[ix*ny+iy];
      /* only plot points within zmin/zmax range */
      if (col < zmin || col > zmax)
	continue;

      plcol1(col/(float)USHRT_MAX);

      if (plsc->plbuf_read == 1) {
	/* buffer read, is a replot to a slow device. */

	corners[0] = ix*(ny+1)+iy;       /* [ix][iy] */
	corners[1] = (ix+1)*(ny+1)+iy;   /* [ix+1][iy] */
	corners[2] = (ix+1)*(ny+1)+iy+1; /* [ix+1][iy+1] */
	corners[3] = ix*(ny+1)+iy+1;     /* [ix][iy+1] */

	for (i = 0; i < 4; i++) {
	  xs[i] = x[corners[i]];
	  ys[i] = y[corners[i]];
	}
	xs[4] = xs[0]; ys[4] = ys[0];
	plP_fill(xs, ys, 5);

      } else {

	xf[0] = xf[1] = ix;
	xf[2] = xf[3] = ix+1;
	yf[0] = yf[3] = iy;
	yf[1] = yf[2] = iy+1;

	for (i = 0; i < 4; i++) {
	  xf[i] = xmin + xf[i]*dx;
	  yf[i] = ymin + yf[i]*dy;
	}
	plfill(4, xf, yf);
      }
    }
  }
}

void
grimage(short *x, short *y, unsigned short *z, PLINT nx, PLINT ny)
{
    plsc->dev_ix = x;
    plsc->dev_iy = y;
    plsc->dev_z = z;
    plsc->dev_nptsX = nx;
    plsc->dev_nptsY = ny;

    plP_esc(PLESC_IMAGE, NULL);
}

/*-------------------------------------------------------------------------*\
 * plimage
 *           (***** SUBJECT TO CHANGE ******)
 *
 * arguments are
 *   data: array containing image data
 *   nx: dimension of the array in the X axis.        
 *   ny: dimension of the  array in the Y axis 
 *   The array data is indexed like data[ix][iy]
 *
 *   xmin, xmax, ymin, ymax:
 *       data[0][0] corresponds to (xmin,ymin)
 *       data[nx-1][ny-1] to (xmax,ymax)
 *
 *   zmin, zmax:
 *       only data within bounds zmin <= data <= zmax will be
 *       plotted. If zmin == zmax, all data will be ploted.
 *
 *   Dxmin, Dxmax, Dymin, Dymax:
 *       plots only the window of points whose(x,y)'s fall
 *       inside the [Dxmin->Dxmax]X[Dymin->Dymax] window
 *
\*-------------------------------------------------------------------------*/

void
plimage(PLFLT **idata, PLINT nx, PLINT ny, 
	PLFLT xmin, PLFLT xmax, PLFLT ymin, PLFLT ymax, PLFLT zmin, PLFLT zmax,
	PLFLT Dxmin, PLFLT Dxmax, PLFLT Dymin, PLFLT Dymax)
{
  PLINT nnx, nny, ix, iy, ixx, iyy, xm, ym;
  PLFLT dx, dy;
  unsigned short *Zf, szmin, szmax;
  short *Xf, *Yf;
  PLFLT lzmin, lzmax, tz;
  
  if (plsc->level < 3) {
    plabort("plimage: window must be set up first");
    return;
  }

  if (nx <= 0 || ny <= 0) {
    plabort("plimage: nx and ny must be positive");
    return;
  }

  if (Dxmin < xmin || Dxmax > xmax || Dymin < ymin || Dymax > ymax){
    plabort("plimage: Dxmin or Dxmax or Dymin or Dymax not compatible with xminor xmax or ymin or ymax.");
    return;
  }

  dx = (xmax - xmin) / (nx - 1);
  dy = (ymax - ymin) / (ny - 1);
  nnx = (Dxmax-Dxmin)/dx + 1;
  nny = (Dymax-Dymin)/dy + 1;

  Zf = (unsigned short *) malloc(nny*nnx*sizeof(unsigned short));

  xm = floor((Dxmin-xmin)/dx); ym = floor((Dymin-ymin)/dy);
  lzmin = lzmax = idata[xm][ym];
  
  for (ix=xm; ix<xm+nnx; ix++) {
    for (iy=ym; iy<ym+nny; iy++) {
      tz = idata[ix][iy];
      if (lzmax < tz)
	lzmax = tz;
      if (lzmin > tz)
	lzmin = tz;
    }
  }

  ixx=-1;
  for (ix=xm; ix<xm+nnx; ix++) {
    ixx++; iyy=0;
    for (iy=ym; iy<ym+nny; iy++)
      Zf[ixx*nny+iyy++] = (idata[ix][iy] - lzmin)/(lzmax-lzmin)*USHRT_MAX;
  }

  if (zmin == zmax) {
    zmin = lzmin;
    zmax = lzmax;
  } else {
    if (zmin < lzmin)
      zmin = lzmin;

    if (zmax > lzmax)
      zmax = lzmax;
  }

  szmin = (zmin - lzmin)/(lzmax-lzmin)*USHRT_MAX;
  szmax = (zmax - lzmin)/(lzmax-lzmin)*USHRT_MAX;

  xmin = Dxmin;  xmax = Dxmax;
  ymin = Dymin;  ymax = Dymax;

  /* The X and Y arrays has size nnx*nny */
  nnx++; nny++;

  Xf = (short *) malloc(nny*nnx*sizeof(short));
  Yf = (short *) malloc(nny*nnx*sizeof(short));

  /* adjust the step for the X/Y arrays */
  dx = dx*(nx-1)/nx;
  dy = dy*(ny-1)/ny;

  for (ix = 0; ix < nnx; ix++)
    for (iy = 0; iy < nny; iy++) {      
      Xf[ix*nny+iy] =  plP_wcpcx(xmin + ix*dx);
      Yf[ix*nny+iy] =  plP_wcpcy(ymin + iy*dy);
    }

  plP_image(Xf, Yf, Zf, nnx, nny, xmin, ymin, dx, dy, szmin, szmax);

  free(Xf);
  free(Yf);
  free(Zf);
}
