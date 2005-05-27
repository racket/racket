/* 
 * xvgam.c - Gamma Correction box handling functions
 *
 * callable functions:
 *
 *   CreateGam(geom)        -  creates the ctrlW window.  Doesn't map it.
 *   GamBox(vis)            -  random processing based on value of 'vis'
 *                             maps/unmaps window, etc.
 *   RedrawGam(x,y,w,h)     -  called by 'expose' events
 *   RedrawGraph(x,y,w,h)   -  called by 'expose' events
 *   ClickGam(x,y)          -  called when B1 clicked in gamW 
 *   TrackGraph(x,y)        -  called when B1 clicked in graphW
 *   GenerateGamma()        -  called to generate/error-check 'ghand'
 *   GenerateFSGamma()      -  called to generate floyd steinberg correction
 *   GammifyColors()        -  does gamma correction of r[],g[],b[] arrays
 *   SetGPreset()           -  sets preset #n to supplied values
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

#define BUTTW   80
#define BUTTW2 100
#define BUTTH   19

#define LINESTR "Lines"
#define CURVSTR "Spline"
#define HSVSTR  "HSV Mode"
#define RGBSTR  "RGB Mode"

byte           gamcr[256];   /* gamma correction curve */
byte           fsgamcr[256]; /* gamma correction curve (for FS dither) */

#define NUMHANDS 4
XPoint        ghand[NUMHANDS];

XPoint defgam[NUMHANDS];
static int firsttime=1;


// #if (defined(__STDC__) || hpux)
static float splint(int xa[],int ya[],float y2a[],int n, float x);
static void spline(int *, int *, int, float *);
// #else
// static void spline();
// static float splint();
// #endif

/*********************/
void wxImage::GenerateGamma()
{
  /* this function generates a gamma correction curve (gamcr)

     This function generates a 4 point spline curve to be used as a 
     non-linear grey 'colormap'.  Two of the points are nailed down at 0,0
     and 255,255, and can't be changed.  You specify the other two.  If
     you specify points on the line (0,0 - 255,255), you'll get the normal
     linear reponse curve.  If you specify points of 50,0 and 200,255, you'll
     get grey values of 0-50 to map to black (0), and grey values of 200-255
     to map to white (255) (roughly).  Values between 50 and 200 will cover
     the output range 0-255.  The reponse curve will be slightly 's' shaped. */

  int i,j;
  static int x[NUMHANDS], y[NUMHANDS];
  float yf[NUMHANDS];

  /* do some idiot-proofing (x-coords must be monotonically increasing)  */

  for (i=0; i<4; i++) { 
    RANGE(ghand[i].x, 0, 255); 
    RANGE(ghand[i].y, 0, 255);
  }

  ghand[0].x = 0;  ghand[3].x = 255;
  if (ghand[1].x < 1)  ghand[1].x = 1;
  if (ghand[1].x >253) ghand[1].x = 253;
  if (ghand[2].x < ghand[1].x) ghand[2].x = ghand[1].x + 1;
  if (ghand[2].x >254) ghand[2].x = 254;

  if (firsttime) {   /* if this is the first 'generate' save as 'default' */
    memcpy(defgam, ghand, sizeof(ghand));
    firsttime=0;
  }

  for (i=0; i<NUMHANDS; i++) { x[i] = ghand[i].x;  y[i] = ghand[i].y; }
  spline(x, y, NUMHANDS, yf);
  
  for (i=0; i<256; i++) {
    j = (int) splint(x, y, yf, NUMHANDS, (float) i);
    if (j<0) j=0;
    else if (j>255) j=255;
    gamcr[i] = j;
  }
}


/*********************/
void wxImage::GenerateFSGamma()
{
  /* this function generates the Floyd-Steinberg gamma curve (fsgamcr)

     This function generates a 4 point spline curve to be used as a 
     non-linear grey 'colormap'.  Two of the points are nailed down at 0,0
     and 255,255, and can't be changed.  You specify the other two.  If
     you specify points on the line (0,0 - 255,255), you'll get the normal
     linear reponse curve.  If you specify points of 50,0 and 200,255, you'll
     get grey values of 0-50 to map to black (0), and grey values of 200-255
     to map to white (255) (roughly).  Values between 50 and 200 will cover
     the output range 0-255.  The reponse curve will be slightly 's' shaped. */

  int i,j;
  static int x[4] = {0,32,224,255};
  static int y[4] = {0, 0,255,255};
  float yf[4];

  spline(x, y, 4, yf);
  
  for (i=0; i<256; i++) {
    j = (int) splint(x, y, yf, 4, (float) i);
    if (j<0) j=0;
    else if (j>255) j=255;
    fsgamcr[i] = j;
  }
}


/*********************/
static void spline(int *x,int *y,int n,float *y2)
{
  /* given arrays of data points x[0..n-1] and y[0..n-1], computes the
     values of the second derivative at each of the data points
     y2[0..n-1] for use in the splint function */

  int i,k;
  float p,qn,sig,un,u[NUMHANDS];

  y2[0] = u[0] = 0.0;

  for (i=1; i<n-1; i++) {
    sig = ((float) x[i]-x[i-1]) / ((float) x[i+1] - x[i-1]);
    p = sig * y2[i-1] + 2.0;
    y2[i] = (sig-1.0) / p;
    u[i] = (((float) y[i+1]-y[i]) / (x[i+1]-x[i])) - 
           (((float) y[i]-y[i-1]) / (x[i]-x[i-1]));
    u[i] = (6.0 * u[i]/(x[i+1]-x[i-1]) - sig*u[i-1]) / p;
  }
  qn = un = 0.0;

  y2[n-1] = (un-qn*u[n-2]) / (qn*y2[n-2]+1.0);
  for (k=n-2; k>=0; k--) {
    y2[k] = y2[k]*y2[k+1]+u[k];
  }
}



/*********************/
static float splint(int xa[],int ya[],float y2a[],int n, float x)
{
  int klo,khi,k;
  float h,b,a;

  klo = 0;
  khi = n-1;
  while (khi-klo > 1) {
    k = (khi+klo) >> 1;
    if (xa[k] > x) khi = k;
    else klo = k;
  }
  h = xa[khi] - xa[klo];
  if (h==0.0) fprintf(stderr, "bad xvalues in splint\n");
  a = (xa[khi]-x)/h;
  b = (x-xa[klo])/h;
  return (a*ya[klo] + b*ya[khi] + ((a*a*a-a)*y2a[klo] +(b*b*b-b)*y2a[khi]) 
	  * (h*h) / 6.0);
}
    
/*********************/
void wxImage::GammifyColors()
{
   HSVgamma();
}


#define NOHUE -1

/*********************/
void wxImage::HSVgamma()
{
  int    i, vi, j;
  double rd, gd, bd, h, s, v, max, min, del, rc, agc, bc;
  double f, p, q, t;

  for (i=0; i<numcols; i++) {
    /* convert RGB to HSV */
    rd = r[i] / 255.0;            /* rd,gd,bd range 0-1 instead of 0-255 */
    gd = g[i] / 255.0;
    bd = b[i] / 255.0;

    /* compute maximum of rd,gd,bd */
    if (rd>=gd) { if (rd>=bd) max = rd;  else max = bd; }
           else { if (gd>=bd) max = gd;  else max = bd; }

    /* compute minimum of rd,gd,bd */
    if (rd<=gd) { if (rd<=bd) min = rd;  else min = bd; }
           else { if (gd<=bd) min = gd;  else min = bd; }

    del = max - min;
    v = max;
    if (max != 0.0) s = (del) / max;
               else s = 0.0;

    h = NOHUE;
    if (s != 0.0) {
      rc = (max - rd) / del;
      agc = (max - gd) / del;
      bc = (max - bd) / del;

      if      (rd==max) h = bc - agc;
      else if (gd==max) h = 2 + rc - bc;
      else if (bd==max) h = 4 + agc - rc;

      h = h * 60;
      if (h<0) h += 360;
    }

    /* map near-black to black to avoid weird effects */
    if (v <= .0625) s = 0.0;

    /* apply gamcr[] function to 'v' (the intensity) */
    vi = (int) floor(v * 255);
    v = gamcr[vi] / 255.0;

    /* convert HSV back to RGB */
    if (s==0.0) { rd = v;  gd = v;  bd = v; }
    else {
      if (h==360.0) h = 0.0;
      h = h / 60.0;
      j = (int) floor(h);
      f = h - j;
      p = v * (1-s);
      q = v * (1 - (s*f));
      t = v * (1 - (s*(1 - f)));

      switch (j) {
      case 0:  rd = v;  gd = t;  bd = p;  break;
      case 1:  rd = q;  gd = v;  bd = p;  break;
      case 2:  rd = p;  gd = v;  bd = t;  break;
      case 3:  rd = p;  gd = q;  bd = v;  break;
      case 4:  rd = t;  gd = p;  bd = v;  break;
      case 5:  rd = v;  gd = p;  bd = q;  break;
      }
    }

    r[i] = (int) floor(rd * 255);
    g[i] = (int) floor(gd * 255);
    b[i] = (int) floor(bd * 255);
  }
}

