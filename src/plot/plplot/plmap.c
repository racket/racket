/* $Id: plmap.c,v 1.1 2004/03/01 20:54:52 cozmic Exp $

	Continental Outline and Political Boundary Backgrounds

	Some plots need a geographical background such as the global
	surface temperatures or the population density.  The routine
	plmap() will draw one of the following backgrounds: continental
	outlines, political boundaries, the United States, and the United
	States with the continental outlines.  The routine plmeridians()
	will add the latitudes and longitudes to the background.  After
	the background has been drawn, one can use a contour routine or a
	symbol plotter to finish off the plot.

	Copyright (C) 1991, 1993, 1994  Wesley Ebisuzaki
        Copyright (C) 1994, 2000, 2001  Maurice LeBrun
        Copyright (C) 1999  Geoffrey Furnish
        Copyright (C) 2000, 2001, 2002  Alan W. Irwin
        Copyright (C) 2001  Andrew Roach
        Copyright (C) 2001  Rafael Laboissiere
        Copyright (C) 2002  Vincent Darley
        Copyright (C) 2003  Joao Cardoso

        This file is part of PLplot.

	PLplot is free software; you can redistribute it and/or modify
	it under the terms of the GNU Library General Public License
	as published by the Free Software Foundation; version 2 of the
	License.

	PLplot is distributed in the hope that it will be useful, but
	WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU Library General Public License for more details.

	You should have received a copy of the GNU Library General Public
	License along with this library; if not, write to the Free Software
	Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
        USA

*/

#include "plplotP.h"

/*----------------------------------------------------------------------*\
 * void plmap(void (*mapform)(PLINT, PLFLT *, PLFLT *), char *type,
 *            PLFLT minlong, PLFLT maxlong, PLFLT minlat, PLFLT maxlat);
 *
 * plot continental outline in world coordinates
 *
 * v1.4: machine independant version
 * v1.3: replaced plcontinent by plmap, added plmeridians
 * v1.2: 2 arguments:  mapform, type of plot
 *
 * mapform(PLINT n, PLFLT *x, PLFLT *y) is a routine to transform the
 * coordinate longitudes and latitudes to a plot coordinate system.  By
 * using this transform, we can change from a longitude, latitude
 * coordinate to a polar stereographic project, for example.  Initially,
 * x[0]..[n-1] are the longitudes and y[0]..y[n-1] are the corresponding
 * latitudes.  After the call to mapform(), x[] and y[] should be replaced
 * by the corresponding plot coordinates.  If no transform is desired,
 * mapform can be replaced by NULL.
 * 
 * type is a character string. The value of this parameter determines the
 * type of background. The possible values are,
 *
 * 	"globe"		continental outlines
 * 	"usa"		USA and state boundaries
 * 	"cglobe"	continental outlines and countries
 * 	"usaglobe"	USA, state boundaries and continental outlines
 * 
 * minlong, maxlong are the values of the longitude on the left and right
 * side of the plot, respectively. The value of minlong must be less than
 * the values of maxlong, and the values of maxlong-minlong must be less
 * or equal to 360.
 * 
 * minlat, maxlat are the minimum and maximum latitudes to be plotted on
 * the background.  One can always use -90.0 and 90.0 as the boundary
 * outside the plot window will be automatically eliminated.  However, the
 * program will be faster if one can reduce the size of the background
 * plotted.
\*----------------------------------------------------------------------*/

#define MAP_FILE ".map"
#define OFFSET (180*100)
#define SCALE 100.0
#define W_BUFSIZ	(32*1024)

void
plmap( void (*mapform)(PLINT, PLFLT *, PLFLT *), char *type,
         PLFLT minlong, PLFLT maxlong, PLFLT minlat, PLFLT maxlat )
{
    PLINT wrap, sign;
    int i, j;
    PLFLT bufx[200], bufy[200], x[2], y[2];
    short int test[400];
    register PDFstrm *in;
    char filename[100];

    unsigned char n_buff[2], buff[800];
    int n;
    long int t;

    /*
     * read map outline 
     */
    strcpy(filename,type);
    strcat(filename,MAP_FILE);

    if ((in = plLibOpenPdfstrm(filename)) == NULL)
	return;

    for (;;) {
	/* read in # points in segment */
	if (pdf_rdx(n_buff, sizeof (unsigned char)* 2, in) == 0) break;
	n = (n_buff[0] << 8) + n_buff[1];
	if (n == 0) break;

	pdf_rdx(buff, sizeof (unsigned char)*4*n, in);
	if (n == 1) continue;

	for (j = i = 0; i < n; i++, j += 2) {
	    t = (buff[j] << 8) + buff[j+1];
	    bufx[i] = (t - OFFSET) / SCALE;
	}
	for (i = 0; i < n; i++, j += 2) {
	    t = (buff[j] << 8) + buff[j+1];
	    bufy[i] = (t - OFFSET) / SCALE;
	}

	for (i = 0; i < n; i++) {
	    while (bufx[i] < minlong) {
		bufx[i] += 360.0;
	    }
	    while (bufx[i] > maxlong) {
		bufx[i] -= 360.0;
	    }
	}

	/* remove last 2 points if both outside of domain and won't plot */

/* AR: 18/11/01 
*       I have commented out the next section which supposedly
*       removes points that do not plot within the domain. 
*       
*       This code appears at any rate to be superseded by the next
*       block of code that checks for wrapping problems. Removing
*       this code seems to have fixed up the problems with mapping
*       function, but I do not wish to delete it outright just now in
*       case I have managed to miss something.
*/

/*	while (n > 1) {
	    if ((bufx[n-1] < minlong && bufx[n-2] < minlong) ||
		(bufx[n-1] > maxlong && bufx[n-2] > maxlong) ||
		(bufy[n-1] < minlat && bufy[n-2] < minlat) ||
		(bufy[n-1] > maxlat && bufy[n-2] > maxlat)) {
		n--;
	    }
	    else {
		break;
	    }
	}
	if (n <= 1) continue;
*/

       if (mapform != NULL) (*mapform)(n,bufx,bufy); /* moved transformation to here   */
                                                     /* so bound-checking worked right */

	wrap = 0;
	/* check for wrap around problems */
	for (i = 0; i < n-1; i++) {

	  /* jc: this code is wrong, as the bufx/y are floats that are
	     converted to ints before abs() is called. Thus, small
	     differences are masked off. The proof that it is wrong is
	     that when replacing abs() with fabs(), as it should be,
	     the code works the wrong way. What a proof :-)

	    test[i] = abs((bufx[i]-bufx[i+1])) > abs(bufy[i]/3);

	    If the intended behaviour is *really* that, than an
	    explicit cast should be used to help other programmers, as
	    is done bellow!!!
	  */

	    test[i] = abs((int)(bufx[i]-bufx[i+1])) > abs((int)bufy[i]/3); /* Changed this from 30 degrees so it is now "polar sensitive" */
	    if (test[i]) wrap = 1;
	}

	if (wrap == 0) {	
	    plline(n,bufx,bufy);
	}
	else {
	    for (i = 0; i < n-1; i++) {
		x[0] = bufx[i];
		x[1] = bufx[i+1];
		y[0] = bufy[i];
		y[1] = bufy[i+1];
		if (test[i] == 0) {
		    plline(2,x,y);
		}
		else {  /* this code seems to supercede the block commented out above */
		    /* segment goes off the edge */
		    sign = (x[1] > x[0]) ? 1 : -1;
		    x[1] -= sign * 360.0;
		    plline(2,x,y);
		    x[0] = bufx[i];
		    x[1] = bufx[i+1];
		    y[0] = bufy[i];
		    y[1] = bufy[i+1];
		    x[0] += sign * 360.0;
		    plline(2,x,y);
		}
	    }
	}
    }
}

/*----------------------------------------------------------------------*\
 * void plmeridians(void (*mapform)(PLINT, PLFLT *, PLFLT *), 
 *		    PLFLT dlong, PLFLT dlat, PLFLT minlong, PLFLT maxlong, 
 *		    PLFLT minlat, PLFLT maxlat);
 *
 * Plot the latitudes and longitudes on the background.  The lines 
 * are plotted in the current color and line style.
 * 
 * mapform(PLINT n, PLFLT *x, PLFLT *y) is a routine to transform the
 * coordinate longitudes and latitudes to a plot coordinate system.  By
 * using this transform, we can change from a longitude, latitude
 * coordinate to a polar stereographic project, for example.  Initially,
 * x[0]..x[n-1] are the longitudes and y[0]..y[n-1] are the corresponding
 * latitudes.  After the call to mapform(), x[] and y[] should be replaced
 * by the corresponding plot coordinates.  If no transform is desired,
 * mapform can be replaced by NULL.
 * 
 * dlat, dlong are the interval in degrees that the latitude and longitude
 * lines are to be plotted. 
 * 
 * minlong, maxlong are the values of the longitude on the left and right
 * side of the plot, respectively. The value of minlong must be less than
 * the values of maxlong, and the values of maxlong-minlong must be less
 * or equal to 360.
 * 
 * minlat, maxlat are the minimum and maximum latitudes to be plotted on
 * the background.  One can always use -90.0 and 90.0 as the boundary
 * outside the plot window will be automatically eliminated.  However, the
 * program will be faster if one can reduce the size of the background
 * plotted.
\*----------------------------------------------------------------------*/

#define NSEG 100

void 
plmeridians( void (*mapform)(PLINT, PLFLT *, PLFLT *), 
               PLFLT dlong, PLFLT dlat,
               PLFLT minlong, PLFLT maxlong, PLFLT minlat, PLFLT maxlat )
{
    PLFLT yy, xx, temp, x[2], y[2], dx, dy;

    if (minlong > maxlong) {
	temp = minlong;
	minlong = maxlong;
	maxlong = temp;
    }
    if (minlat > maxlat) {
	temp = minlat;
	minlat = maxlat;
	maxlat = temp;
    }
    dx = (maxlong - minlong) / NSEG;
    dy = (maxlat - minlat) / NSEG;

    /* latitudes */

    for (yy = dlat * ceil(minlat/dlat); yy <= maxlat; yy += dlat) {
	if (mapform == NULL) {
	    y[0] = y[1] = yy;
	    x[0] = minlong;
	    x[1] = maxlong;
	    plline(2,x,y);
	}
	else {
	    for (xx = minlong; xx < maxlong; xx += dx) {
	        y[0] = y[1] = yy;
		x[0] = xx;
		x[1] = xx + dx;
	 	(*mapform)(2,x,y);
		plline(2,x,y);
	    }
	}
    }

    /* longitudes */
 
    for (xx = dlong * ceil(minlong/dlong); xx <= maxlong; xx += dlong) {
        if (mapform == NULL) {
            x[0] = x[1] = xx;
            y[0] = minlat;
            y[1] = maxlat;
            plline(2,x,y);
        }
        else {
            for (yy = minlat; yy < maxlat; yy += dy) {
                x[0] = x[1] = xx;
                y[0] = yy;
                y[1] = yy + dy;
                (*mapform)(2,x,y);
                plline(2,x,y);
            }
        }
    }
}
