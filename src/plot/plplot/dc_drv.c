/*
      Device driver whose handlers are inplemented in Racket
      using dc<%>.
*/

#include "plDevs.h"
#include "plplotP.h"
#include "drivers.h"

/* Device info */
char* plD_DEVICE_INFO_dc = "dc:dc<%>:0:dc:50:dc";

void plD_init_dc		(PLStream *);
void plD_line_dc		(PLStream *, short, short, short, short);
void plD_polyline_dc		(PLStream *, short *, short *, PLINT);
void plD_eop_dc   		(PLStream *);
void plD_eop_jpeg		(PLStream *);
void plD_bop_dc                 (PLStream *);
void plD_tidy_dc		(PLStream *);
void plD_state_dc		(PLStream *, PLINT);
void plD_esc_dc 		(PLStream *, PLINT, void *);

typedef struct {
  void *p;
  void (*drawLine)(void *p, short x1a, short y1a, short x2a, short y2a);
  void (*drawLines)(void *p, short *xa, short *ya, PLINT npts);
  void (*fillPoly)(void *p, short *xa, short *ya, PLINT npts);
  void (*setWidth)(void *p, int w);
  void (*setColor)(void *p, short i);
  void (*setColorRGB)(void *p, short r, short g, short b);
  void (*startPage)(void *p);
  void (*endPage)(void *p);
  void (*endDoc)(void *p);
} dc_Dev;

void plD_dispatch_init_dc( PLDispatchTable *pdt )
{
    pdt->pl_MenuStr  = "drawing context";
    pdt->pl_DevName  = "dc<%>";
    pdt->pl_type     = plDevType_FileOriented;
    pdt->pl_seq      = 50;
    pdt->pl_init     = (plD_init_fp)     plD_init_dc;
    pdt->pl_line     = (plD_line_fp)     plD_line_dc;
    pdt->pl_polyline = (plD_polyline_fp) plD_polyline_dc;
    pdt->pl_eop      = (plD_eop_fp)      plD_eop_dc;
    pdt->pl_bop      = (plD_bop_fp)      plD_bop_dc;
    pdt->pl_tidy     = (plD_tidy_fp)     plD_tidy_dc;
    pdt->pl_state    = (plD_state_fp)    plD_state_dc;
    pdt->pl_esc      = (plD_esc_fp)      plD_esc_dc;
}

/*--------------------------------------------------------------------------*\
 * plD_init_dc_Dev()
 *
\*--------------------------------------------------------------------------*/

static void
plD_init_dc_Dev(PLStream *pls)
{
  /* Allocate and initialize device-specific data */
  dc_Dev *dev;

  if (pls->dev != NULL)
    free((void *) pls->dev);

  pls->dev = calloc(1, (size_t) sizeof(dc_Dev));
  if (pls->dev == NULL)
    plexit("plD_init_dc_Dev: Out of memory.");
  
  dev = (dc_Dev *) pls->dev;
}

/*----------------------------------------------------------------------*\
 * plD_init_dc()
 *
 * Initialize device.
\*----------------------------------------------------------------------*/

void plD_init_dc(PLStream *pls)
{
  dc_Dev *dev;

    pls->termin = 0;            /* Not an interactive device */
    pls->icol0 = 1;
    pls->bytecnt = 0;
    pls->page = 0;
    pls->dev_fill0 = 1;         /* Can do solid fills */

    if (!pls->colorset)
	pls->color = 1;         /* Is a color device */

/* Initialize family file info */
    plFamInit(pls);

/* Allocate and initialize device-specific data */
    plD_init_dc_Dev(pls);
    dev=(dc_Dev *)pls->dev;

      if (pls->xlength <= 0 || pls->ylength <=0)
      {
/* use default width, height of 800x600 if not specifed by -geometry option
 * or plspage */
	 plspage(0., 0., 800, 600, 0, 0);
      }

     pls->graphx = GRAPHICS_MODE;

     if (pls->xdpi<=0)
     {
/* This corresponds to a typical monitor resolution of 4 pixels/mm. */
        plspage(4.*25.4, 4.*25.4, 0, 0, 0, 0);
     }
     else
     {
        pls->ydpi=pls->xdpi;        /* Set X and Y dpi's to the same value */
     }
/* Convert DPI to pixels/mm */
     plP_setpxl(1*pls->xdpi/25.4,1*pls->ydpi/25.4);

     plP_setphy(0, 1*(pls->xlength - 1), 0, 1*(pls->ylength - 1));
}

/*----------------------------------------------------------------------*\
 * plD_line_dc()
 *
 * Draw a line in the current color from (x1,y1) to (x2,y2).
\*----------------------------------------------------------------------*/

void
plD_line_dc(PLStream *pls, short x1a, short y1a, short x2a, short y2a)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  if (dev->drawLine)
    dev->drawLine(dev->p, x1a, y1a, x2a, y2a);
}

/*----------------------------------------------------------------------*\
 * plD_polyline_dc()
 *
 * Draw a polyline in the current color.
\*----------------------------------------------------------------------*/

void
plD_polyline_dc(PLStream *pls, short *xa, short *ya, PLINT npts)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  if (dev->drawLines)
    dev->drawLines(dev->p, xa, ya, npts);
}


/*----------------------------------------------------------------------*\
 * fill_polygon()
 *
 * Fill polygon described in points pls->dev_x[] and pls->dev_y[].
\*----------------------------------------------------------------------*/

static void
fill_polygon(PLStream *pls)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  if (dev->fillPoly)
    dev->fillPoly(dev->p, pls->dev_x, pls->dev_y, pls->dev_npts);
}


/*----------------------------------------------------------------------*\
 * plD_state_dc()
 *
 * Handle change in PLStream state (color, pen width, fill attribute, etc).
\*----------------------------------------------------------------------*/

void
plD_state_dc(PLStream *pls, PLINT op)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  switch (op) {

    case PLSTATE_WIDTH:
      if (dev->setWidth)
        dev->setWidth(dev->p, pls->width);
      break;

  case PLSTATE_COLOR0:
    
    if (pls->icol0 == PL_RGB_COLOR) {
      if (dev->setColorRGB)
        dev->setColorRGB(dev->p, pls->curcolor.r, pls->curcolor.g, pls->curcolor.b);
    } else {
      if (dev->setColor)
        dev->setColor(dev->p, pls->icol0);
    }
    break;

  case PLSTATE_COLOR1:
    if (dev->setColorRGB)
      dev->setColorRGB(dev->p, pls->curcolor.r, pls->curcolor.g, pls->curcolor.b);
    break;
  }
}


/*----------------------------------------------------------------------*\
 * plD_esc_dc()
 *
 * Escape function.
\*----------------------------------------------------------------------*/

void plD_esc_dc(PLStream *pls, PLINT op, void *ptr)
{
    switch (op) {

      case PLESC_FILL:  /* fill */
	fill_polygon(pls);
	break;

    }
}

/*----------------------------------------------------------------------*\
 * plD_bop_dc()
 *
 * Set up for the next page.
 * Advance to next family file if necessary (file output).
\*----------------------------------------------------------------------*/

void plD_bop_dc(PLStream *pls)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  if (dev->startPage)
    dev->startPage(dev->p);
}

/*----------------------------------------------------------------------*\
 * plD_tidy_dc()
 *
 * Close graphics file or otherwise clean up.
\*----------------------------------------------------------------------*/

void plD_tidy_dc(PLStream *pls)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  if (dev->endDoc)
    dev->endDoc(dev->p);

}

/*----------------------------------------------------------------------*\
 * plD_eop_dc()
 *
 * End of page.
\*----------------------------------------------------------------------*/

void plD_eop_dc(PLStream *pls)
{
  dc_Dev *dev=(dc_Dev *)pls->dev;

  if (dev->endPage)
    dev->endPage(dev->p);
}
