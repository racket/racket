/*
 *	Functions for drawing String's with tab characters in them
 */

#include <stdlib.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#ifdef WX_USE_XFT
# include <X11/Xcms.h>
# include <X11/Xft/Xft.h>
#endif
#include "xwTabString.h"
#include "wxAllocColor.h"

extern int scheme_utf8_decode_all(unsigned char *, int, unsigned int *, int);
extern int scheme_utf8_decode(const unsigned char *s, int start, int end, 
			      unsigned int *us, int dstart, int dend,
			      long *ipos, char utf16, int permissive);
#ifdef WX_USE_XFT
extern wxExtFont wxFindAAFont(Display *dpy, wxExtFont xfont, int c);
#endif

static int leading_utf8_len(char *s, int len)
{
  long ipos;
  scheme_utf8_decode(s, 0, len, 
		     NULL, 0, 1,
		     &ipos, 0, '?');
  return ipos;
}

static int xdoDraw(measure, font,
		   display, drawable, gc, x, y, string, length, image
#ifdef WX_USE_XFT
		   , xfont, draw, col
#endif
		   )
 int measure;
 XFontStruct *font;
 Display *display;
 Drawable drawable;
 GC gc;
 int x;
 int y;
 String string;
 int length;
#ifdef WX_USE_XFT
 wxExtFont xfont;
 XftDraw *draw;
 XftColor *col;
#endif
{
# define WXTAB_BUF_SIZE 64
  unsigned int *us, usbuf[WXTAB_BUF_SIZE];
  long ulen;
  int width = 0;

  ulen = scheme_utf8_decode_all(string, length, NULL, '?');
  if (ulen <= WXTAB_BUF_SIZE)
    us = usbuf;
  else
    us = (unsigned int *)XtMalloc(ulen * sizeof(unsigned int));
  ulen = scheme_utf8_decode_all(string, length, us, '?');

#ifdef WX_USE_XFT
  if (!xfont)
#endif
    {
      /* Squash 32-bit encoding into 16-bit encoding.
	 Since we overwrite the array, it's important
	 to start at position 0 and go up: */
      int i, v;
      for (i = 0; i < ulen; i++) {
	if (us[i] > 0xFFFF)
	  v = '?';
	else
	  v = us[i];
	((XChar2b *)us)[i].byte2 = v & 0xff;
	((XChar2b *)us)[i].byte1 = v >> 8;
      }
    }

  if (measure
#ifdef WX_USE_XFT
      || xfont
#endif
      ) {
#ifdef WX_USE_XFT
    if (xfont) {
      XGlyphInfo overall;
      int i, start = 0;
      
      width = 0;
      while (1) {
	for (i = start; i < ulen; i++) {
	  if (!XftGlyphExists(display, xfont, us[i]))
	    break;
	}

	if (i > start) {
	  XftTextExtents32(display, xfont, us + start, i - start, &overall);
	  if (!measure) {
	    if (gc) {
	      XFillRectangle(display, drawable, gc, x + width, y - xfont->ascent,
			     overall.xOff, xfont->ascent + xfont->descent);
	    }
	    XftDrawString32(draw, col, xfont, x + width, y, us + start, i - start);
	  }
	  width += overall.xOff;
	}

	start = i;
	if (start < ulen) {
	  /* Substitute */
	  wxExtFont sxfont;
	  sxfont = wxFindAAFont(display, xfont, us[start]);
	  XftTextExtents32(display, sxfont, us + start, 1, &overall);

	  if (!measure) {
	    if (gc) {
	      XFillRectangle(display, drawable, gc, x + width, y - sxfont->ascent,
			     overall.xOff, sxfont->ascent + sxfont->descent);
	    }
	    XftDrawString32(draw, col, sxfont, x + width, y, us + start, 1);
	  }

	  width += overall.xOff;
	  start++;
	} else
	  break;
      }
    } else
#endif
      {
	width = XTextWidth16(font, (XChar2b *)us, ulen);
      }
  }

  if (!measure) {
#ifdef WX_USE_XFT
    if (xfont) {
      /* Done above */
    } else 
#endif
      {
	if (image)
	  XDrawImageString16(display, drawable, gc, x, y, (XChar2b*)us, ulen);
	else
	  XDrawString16(display, drawable, gc, x, y, (XChar2b*)us, ulen);
      }
  }

  if (us != usbuf)
    XtFree((char *)us);

  return width;
}

#ifdef WX_USE_XFT
# define doDraw(dpy, d, gc, x, y, s, len, i, xf, dw, c) xdoDraw(0, font, dpy, d, gc, x, y, s, len, i, xf, dw, c)
#else
# define doDraw(dpy, d, gc, x, y, s, len, i, xf, dw, c) xdoDraw(0, font, dpy, d, gc, x, y, s, len, i)
#endif

#ifdef WX_USE_XFT
# define wxXftTextWidth(dpy, font, s, len, xfont) xdoDraw(1, font, dpy, 0, 0, 0, 0, s, len, 0, xfont, 0, 0)
#else
# define wxXftTextWidth(dpy, font, s, len, xfont) xdoDraw(1, font, dpy, 0, 0, 0, 0, s, len, 0) 
#endif

/*
 *	Like DrawImageString, except it takes an additional  "tabs"
 *	argument, used to specify what horizontal pixel position to
 *	move to when tab characters are present in the string.  If
 *	the "tabs" argument is NULL, works exactly like its
 *	counterpart.
 */
static void
doDrawImageString(display, drawable, gc, x, y, string, length, tabs, font, xfont, line, image, xon, clip)
     Display *display;
     Drawable drawable;
     GC gc;
     int x;
     int y;
     String string;
     int length;
     int *tabs;
     XFontStruct *font;
     wxExtFont xfont;
     int line;
     int image;
     int xon;
     Region clip;
{
  register char	*p, *ep, *ap;
  register int	tx, tab;
#ifdef WX_USE_XFT
  XftColor col;
  XftDraw *draw;
#endif

  if (!length)
    return;

#ifdef WX_USE_XFT
  if (xfont) {
    Visual *visual;
    Colormap cm;

    cm = wx_default_colormap;
    visual = XcmsVisualOfCCC(XcmsCCCOfColormap(display, cm));
    
    draw = XftDrawCreate(display, drawable, visual, cm);
    if (clip)
      XftDrawSetClip(draw, clip);

    if (xon < 0) {
      col.pixel = 0;
      col.color.red = 0xFFFF;
      col.color.green = 0xFFFF;
      col.color.blue = 0xFFFF;
    } else if (xon) {
      col.pixel = 0;
      col.color.red = 0;
      col.color.green = 0;
      col.color.blue = 0;
    } else {
      col.pixel = 0;
      col.color.red = 0xA0A0;
      col.color.green = 0xA0A0;
      col.color.blue = 0xA0A0;
    }
    col.color.alpha = 0xFFFF;
  } else
    draw = NULL;
#endif
  
  tab = tx = 0;
  for (p = string; length; )
    {
      if (tabs)
	ep = strnchr(p, '\t', length);
      else
	ep = NULL;
      if (font)
	ap = strnchr(p, '&', length);
      else
	ap = NULL;

      if (ep && ap) {
	if ((long)ep < (long)ap)
	  ap = NULL;
	else
	  ep = NULL;
      }

      if (ep) {
	doDraw(display, drawable, gc, x+tx, y, p, ep - p, image, xfont, draw, &col);
	tx = tabs[tab++];
	length -= ep - p + 1;
	p = ep + 1;
      } else if (ap) {
	doDraw(display, drawable, gc, x+tx, y, p, ap - p, image, xfont, draw, &col);
	tx += wxXftTextWidth(display, font, p, ap - p, xfont);
	length -= ap - p + 1;
	p = ap + 1;
	if (length) {
	  /* Underline next */
	  int ww;
	  int csize;
	  
	  csize = leading_utf8_len(p, length);

	  ww = wxXftTextWidth(display, font, p, csize, xfont);
	  doDraw(display, drawable, gc, x+tx, y, p, csize, image, xfont, draw, &col);
	  if (line && (*p != '&')) {
#ifdef WX_USE_XFT	
	    if (xfont)
	      XftDrawRect(draw,&col, x+tx, y+1, ww, 1);
	    else
#endif
	      XDrawLine(display, drawable, gc, x+tx, y+1, x+tx+ww, y+1);
	  }
	  length -= csize;
	  tx += ww;
	  p += csize;
	}
      } else {
	doDraw(display, drawable, gc, x+tx, y, p, length, image, xfont, draw, &col);
	break;
      }
    }

#ifdef WX_USE_XFT
  if (draw)
    XftDrawDestroy(draw);
#endif
}

void
XfwfDrawImageString(display, drawable, gc, x, y, string, length, tabs, fnt, xfnt, xon, clip)
     Display *display;
     Drawable drawable;
     GC gc;
     int x;
     int y;
     String string;
     int length;
     int *tabs;
     XFontStruct *fnt;
     wxExtFont xfnt;
     int xon;
     Region clip;
{
  doDrawImageString(display, drawable, gc, x, y, string, length, tabs, fnt, xfnt, 1, 1, xon, clip);
}

void
XfwfDrawString(display, drawable, gc, x, y, string, length, tabs, fnt, xfnt, xon, line, clip)
     Display *display;
     Drawable drawable;
     GC gc;
     int x;
     int y;
     String string;
     int length;
     int *tabs;
     XFontStruct *fnt;
     wxExtFont xfnt;
     int line;
     int xon;
     Region clip;
{
  doDrawImageString(display, drawable, gc, x, y, string, length, tabs, fnt, xfnt, line, 0, xon, clip);
}

/*
 *	Converts a string list of tabs to an array of tabs
 */
int *
XfwfTablist2Tabs(tablist)
char *tablist;
{
	register int	*tabs = NULL;
	register int	ntabs = 0;

	if (!tablist)
		return NULL;
	for (;;)
	{
		/* Skip leading blanks */
		while (*tablist && *tablist == ' ') ++tablist;
		if (!*tablist) break;

		/* Allocate space for the new tab */
		if (ntabs)
			tabs = (int *) XtRealloc( (char *) tabs,
						(ntabs+1) * sizeof(int));
		else
			tabs = (int *) XtMalloc( (ntabs + 1) * sizeof(int));
		/* Add it to the list */
		tabs[ntabs++] = atoi(tablist);
		/* Skip to the next blank */
		while (*tablist && *tablist != ' ') ++tablist;
	}
	return (tabs);
}

/*
 *	Like TextWidth, except it takes an additional  "tabs"
 *	argument, used to specify what horizontal pixel position to
 *	move to when tab characters are present in the string.  If
 *	the "tabs" argument is NULL, works exactly like its
 *	counterpart.
 */
int
XfwfTextWidth(display, font, xfont, str, length, tabs)
     Display *display;
     XFontStruct *font;
     wxExtFont xfont;
     String str;
     int length;
     int *tabs;
{
  register char	*p, *ep, *c = NULL, *pp;
  register int	tx, tab, rc, ll;

  if (!length)
    return 0;
 
  p = pp = str;
  ll = length;

  while (1) {
    ep = strnchr(pp, '&', ll);
    if (ep) {
      int l = ep - p;
      if (!c)
	c = XtMalloc(length + 1);
      memmove(c, p, l);
      memmove(c + l, p + l + 1, length - l); /* gets nul char */
      length -= 1;
      p = c;
      if (length > l) {
	pp = c + l + 1; /* Skip next char */
	ll = length - (l + 1);
      } else {
	pp = p;
	ll = length;
      }
    } else
      break;
  }

  tab = tx = 0;
  if (length == 0) {
    if (c)
      XtFree(c);
    return 0;
  }
  for (; length; ) {
    ep = strnchr(p, '\t', length);
    if (ep && tabs) {
      tx = tabs[tab++];
      length -= ep - p + 1;
      p = ep + 1;
    } else {
      rc = wxXftTextWidth(display, font, p, length, xfont);
      if (c)
	XtFree(c);
      if (rc < 0) return rc; else return rc + tx;
    }
  }

  if (c)
    XtFree(c);

  return -1;
}

/*
 *	Like strchr, except has a length limit.
 */
char *
strnchr(s, c, n)
     char *s;
     int c;
     int n;
{
	while (n--)
		if (*s == c) return s; else ++s;
	return NULL;
}

int wx_enough_colors(Screen *s)
{
  if (wx_visual_depth > 8)
    return 1;
  else
    return 0;
}



