
#include <stdlib.h>
#include <X11/Xcms.h>

typedef struct {
  unsigned short red_in, green_in, blue_in;
  unsigned short red_out, green_out, blue_out;
  unsigned long pixel;
  int weight;
} ColorCache;

#define COLOR_CACHE_SIZE 1000

static ColorCache cache[COLOR_CACHE_SIZE];
static int cache_end;

static long alloc_size;
static long alloc_count;
static unsigned long *alloced;

extern Screen *wxAPP_SCREEN;
extern Display *wxAPP_DISPLAY;
extern Visual *wxAPP_VISUAL;

#define OK 1

/* Fast TrueColor code is from Guillaume Chazarain <gfc@altern.org> */

/* How many bits are set in mask ? */
static int mask_length(unsigned long mask)
{
    int length = 0;

    while (mask) {
        length += mask & 1;
        mask >>= 1;
    }

    return length;
}

/* Where does the mask start, in bits from the LSB? */
static int mask_start(unsigned long mask)
{
    int pos = 0;

    while (!(mask & 0x1)) {
      pos++;
      mask >>= 1;
    }

    return pos;
}

/* Take the nb most significant bits in value. */
static inline unsigned short n_bits(unsigned short value, int nb)
{
    /* length should be 16. */
    const int length = sizeof(unsigned short) * 8;
    unsigned short mask;

    /*        16 - nb bits
     *        vvvvvvvvvvv
     * mask = 0000000000011111
     *                   ^^^^^
     *                  nb bits
     */
    mask = (1 << nb) - 1;

    /* mask = 1111100000000000 */
    mask <<= length - nb;

    /* value = xxxxx00000000000 */
    value &= mask;

    /* value = 00000000000xxxxx */
    value >>= length - nb;

    return value;
}

static Visual *tc;

static int tc_known;

static unsigned int r_length, g_length, b_length;
static unsigned int r_start, g_start, b_start;

int wx_simple_r_start, wx_simple_g_start, wx_simple_b_start;

int wx_alloc_color_is_fast;

Colormap fast_colormap = 0; /* for init, we assume that no valid colormap is 0! */

Status wxAllocColor(Display *d, Colormap cm, XColor *c)
{
  int i;
  int min_weight;
  int min_weight_pos;
  unsigned short ri, gi, bi;
  int p, w, o;
  unsigned long pixel;
  Status status;

  /* Check fast path first: */
  if (cm == fast_colormap) {
    c->red = n_bits(c->red, r_length);
    c->green = n_bits(c->green, g_length);
    c->blue = n_bits(c->blue, b_length);

    c->pixel = ((c->red << r_start)
		| (c->green << g_start)
		| (c->blue << b_start));

    return OK;
  }

  /* If we have a weird colormap, essentially give up (no
     deallocation). */
  if (cm != wx_default_colormap)
    return XAllocColor(d, cm, c);

  /* Is the default colormap TrueColor? */

  if (!tc_known) {
    tc = wxAPP_VISUAL;
    if (tc->class != TrueColor)
      tc = NULL;
    else {
      r_length = mask_length(tc->red_mask);
      g_length = mask_length(tc->green_mask);
      b_length = mask_length(tc->blue_mask);

      r_start = mask_start(tc->red_mask);
      g_start = mask_start(tc->green_mask);
      b_start = mask_start(tc->blue_mask);

      if ((r_length == 8) && (g_length == 8) && (b_length == 8)) {
	wx_simple_r_start = r_start;
	wx_simple_g_start = g_start;
	wx_simple_b_start = b_start;
	wx_alloc_color_is_fast = 2;
      } else
	wx_alloc_color_is_fast = 1;

      fast_colormap = wx_default_colormap;
    }
    tc_known = 1;

    /* Try again: */
    return wxAllocColor(d, cm, c);
  }
  
  /* Not TrueColor. Do things the difficult way... */

  /* Check for black: */
  if (!c->red && !c->green && !c->blue) {
    c->pixel = BlackPixelOfScreen(wxAPP_SCREEN);
    return OK;
  }
  
  /* Check for white: */
  if ((c->red >= 0xFF00) && (c->green >= 0xFF00) && (c->blue >= 0xFF00)) {
    c->pixel = WhitePixelOfScreen(wxAPP_SCREEN);
    c->red = 0xFFFF;
    c->green = 0xFFFF;
    c->blue = 0xFFFF;
    return OK;
  }

  ri = c->red;
  gi = c->green;
  bi = c->blue;

  /* Check in cache: */ 
  min_weight_pos = 0;
  min_weight = cache[0].weight;
  for (i = 0; i < cache_end; i++) {
    if (cache[i].red_in == ri
	&& cache[i].green_in == gi
	&& cache[i].blue_in == bi) {
      c->red = cache[i].red_out;
      c->green = cache[i].green_out;
      c->blue = cache[i].blue_out;
      c->pixel = cache[i].pixel;

      if (cache[i].weight < 10000)
	cache[i].weight++;

      return OK;
    } else if (cache[i].weight < min_weight) {
      min_weight = cache[i].weight;
      min_weight_pos = i;
    }
  }

  if (cache_end == COLOR_CACHE_SIZE) {
    /* Degrade weights: */
    if (cache[COLOR_CACHE_SIZE - 1].pixel) {
      for (i = 0; i < cache_end; i++) 
	if (cache[i].weight)
	  --cache[i].weight;
    }
  } else
    min_weight_pos = cache_end++;

  status = XAllocColor(d, cm, c);

  if (status == OK) {
    /* Add to cache: */
    cache[min_weight_pos].red_in = ri;
    cache[min_weight_pos].green_in = gi;
    cache[min_weight_pos].blue_in = bi;
    cache[min_weight_pos].red_out = c->red;
    cache[min_weight_pos].green_out = c->green;
    cache[min_weight_pos].blue_out = c->blue;
    cache[min_weight_pos].pixel = c->pixel;
    cache[min_weight_pos].weight = 10;

    /* Record allocation */

    /* Binary search for pixel: */
    pixel = c->pixel;
    if (alloc_count) {
      o = 0;
      p = alloc_count >> 1;
      w = alloc_count;
      
      while (1) {
	unsigned long v;
	
	v = alloced[p];
	
	if (v == pixel) {
	  /* Balance redundant Alloc with Free: */
	  XFreeColors(d, cm, &pixel, 1, 0);
	  return OK;
	}
	if (w == 1) {
	  if (v < pixel)
	    p++;
	  break;
	}
	if (v < pixel) {
	  w = o + w - p;
	  o = p;
	} else {
	  w = p - o;
	}
	p = o + (w >> 1);
      }
    } else
      p = 0;

    /* Not alloced before. */
    /* First make sure array is large enough: */
    if (alloc_count == alloc_size) {
      unsigned long *old = alloced;

      if (!alloc_size)
	alloc_size = 256;
      else
	alloc_size = alloc_size * 2;
      
      alloced = (unsigned long *)malloc(sizeof(unsigned long) * alloc_size);
      for (i = 0; i < alloc_count; i++)
	alloced[i] = old[i];
      free(old);
    }
    
    for (i = alloc_count; i-- > p; )
      alloced[i + 1] = alloced[i];
    alloced[p] = pixel;
    alloc_count++;
    
    return OK;
  } else
    return status;
}

int wxQueryColor(Display *display, Colormap colormap, XColor *def_in_out)
{
  if (tc && (colormap == wx_default_colormap)) {
    unsigned long pixel = def_in_out->pixel, red, green, blue;

    red = (pixel >> r_start) & ((1 << r_length) - 1);
    green = (pixel >> g_start) & ((1 << g_length) - 1);
    blue = (pixel >> b_start) & ((1 << b_length) - 1);

    def_in_out->red = (red << (16 - r_length));
    def_in_out->green = (green << (16 - g_length));
    def_in_out->blue = (blue << (16 - b_length));
    
    return 1; /* is this the right return value? */
  }

  return XQueryColor(display, colormap, def_in_out);
}
