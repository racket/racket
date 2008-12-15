#ifdef WX_USE_CAIRO
extern "C" {
# include <cairo.h>
# ifndef WX_CAIRO_NO_XLIBH
#  include <cairo-xlib.h>
# endif
};

/* Check API version: */
# ifdef cairo_set_rgb_color
  /* New Cairo API (0.5 and up) */
typedef cairo_matrix_t cairo_matrix_p;
#  undef cairo_set_rgb_color
#  define cairo_set_rgb_color(d, r, g, b) cairo_set_source_rgb(d, r, g, b)
#  define cairo_set_matrix_create(m) /* no op */
#  undef cairo_current_matrix
#  define cairo_current_matrix(d, m) cairo_get_matrix(d, &(m))
#  undef cairo_matrix_destroy
#  define cairo_matrix_destroy(m) /* no op */
#  define cairo__set_matrix(CAIRO_DEV, m) cairo_set_matrix(CAIRO_DEV, &(m))
#  define cairo_set_create_xlib(dev, display, drawable, vis, w, h) \
          dev = cairo_create(cairo_xlib_surface_create(wxAPP_DISPLAY, DRAWABLE, vis, w, h))
#  undef cairo_default_matrix
#  define cairo_default_matrix(dev) cairo_identity_matrix(dev)
#  undef cairo_init_clip
#  define cairo_init_clip(dev) cairo_reset_clip(dev)
#  define cairo_destroy_it(c) (cairo_surface_destroy(cairo_get_target(c)), cairo_destroy(c))
# else
  /* Old Cairo API (0.5 and up) */
typedef cairo_matrix_t *cairo_matrix_p;
#  define cairo_set_matrix_create(m) { cairo_matrix_t *__m; __m = cairo_matrix_create(); m = __m; }
#  define cairo__set_matrix(CAIRO_DEV, m) cairo_set_matrix(CAIRO_DEV, m)
#  define cairo_set_create_xlib(dev, display, drawable, vis, w, h) \
          dev = cairo_create(); cairo_set_target_drawable(dev, wxAPP_DISPLAY, DRAWABLE)
#  define cairo_destroy_it(c) cairo_destroy(c)
# endif
#endif
