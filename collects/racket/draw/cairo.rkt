#lang scheme/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         setup/dirs
         "utils.rkt")

(define cairo-lib 
  (case (system-type)
    [(macosx) (ffi-lib "libcairo.2")]
    [(unix) (ffi-lib "libcairo" '("2"))]
    [(windows) 
     (ffi-lib "zlib1")
     (ffi-lib "libpng14-14")
     (ffi-lib "libexpat-1")
     (ffi-lib "freetype6")
     (ffi-lib "libfontconfig-1")
     (ffi-lib "libcairo-2")]))

(define gdk-lib 
  (case (system-type)
    [(unix) (ffi-lib "libgdk-x11-2.0" '("0"))]
    [else #f]))

(define-ffi-definer define-cairo cairo-lib
  #:provide provide-protected)

(provide _cairo_t
         _cairo_font_options_t)

(define _cairo_surface_t (_cpointer 'cairo_surface_t))
(define _cairo_surface_t/null (_cpointer/null 'cairo_surface_t))
(define _cairo_t (_cpointer 'cairo_t))
(define _cairo_pattern_t (_cpointer 'cairo_pattern_t))
(define _cairo_font_options_t (_cpointer/null 'cairo_font_options_t))
(define _CGContextRef _pointer)

(define-cstruct _cairo_matrix_t ([xx _double*]
                                 [yx _double*]
                                 [xy _double*]
                                 [yy _double*]
                                 [x0 _double*]
                                 [y0 _double*]))
(provide (struct-out cairo_matrix_t))

(define-cstruct _cairo_glyph_t ([index _long] [x _double*] [y _double*]))
(provide make-cairo_glyph_t)

(define-cairo cairo_destroy (_fun _cairo_t -> _void) 
  #:wrap (deallocator))

(define-cairo cairo_surface_destroy (_fun _cairo_surface_t -> _void)
  #:wrap (deallocator))

(define-cairo cairo_quartz_surface_create
  (_fun _int _uint _uint -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_quartz_surface_create_for_cg_context
  (_fun _CGContextRef _uint _uint -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_win32_surface_create
  (_fun _pointer -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_surface_create_similar
  (_fun _cairo_surface_t _int _int _int -> _cairo_surface_t))

(define-cairo cairo_create (_fun _cairo_surface_t -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define-cairo cairo_get_target (_fun _cairo_t -> _cairo_surface_t)) ;; not an allocator

;; Context
(define-cairo cairo_paint (_fun _cairo_t -> _void))
(define-cairo cairo_fill (_fun _cairo_t -> _void))
(define-cairo cairo_fill_preserve (_fun _cairo_t -> _void))
(define-cairo cairo_stroke (_fun _cairo_t -> _void))
(define-cairo cairo_stroke_preserve (_fun _cairo_t -> _void))
(define-cairo cairo_save (_fun _cairo_t -> _void))
(define-cairo cairo_restore (_fun _cairo_t -> _void))
(define-cairo cairo_clip (_fun _cairo_t -> _void))
(define-cairo cairo_reset_clip (_fun _cairo_t -> _void))

(define-cairo cairo_in_fill (_fun _cairo_t _double* _double* -> _bool))

(define-cairo cairo_clip_extents (_fun _cairo_t 
                                       (x1 : (_ptr o _double)) 
                                       (y1 : (_ptr o _double)) 
                                       (x2 : (_ptr o _double)) 
                                       (y2 : (_ptr o _double)) 
                                       -> _void
                                       -> (values x1 y1 x2 y2)))

;; Transforms
(define-cairo cairo_translate (_fun _cairo_t _double* _double* -> _void))
(define-cairo cairo_scale (_fun _cairo_t _double* _double* -> _void))
(define-cairo cairo_rotate (_fun _cairo_t _double* -> _void))
(define-cairo cairo_transform (_fun _cairo_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_identity_matrix (_fun _cairo_t -> _void))
(define-cairo cairo_get_matrix (_fun _cairo_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_set_matrix (_fun _cairo_t _cairo_matrix_t-pointer -> _void))

(define-cairo cairo_matrix_init_translate (_fun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_init (_fun _cairo_matrix_t-pointer _double* _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_matrix_translate (_fun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_scale (_fun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_rotate (_fun _cairo_matrix_t-pointer _double* -> _void))
(define-cairo cairo_matrix_multiply (_fun _cairo_matrix_t-pointer _cairo_matrix_t-pointer _cairo_matrix_t-pointer -> _void))

;; Stroke & Fill
(define-cairo cairo_set_source_rgb (_fun _cairo_t _double* _double* _double* -> _void))
(define-cairo cairo_set_source_rgba (_fun _cairo_t _double* _double* _double* _double* -> _void))
(define-cairo cairo_set_line_width (_fun _cairo_t _double* -> _void))
(define-cairo cairo_set_line_cap (_fun _cairo_t _int -> _void))
(define-cairo cairo_set_line_join (_fun _cairo_t _int -> _void))
(define-cairo cairo_set_dash (_fun _cairo_t (v : (_vector i _double*)) [_int = (vector-length v)] _double* -> _void))
(define-cairo cairo_set_antialias (_fun _cairo_t _int -> _void))

(define-cairo cairo_set_fill_rule (_fun _cairo_t _int -> _void))

(define-cairo cairo_get_operator (_fun _cairo_t -> _int))
(define-cairo cairo_set_operator (_fun _cairo_t _int -> _void))

;; Text
(define-cairo cairo_font_options_destroy (_fun _cairo_font_options_t -> _void)
  #:wrap (deallocator))
(define-cairo cairo_font_options_create (_fun -> _cairo_font_options_t)
  #:wrap (allocator cairo_font_options_destroy))
(define-cairo cairo_font_options_copy (_fun _cairo_font_options_t _cairo_font_options_t -> _void))
(define-cairo cairo_get_font_options (_fun _cairo_t _cairo_font_options_t -> _void))
(define-cairo cairo_set_font_options (_fun _cairo_t _cairo_font_options_t -> _void))
(define-cairo cairo_font_options_set_antialias (_fun _cairo_font_options_t _int -> _void))

(define-cairo cairo_show_glyphs (_fun _cairo_t _cairo_glyph_t-pointer _int -> _void))

;; Paths
(define-cairo cairo_rectangle (_fun _cairo_t _double* _double* _double* _double* -> _void))
(define-cairo cairo_move_to (_fun _cairo_t _double* _double* -> _void))
(define-cairo cairo_rel_move_to (_fun _cairo_t _double* _double* -> _void))
(define-cairo cairo_line_to (_fun _cairo_t _double* _double* -> _void))
(define-cairo cairo_rel_line_to (_fun _cairo_t _double* _double* -> _void))
(define-cairo cairo_arc (_fun _cairo_t _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_arc_negative (_fun _cairo_t _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_curve_to (_fun _cairo_t _double* _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_new_path (_fun _cairo_t -> _void))
(define-cairo cairo_close_path (_fun _cairo_t -> _void))

(define-cairo cairo_show_page (_fun _cairo_t -> _void))

(define-cairo cairo_set_source (_fun _cairo_t _cairo_pattern_t -> _void))
(define-cairo cairo_get_source (_fun _cairo_t -> _cairo_pattern_t)) ;; not an allocator
(define-cairo cairo_set_source_surface (_fun _cairo_t _cairo_surface_t _double* _double* -> _void))
(define-cairo cairo_mask (_fun _cairo_t _cairo_pattern_t -> _void))
(define-cairo cairo_mask_surface (_fun _cairo_t _cairo_surface_t _double* _double* -> _void))
(define-cairo cairo_pattern_destroy (_fun _cairo_pattern_t -> _void)
  #:wrap (deallocator))
(define-cairo cairo_pattern_create_for_surface (_fun _cairo_surface_t -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
(define-cairo cairo_pattern_reference (_fun _cairo_pattern_t -> _void)
  #:wrap (retainer cairo_pattern_destroy car))
(define-cairo cairo_pattern_set_matrix (_fun _cairo_pattern_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_pattern_set_extend (_fun _cairo_pattern_t _int -> _void))

;; Surfaces
(define-cairo cairo_surface_finish (_fun _cairo_surface_t -> _void))
(define-cairo cairo_surface_flush (_fun _cairo_surface_t -> _void))
(define-cairo cairo_surface_mark_dirty (_fun _cairo_surface_t -> _void))
(define-cairo cairo_image_surface_create (_fun _int _int _int -> _cairo_surface_t)
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_ps_surface_create (_fun _path _double* _double* -> _cairo_surface_t)
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_ps_surface_set_eps (_fun _cairo_surface_t _bool -> _void))
(define-cairo cairo_ps_surface_dsc_begin_setup (_fun _cairo_surface_t -> _void))
(define-cairo cairo_ps_surface_dsc_comment (_fun _cairo_surface_t _string -> _void))
(define-cairo cairo_image_surface_get_data (_fun (s : _cairo_surface_t)
                                                 -> (_bytes o
                                                            (* (cairo_image_surface_get_height s)
                                                               (cairo_image_surface_get_stride s)))))
(define-cairo cairo_image_surface_get_width (_fun _cairo_surface_t -> _int))
(define-cairo cairo_image_surface_get_height (_fun _cairo_surface_t -> _int))
(define-cairo cairo_image_surface_get_stride (_fun _cairo_surface_t -> _int))
(define-cairo cairo_image_surface_get_format (_fun _cairo_surface_t -> _int))

;; Not recommended, because it's not registered as an allocator (can't
;; call it in atomic mode):
(define-cairo cairo_image_surface_create_from_png_stream (_fun (_fun _pointer
                                                                     (s : _pointer)
                                                                     (len : _int) 
                                                                     -> _int)
                                                               (_pointer = #f)
                                                               -> _cairo_surface_t/null))

(define-cairo cairo_surface_write_to_png_stream (_fun _cairo_surface_t
                                                      (_fun _pointer
                                                            (s : _pointer)
                                                            (len : _int) 
                                                            -> _int)
                                                      (_pointer = #f)
                                                      -> _int))

(define-enum
  0
  CAIRO_OPERATOR_CLEAR

  CAIRO_OPERATOR_SOURCE
  CAIRO_OPERATOR_OVER
  CAIRO_OPERATOR_IN
  CAIRO_OPERATOR_OUT
  CAIRO_OPERATOR_ATOP
  
  CAIRO_OPERATOR_DEST
  CAIRO_OPERATOR_DEST_OVER
  CAIRO_OPERATOR_DEST_IN
  CAIRO_OPERATOR_DEST_OUT
  CAIRO_OPERATOR_DEST_ATOP
  
  CAIRO_OPERATOR_XOR
  CAIRO_OPERATOR_ADD
  CAIRO_OPERATOR_SATURATE)

(define-enum
  0
  CAIRO_LINE_CAP_BUTT
  CAIRO_LINE_CAP_ROUND
  CAIRO_LINE_CAP_SQUARE)

(define-enum
  0
  CAIRO_LINE_JOIN_MITER
  CAIRO_LINE_JOIN_ROUND
  CAIRO_LINE_JOIN_BEVEL)

(define-enum
  0
  CAIRO_FILL_RULE_WINDING
  CAIRO_FILL_RULE_EVEN_ODD)

(define-enum
  0
  CAIRO_ANTIALIAS_DEFAULT
  CAIRO_ANTIALIAS_NONE
  CAIRO_ANTIALIAS_GRAY
  CAIRO_ANTIALIAS_SUBPIXEL)

(define-enum
  0
  CAIRO_FORMAT_ARGB32
  CAIRO_FORMAT_RGB24
  CAIRO_FORMAT_A8
  CAIRO_FORMAT_A1)

(define-enum
  0
  CAIRO_STATUS_SUCCESS)

(define-enum
  0
  CAIRO_EXTEND_NONE
  CAIRO_EXTEND_REPEAT
  CAIRO_EXTEND_REFLECT
  CAIRO_EXTEND_PAD)

(define/provide CAIRO_CONTENT_COLOR_ALPHA #x3000)
