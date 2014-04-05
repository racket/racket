#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "cairo-lib.rkt"
         "../private/libs.rkt"
         "../private/utils.rkt")

(define-ffi-definer define-cairo cairo-lib
  #:provide provide-protected)

(provide _cairo_t
         _cairo_surface_t
         _cairo_font_options_t)

;; ALLOCATION NOTE: drawing to a Cairo surface might call back to
;; Racket, because a drawing suface might be a PDF or SVG file
;; that is written through a callback to Racket. Consequently,
;; all GC-allocated arguments to Cairo functions must be allocated
;; a 'atomic-interior, so that they do not move in case of a GC.

(define _cairo_surface_t (_cpointer 'cairo_surface_t))
(define _cairo_surface_t/null (_cpointer/null 'cairo_surface_t))
(define _cairo_t (_cpointer 'cairo_t))
(define _cairo_pattern_t (_cpointer 'cairo_pattern_t))
(define _cairo_font_options_t (_cpointer/null 'cairo_font_options_t))
(define _CGContextRef (_cpointer 'CGContextRef))

(define-cstruct _cairo_matrix_t ([xx _double*]
                                 [yx _double*]
                                 [xy _double*]
                                 [yy _double*]
                                 [x0 _double*]
                                 [y0 _double*])
  #:malloc-mode 'atomic-interior)
(provide (struct-out cairo_matrix_t))

(define-cstruct _cairo_glyph_t ([index _long] [x _double*] [y _double*]))
(provide make-cairo_glyph_t)

(define-fun-syntax _ptr/immobile
  (syntax-id-rules (_ptr/immobile o)
    [(_ptr/immobile o t) (type: _pointer
                                pre:  (malloc t 'atomic-interior)
                                post: (x => (ptr-ref x t)))]))
 
;; Cairo is supposed to be thread-safe, but concurrent use seems
;; to cause trouble right now. (Try rendering the "plot" document
;; in multiple places at once.) For now, treat Cairo as non-thread
;; safe. Use `_cfun' for Cairo functions and `_cbfun' for callbacks:
(define-syntax-rule (_cfun . rest)
  (_fun #:in-original-place? #t . rest))
(define-syntax-rule (_cbfun . rest)
  (_fun #:async-apply (lambda (f) (f)) . rest))

(define-cairo cairo_destroy (_cfun _cairo_t -> _void) 
  #:wrap (deallocator))

(define-cairo cairo_surface_destroy (_cfun _cairo_surface_t -> _void)
  #:wrap (deallocator))

(define-cairo cairo_quartz_surface_create
  (_cfun _int _uint _uint -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_quartz_surface_create_for_cg_context
  (_cfun _CGContextRef _uint _uint -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_quartz_surface_get_cg_context
  (_cfun _cairo_surface_t -> _CGContextRef)
  #:make-fail make-not-available)

(define-cairo cairo_win32_surface_create
  (_cfun _pointer -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_win32_printing_surface_create
  (_cfun _pointer -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))

(define-cairo cairo_surface_create_similar
  (_cfun _cairo_surface_t _int _int _int -> _cairo_surface_t))

(define-cairo cairo_xlib_surface_create (_cfun _pointer ; Display*
                                               _ulong   ; Drawable
                                               _pointer ; Visual*
                                               _int     ; width
                                               _int     ; height
                                               -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))

(define-cairo cairo_win32_surface_create_with_dib 
  (_cfun _int _int _int -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_win32_surface_create_with_ddb 
  (_cfun _pointer _int _int _int -> _cairo_surface_t)
  #:make-fail make-not-available
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_win32_surface_get_dc
  (_cfun _cairo_surface_t -> _pointer)
  #:make-fail make-not-available)

(define-cairo cairo_create (_cfun _cairo_surface_t -> _cairo_t)
  #:wrap (allocator cairo_destroy))

(define-cairo cairo_get_target (_cfun _cairo_t -> _cairo_surface_t)) ;; not an allocator

(define-cairo cairo_surface_get_type (_cfun _cairo_surface_t -> _int))

;; Context
(define-cairo cairo_paint (_cfun _cairo_t -> _void))
(define-cairo cairo_paint_with_alpha (_cfun _cairo_t _double* -> _void))
(define-cairo cairo_fill (_cfun _cairo_t -> _void))
(define-cairo cairo_fill_preserve (_cfun _cairo_t -> _void))
(define-cairo cairo_stroke (_cfun _cairo_t -> _void))
(define-cairo cairo_stroke_preserve (_cfun _cairo_t -> _void))
(define-cairo cairo_save (_cfun _cairo_t -> _void))
(define-cairo cairo_restore (_cfun _cairo_t -> _void))
(define-cairo cairo_clip (_cfun _cairo_t -> _void))
(define-cairo cairo_reset_clip (_cfun _cairo_t -> _void))

(define-cairo cairo_in_fill (_cfun _cairo_t _double* _double* -> _bool))

(define-cairo cairo_clip_extents (_cfun _cairo_t 
                                       (x1 : (_ptr/immobile o _double)) 
                                       (y1 : (_ptr/immobile o _double)) 
                                       (x2 : (_ptr/immobile o _double)) 
                                       (y2 : (_ptr/immobile o _double)) 
                                       -> _void
                                       -> (values x1 y1 x2 y2))
  ;; cairo_clip_extents is in version 1.4 and later
  #:fail (lambda ()
           (let ([warned? #f])
             (lambda (cr) 
               (unless warned?
                 (log-warning "cairo_clip_extents is unavailable; returning the empty rectangle")
                 (set! warned? #t))
               (values 0 0 0 0)))))

(define-cstruct _cairo_rectangle_t ([x _double]
                                    [y _double]
                                    [width _double]
                                    [height _double]))
(define-cstruct _cairo_rectangle_list_t ([status _int]
                                         [rectangles _cairo_rectangle_t-pointer/null]
                                         [num_rectangles _int]))
(provide (struct-out cairo_rectangle_t) _cairo_rectangle_t
         (struct-out cairo_rectangle_list_t))
(define-cairo cairo_rectangle_list_destroy (_cfun _cairo_rectangle_list_t-pointer -> _void)
  #:wrap (deallocator)
  ;; cairo_rectangle_list_destroy is in 1.4 and later
  #:fail (lambda () (lambda (l) (void))))
(define-cairo cairo_copy_clip_rectangle_list (_cfun _cairo_t -> _cairo_rectangle_list_t-pointer)
  #:wrap (allocator cairo_rectangle_list_destroy)
  ;; cairo_copy_clip_rectangle_list is in 1.4 and later
  #:fail (lambda () (lambda (c) (make-cairo_rectangle_list_t -1 #f 0))))

(define-cairo cairo_fill_extents (_cfun _cairo_t 
                                        (x1 : (_ptr/immobile o _double)) 
                                        (y1 : (_ptr/immobile o _double)) 
                                        (x2 : (_ptr/immobile o _double)) 
                                        (y2 : (_ptr/immobile o _double)) 
                                        -> _void
                                        -> (values x1 y1 x2 y2)))

(define-cairo cairo_stroke_extents (_cfun _cairo_t 
                                          (x1 : (_ptr/immobile o _double)) 
                                          (y1 : (_ptr/immobile o _double)) 
                                          (x2 : (_ptr/immobile o _double)) 
                                          (y2 : (_ptr/immobile o _double)) 
                                          -> _void
                                          -> (values x1 y1 x2 y2)))

;; Transforms
(define-cairo cairo_translate (_cfun _cairo_t _double* _double* -> _void))
(define-cairo cairo_scale (_cfun _cairo_t _double* _double* -> _void))
(define-cairo cairo_rotate (_cfun _cairo_t _double* -> _void))
(define-cairo cairo_transform (_cfun _cairo_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_identity_matrix (_cfun _cairo_t -> _void))
(define-cairo cairo_get_matrix (_cfun _cairo_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_set_matrix (_cfun _cairo_t _cairo_matrix_t-pointer -> _void))

(define-cairo cairo_matrix_init_translate (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_init (_cfun _cairo_matrix_t-pointer _double* _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_matrix_translate (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_scale (_cfun _cairo_matrix_t-pointer _double* _double* -> _void))
(define-cairo cairo_matrix_rotate (_cfun _cairo_matrix_t-pointer _double* -> _void))
(define-cairo cairo_matrix_multiply (_cfun _cairo_matrix_t-pointer _cairo_matrix_t-pointer _cairo_matrix_t-pointer -> _void))

;; Stroke & Fill
(define-cairo cairo_set_source_rgb (_cfun _cairo_t _double* _double* _double* -> _void))
(define-cairo cairo_set_source_rgba (_cfun _cairo_t _double* _double* _double* _double* -> _void))
(define-cairo cairo_set_line_width (_cfun _cairo_t _double* -> _void))
(define-cairo cairo_set_line_cap (_cfun _cairo_t _int -> _void))
(define-cairo cairo_set_line_join (_cfun _cairo_t _int -> _void))
(define-cairo cairo_set_dash (_cfun _cairo_t (v : (_vector i _double*)) [_int = (vector-length v)] _double* -> _void))
(define-cairo cairo_set_antialias (_cfun _cairo_t _int -> _void))

(define-cairo cairo_set_fill_rule (_cfun _cairo_t _int -> _void))

(define-cairo cairo_get_operator (_cfun _cairo_t -> _int))
(define-cairo cairo_set_operator (_cfun _cairo_t _int -> _void))

;; Text
(define-cairo cairo_font_options_destroy (_cfun _cairo_font_options_t -> _void)
  #:wrap (deallocator))
(define-cairo cairo_font_options_create (_cfun -> _cairo_font_options_t)
  #:wrap (allocator cairo_font_options_destroy))
(define-cairo cairo_font_options_copy (_cfun _cairo_font_options_t _cairo_font_options_t -> _void))
(define-cairo cairo_get_font_options (_cfun _cairo_t _cairo_font_options_t -> _void))
(define-cairo cairo_set_font_options (_cfun _cairo_t _cairo_font_options_t -> _void))
(define-cairo cairo_font_options_set_antialias (_cfun _cairo_font_options_t _int -> _void))
(define-cairo cairo_font_options_set_hint_metrics (_cfun _cairo_font_options_t _int -> _void))
(define-cairo cairo_font_options_set_hint_style (_cfun _cairo_font_options_t _int -> _void))

(define-cairo cairo_show_glyphs (_cfun _cairo_t
                                       _cairo_glyph_t-pointer ; must be immobile
                                       _int
                                       -> _void))

;; Paths
(define-cairo cairo_rectangle (_cfun _cairo_t _double* _double* _double* _double* -> _void))
(define-cairo cairo_move_to (_cfun _cairo_t _double* _double* -> _void))
(define-cairo cairo_rel_move_to (_cfun _cairo_t _double* _double* -> _void))
(define-cairo cairo_line_to (_cfun _cairo_t _double* _double* -> _void))
(define-cairo cairo_rel_line_to (_cfun _cairo_t _double* _double* -> _void))
(define-cairo cairo_arc (_cfun _cairo_t _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_arc_negative (_cfun _cairo_t _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_curve_to (_cfun _cairo_t _double* _double* _double* _double* _double* _double* -> _void))
(define-cairo cairo_new_path (_cfun _cairo_t -> _void))
(define-cairo cairo_close_path (_cfun _cairo_t -> _void))

(define-cairo cairo_show_page (_cfun _cairo_t -> _void))

;; Patterns
(define-cairo cairo_set_source (_cfun _cairo_t _cairo_pattern_t -> _void))
(define-cairo cairo_get_source (_cfun _cairo_t -> _cairo_pattern_t)) ;; not an allocator
(define-cairo cairo_set_source_surface (_cfun _cairo_t _cairo_surface_t _double* _double* -> _void))
(define-cairo cairo_mask (_cfun _cairo_t _cairo_pattern_t -> _void))
(define-cairo cairo_mask_surface (_cfun _cairo_t _cairo_surface_t _double* _double* -> _void))
(define-cairo cairo_pattern_destroy (_cfun _cairo_pattern_t -> _void)
  #:wrap (deallocator))
(define-cairo cairo_pattern_create_for_surface (_cfun _cairo_surface_t -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
(define-cairo cairo_pattern_reference (_cfun _cairo_pattern_t -> _void)
  #:wrap (retainer cairo_pattern_destroy car))
(define-cairo cairo_pattern_set_matrix (_cfun _cairo_pattern_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_pattern_set_extend (_cfun _cairo_pattern_t _int -> _void))

;; Gradients
(define-cairo cairo_pattern_add_color_stop_rgb (_cfun _cairo_pattern_t _double* _double* _double* _double* -> _void))
(define-cairo cairo_pattern_add_color_stop_rgba (_cfun _cairo_pattern_t _double* _double* _double* _double* _double* -> _void))
#; ; 1.4 and later:
(define-cairo cairo_pattern_get_color_stop_count (_cfun _cairo_pattern_t (_ptr o _int)  -> _int)
  #:make-fail make-not-available)
#; ; 1.4 and later:
(define-cairo cairo_pattern_get_color_stop_rgba (_cfun _cairo_pattern_t _int (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available)

#; ; 1.4 and later:
(define-cairo cairo_pattern_create_rgb (_cfun _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
#; ; 1.4 and later:
(define-cairo cairo_pattern_create_rgba (_cfun _double* _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
#; ; 1.4 and later:
(define-cairo cairo_pattern_get_rgba (_cfun _cairo_pattern_t (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available) ;; not an allocator
#; ; 1.4 and later:
(define-cairo cairo_pattern_get_surface (_cfun _cairo_pattern_t (_ptr o _cairo_surface_t) -> _int)
  #:make-fail make-not-available) ;; not an allocator

(define-cairo cairo_pattern_create_linear (_cfun _double* _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
#; ; 1.4 and later:
(define-cairo cairo_pattern_get_linear_points (_cfun _cairo_pattern_t (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available)
(define-cairo cairo_pattern_create_radial (_cfun _double* _double* _double* _double* _double* _double* -> _cairo_pattern_t)
  #:wrap (allocator cairo_pattern_destroy))
#; ; 1.4 and later:
(define-cairo cairo_pattern_get_radial_circles (_cfun _cairo_pattern_t (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) (_ptr o _double*) -> _int)
  #:make-fail make-not-available)
(define-cairo cairo_pattern_status (_cfun _cairo_pattern_t -> _int))

(define-cairo cairo_pattern_get_extend (_cfun _cairo_pattern_t -> _int))
(define-cairo cairo_pattern_set_filter (_cfun _cairo_pattern_t _int -> _void))
(define-cairo cairo_pattern_get_filter (_cfun _cairo_pattern_t -> _int))
(define-cairo cairo_pattern_get_matrix (_cfun _cairo_pattern_t _cairo_matrix_t-pointer -> _void))
(define-cairo cairo_pattern_get_type (_cfun _cairo_pattern_t -> _int))


;; Surfaces
(define-cairo cairo_surface_finish (_cfun _cairo_surface_t -> _void))
(define-cairo cairo_surface_flush (_cfun _cairo_surface_t -> _void))
(define-cairo cairo_surface_mark_dirty (_cfun _cairo_surface_t -> _void))
(define-cairo cairo_image_surface_create (_cfun _int _int _int -> _cairo_surface_t)
  #:wrap (allocator cairo_surface_destroy))
(define-cairo cairo_ps_surface_create (_cfun _path _double* _double* -> _cairo_surface_t)
  #:wrap (allocator cairo_surface_destroy))
#; ; 1.2 and later:
(define-cairo cairo_surface_set_fallback_resolution (_cfun _cairo_surface_t _double* _double* -> _void))
#; ; 1.8 and later:
(define-cairo cairo_surface_get_fallback_resolution (_cfun _cairo_surface_t (x : (_ptr o _double)) (y :  (_ptr o _double))
                                                           -> _void
                                                           -> (values x y)))

;; Stream surfaces

;;  The first argument to a stream-surface creation
;;  function is a procedure, and we need the procedure to
;;  live just as long as the surface. Implement that by
;;  saving the closure via user data on the surface.
;;  Externally, a stream-creation function takes
;;  just a closure --- not a function and data.
(define _cairo_write_func_t 
  (_cbfun #:atomic? #t _pointer _pointer _uint -> _int))
(define _stream-surface-proc 
  (_cfun _cairo_write_func_t _pointer _double* _double* -> _cairo_surface_t))
(define cell-key (malloc 1 'raw))
(define stream-surface-allocator 
  (lambda (p)
    ((allocator cairo_surface_destroy)
     (lambda (proc w h)
       (let* ([new-proc (lambda (null bytes len)
                          (proc bytes len))]
              [free-cell-box (box #f)]
              [s (p new-proc #f w h)]
              [b (malloc-immobile-cell (cons new-proc free-cell-box))])
         (parameterize ([current-sud-box free-cell-box])
           (cairo_surface_set_user_data s cell-key b free-immobile-cell))
         s)))))
(define-cairo cairo_ps_surface_create_for_stream
  _stream-surface-proc
  #:wrap stream-surface-allocator)
(define-cairo cairo_pdf_surface_create_for_stream 
  _stream-surface-proc
  #:wrap stream-surface-allocator)
(define-cairo cairo_svg_surface_create_for_stream 
  _stream-surface-proc
  #:wrap stream-surface-allocator)

(define current-sud-box (make-parameter #f))
(define-cairo cairo_surface_set_user_data 
  (_cfun _cairo_surface_t _pointer _pointer 
        (_cbfun #:atomic? #t 
                #:keep (lambda (v) (set-box! (current-sud-box) v))
                _pointer -> _void)
        -> _int))

(define-cairo cairo_ps_surface_set_eps (_cfun _cairo_surface_t _bool -> _void)
  #:fail (lambda ()
	   ;; cairo_ps_surface_set_eps is in version 1.6 and later;
	   ;; if it's not available, we just do without
	   (lambda (s b) (void))))
(define-cairo cairo_ps_surface_dsc_begin_setup (_cfun _cairo_surface_t -> _void))
(define-cairo cairo_ps_surface_dsc_comment (_cfun _cairo_surface_t _string -> _void))
(define-cairo cairo_image_surface_get_data (_cfun (s : _cairo_surface_t)
                                                 -> (_bytes o
                                                            (* (cairo_image_surface_get_height s)
                                                               (cairo_image_surface_get_stride s)))))
(define-cairo cairo_image_surface_get_width (_cfun _cairo_surface_t -> _int))
(define-cairo cairo_image_surface_get_height (_cfun _cairo_surface_t -> _int))
(define-cairo cairo_image_surface_get_stride (_cfun _cairo_surface_t -> _int))
(define-cairo cairo_image_surface_get_format (_cfun _cairo_surface_t -> _int))

;; Not recommended, because it's not registered as an allocator (can't
;; call it in atomic mode):
(define-cairo cairo_image_surface_create_from_png_stream (_cfun (_cbfun _pointer
                                                                        (s : _pointer)
                                                                        (len : _int) 
                                                                        -> _int)
                                                                (_pointer = #f)
                                                                -> _cairo_surface_t/null))
;; Not recommended, unless it makes sense to make the allback atomic:
(define-cairo cairo_surface_write_to_png_stream (_cfun _cairo_surface_t
                                                      (_cbfun _pointer
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

(define-enum
  0
  CAIRO_HINT_METRICS_DEFAULT
  CAIRO_HINT_METRICS_OFF
  CAIRO_HINT_METRICS_ON)

(define-enum
  0
  CAIRO_HINT_STYLE_DEFAULT
  CAIRO_HINT_STYLE_NONE
  CAIRO_HINT_STYLE_SLIGHT
  CAIRO_HINT_STYLE_MEDIUM
  CAIRO_HINT_STYLE_FULL)

(define-enum
  0
  CAIRO_PATTERN_TYPE_SOLID
  CAIRO_PATTERN_TYPE_SURFACE
  CAIRO_PATTERN_TYPE_LINEAR
  CAIRO_PATTERN_TYPE_RADIAL)

(define-enum
  0
  CAIRO_FILTER_FAST
  CAIRO_FILTER_GOOD
  CAIRO_FILTER_BEST
  CAIRO_FILTER_NEAREST
  CAIRO_FILTER_BILINEAR
  CAIRO_FILTER_GAUSSIAN)

(define/provide CAIRO_CONTENT_COLOR_ALPHA #x3000)

(define-enum 
  6
  CAIRO_SURFACE_TYPE_QUARTZ)

;; ----------------------------------------

(define-cstruct _cairo_path_data_t_header ([type _int]
                                           [length _int]))
(define-cstruct _cairo_path_data_t_point ([x _double]
                                          [y _double]))

(define _cairo_path_data_t (_union
                            _cairo_path_data_t_header
                            _cairo_path_data_t_point))

(define-cstruct _cairo_path_t ([status _int]
                               [data _pointer]
                               [num_data _int]))

(define-cairo cairo_path_destroy (_cfun _cairo_path_t-pointer -> _void)
  #:wrap (deallocator))

(define-cairo cairo_copy_path (_cfun _cairo_t -> _cairo_path_t-pointer)
  #:wrap (allocator cairo_path_destroy))

(define-cairo cairo_path_extents (_cfun _cairo_t 
                                        (x1 : (_ptr o _double)) 
                                        (y1 : (_ptr o _double)) 
                                        (x2 : (_ptr o _double)) 
                                        (y2 : (_ptr o _double)) 
                                        -> _void
                                        -> (values x1 y1 x2 y2))
  ;; cairo_path_extents is in version 1.6 and later
  #:fail (lambda ()
           cairo_stroke_extents))

(define-enum 0
  CAIRO_PATH_MOVE_TO
  CAIRO_PATH_LINE_TO
  CAIRO_PATH_CURVE_TO
  CAIRO_PATH_CLOSE_PATH)

(provide cairo-path->list)

(define (cairo-path->list path)
  (define len (cairo_path_t-num_data path))
  (define data (cairo_path_t-data path))
  (let loop ([i 0])
    (if (= i len)
        null
        (let ([h (union-ref (ptr-ref data _cairo_path_data_t i) 0)])
          (cons (let ([t (cairo_path_data_t_header-type h)])
                  (cond
                   [(or (= t CAIRO_PATH_MOVE_TO)
                        (= t CAIRO_PATH_LINE_TO))
                    (define a (union-ref (ptr-ref data _cairo_path_data_t (add1 i)) 1))
                    (list (if (= t CAIRO_PATH_MOVE_TO) 'move 'line)
                          (cairo_path_data_t_point-x a)
                          (cairo_path_data_t_point-y a))]
                   [(= t CAIRO_PATH_CURVE_TO)
                    (define a (union-ref (ptr-ref data _cairo_path_data_t (+ 1 i)) 1))
                    (define b (union-ref (ptr-ref data _cairo_path_data_t (+ 2 i)) 1))
                    (define c (union-ref (ptr-ref data _cairo_path_data_t (+ 3 i)) 1))
                    (list 'curve
                          (cairo_path_data_t_point-x a) (cairo_path_data_t_point-y a)
                          (cairo_path_data_t_point-x b) (cairo_path_data_t_point-y b)
                          (cairo_path_data_t_point-x c) (cairo_path_data_t_point-y c))]
                   [(= t CAIRO_PATH_CLOSE_PATH)
                    '(close)]))
                (loop (+ i (cairo_path_data_t_header-length h))))))))
