#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "glib.rkt"
         "cairo.rkt"
         "../private/utils.rkt"
         "../private/libs.rkt")

(define-runtime-lib pango-lib
  [(unix) (ffi-lib "libpango-1.0" '("0" ""))]
  [(macosx) (ffi-lib "libpango-1.0.0.dylib")]
  [(windows) (ffi-lib "libpango-1.0-0.dll")])

(define-runtime-lib pangowin32-lib
  [(unix) #f]
  [(macosx)]
  [(windows)
   (ffi-lib "libpangowin32-1.0-0.dll")])

(define-runtime-lib pangocairo-lib
  [(unix) (ffi-lib "libpangocairo-1.0" '("0" ""))]
  [(macosx)
   (ffi-lib "libharfbuzz.0.dylib")
   (ffi-lib "libpangoft2-1.0.0.dylib")
   (ffi-lib "libpangocairo-1.0.0.dylib")]
  [(windows)
   (ffi-lib "libintl-8.dll")
   (ffi-lib "libpangowin32-1.0-0.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "libfreetype-6.dll")
   (ffi-lib "libfontconfig-1.dll")
   (ffi-lib "libharfbuzz-0.dll")
   (ffi-lib "libpangoft2-1.0-0.dll")
   (ffi-lib "libpangocairo-1.0-0.dll")])

(define-ffi-definer define-pango pango-lib
  #:provide provide)
(define-ffi-definer define-pangocairo pangocairo-lib
  #:provide provide)
(define-ffi-definer define-pangowin32 pangowin32-lib
  #:provide provide)

;; Pango's Core Text back-end can somehow go wrong if we're going to eventually
;; use AppKit but don't load AppKit it before using functions such as
;; `pango_cairo_font_map_get_default'. So, force AppKit now for the platform
;; where the Core Text back-end is used:
(when (equal? "x86_64-macosx/3m"
              (path->string (system-library-subpath)))
  (void (ffi-lib (format "/System/Library/Frameworks/AppKit.framework/AppKit"))))

;; ALLOCATION NOTE: since Pango calls into Cairo, it has the same
;; allocation constraints on arguments as Cairo functions; see
;; "cairo.rkt".

(define PangoContext (_cpointer 'PangoContext))
(define PangoLayout (_cpointer 'PangoLayout))
(define PangoFontDescription (_cpointer 'PangoFontDescription))
(define PangoFontFamily (_cpointer 'PangoFontFamily))
(define PangoFontFace (_cpointer 'PangoFontFace))
(define PangoFont (_cpointer 'PangoFont))
(define PangoFontMap (_cpointer 'PangoFontMap))
(define PangoFontMetrics (_cpointer 'PangoFontMetrics))
(define PangoAttrList (_cpointer 'PangoAttrList))
(define PangoAttribute (_cpointer 'PangoAttribute))
(define PangoLanguage (_cpointer 'PangoLanguage))
(define PangoCoverage (_cpointer 'PangoCoverage))
(define PangoLayoutIter (_cpointer 'PangoLayoutIter))
(define PangoLayoutLine (_cpointer 'PangoLayoutLine))

(define-cstruct _PangoRectangle ([x _int]
                                 [y _int]
                                 [width _int]
                                 [height _int])
  #:malloc-mode 'atomic-interior)
(provide make-PangoRectangle
         PangoRectangle-x
         PangoRectangle-y
         PangoRectangle-width
         PangoRectangle-height)

(define-cstruct _PangoItem
  ([offset _int]
   [length _int]
   [num_chars _int]
   ;; Inline PangoAnalysis:
   [shape_engine _pointer]
   [lang_engine _pointer]
   [font (_or-null PangoFont)]
   [level _uint8]
   [gravity _uint8]
   [flags _uint8]
   [script _uint8]
   [language _pointer]
   [extra_attrs _pointer]))

(provide (struct-out PangoItem)
         _PangoItem _PangoItem-pointer)

(define-cstruct _PangoGlyphInfo
  ([glyph _uint32]
   [width _uint32]
   [dx _uint32]
   [dy _uint32]
   [is_cluster_start _uint]))

(provide (struct-out PangoGlyphInfo)
         _PangoGlyphInfo _PangoGlyphInfo-pointer)

(define-cstruct _PangoGlyphString
  ([num_glyphs _int]
   [glyphs _pointer]
   [log_clusters _pointer])
  #:malloc-mode 'atomic-interior)

(provide (struct-out PangoGlyphString)
         _PangoGlyphString)

(define-cstruct _PangoGlyphItem ([item _PangoItem-pointer]
                                 [glyphs _PangoGlyphString-pointer]))
(provide (struct-out PangoGlyphItem))

;; As of Pango 1.28, Pango is not thread-safe at the C level, which
;; means that it isn't place-safe in Racket. Also, for some reason,
;; when parts of Pango are initialized in a non-main place under
;; Windows, then font operations start to fail when that place exits.
;; Run all Pango calls in the original place, which synchronizes them
;; and avoids Windows problems.
(define-syntax-rule (_pfun spec ...)
  (_fun #:in-original-place? #t spec ...))

(provide g_object_unref g_free)
(define-gobj g_object_unref (_pfun _pointer -> _void)
  #:wrap (deallocator))
(define-glib g_free (_pfun _pointer -> _void)
  #:wrap (deallocator))

;; For working around a Win32 Pango bug (see `unref-font-map'):
(define _GQueue (_cpointer 'GQueue))
(define-cstruct _PangoWin32FontMap ([type-instance _pointer]
				    [ref_count _uint]
				    [qdata _pointer]
				    [font_cache _pointer]
				    [freed_fonts _GQueue]))
(define-glib g_queue_foreach (_pfun _GQueue _fpointer #;(_fun _pointer -> _void) _pointer -> _void))
(define-glib g_queue_free (_pfun _GQueue -> _void))
(define-glib g_queue_new (_pfun -> _GQueue))
(define-gobj raw_g_object_unref _fpointer #:c-id g_object_unref)

(define (unref-font-map v)
  (when (eq? (system-type) 'windows)
    ;; For version 1.28 of Pango, reported as Bug 649293:
    ;; Under Windows, PangoWin32FontMap holds a queue of freed
    ;; fonts, and the fonts hold a weak link back to the map.
    ;; Unreffing the font map drops the weak links and *then*
    ;; tries to release the freed fonts, which leads to failures
    ;; releasing the fonts. Work around the bug by manually
    ;; flushing the queue of freed fonts before the font map is
    ;; unreffed.
    (let ([fm (cast v _pointer _PangoWin32FontMap-pointer)])
      (g_queue_foreach (PangoWin32FontMap-freed_fonts fm) raw_g_object_unref #f)
      (g_queue_free (PangoWin32FontMap-freed_fonts fm))
      (set-PangoWin32FontMap-freed_fonts! fm (g_queue_new))))
  (g_object_unref v))

(define-pangocairo pango_cairo_font_map_get_default (_pfun -> PangoFontMap)) ;; not an allocator
(define-pangocairo pango_cairo_font_map_new (_pfun -> PangoFontMap)
  #:wrap (allocator unref-font-map))

(define-pango pango_context_new (_pfun -> PangoContext)
  #:wrap (allocator g_object_unref))
;; pango_font_map_create_context() is in 1.22 and later
(provide pango_font_map_create_context)
(define (pango_font_map_create_context fm)
  (let ([c (pango_context_new)])
    (pango_context_set_font_map c fm)
    c))
(define-pangocairo pango_cairo_update_context (_pfun _cairo_t PangoContext -> _void))

;; The convenince function pango_cairo_create_context() is in 1.22 and later
(provide pango_cairo_create_context)
(define (pango_cairo_create_context cr)
  (let ([ctx (pango_font_map_create_context
              (pango_cairo_font_map_get_default))])
    (pango_cairo_update_context cr ctx)
    ctx))

(define-pangocairo pango_cairo_create_layout (_pfun _cairo_t -> PangoLayout)
  #:wrap (allocator g_object_unref))
(define-pangocairo pango_cairo_update_layout (_pfun _cairo_t PangoLayout -> _void))
(define-pango pango_layout_set_text (_pfun PangoLayout [s : _string] [_int = -1] -> _void))
(define-pangocairo pango_cairo_show_layout (_pfun _cairo_t PangoLayout -> _void))
(define-pangocairo pango_cairo_show_layout_line (_pfun _cairo_t PangoLayoutLine -> _void))
(define-pangocairo pango_cairo_show_glyph_string (_pfun _cairo_t PangoFont _PangoGlyphString-pointer -> _void))
(define-pangocairo pango_cairo_layout_line_path (_pfun _cairo_t PangoLayoutLine -> _void))

(define-pango pango_layout_iter_free (_pfun PangoLayoutIter -> _void)
  #:wrap (deallocator))
(define-pango pango_layout_get_iter (_pfun PangoLayout -> PangoLayoutIter)
  #:wrap (allocator pango_layout_iter_free))
(define-pango pango_layout_iter_get_baseline (_pfun PangoLayoutIter -> _int))
(define-pango pango_layout_iter_next_run (_pfun PangoLayoutIter -> _bool))
(define-pango pango_layout_iter_get_run (_pfun PangoLayoutIter -> (_or-null _PangoGlyphItem-pointer)))
(define-pango pango_layout_iter_get_run_readonly (_pfun PangoLayoutIter -> (_or-null _PangoGlyphItem-pointer))
  #:fail (lambda () pango_layout_iter_get_run))

(define-pango pango_layout_get_line (_pfun PangoLayout _int -> PangoLayoutLine))
(define-pango pango_layout_get_line_readonly (_pfun PangoLayout _int -> PangoLayoutLine)
  #:fail (lambda () pango_layout_get_line))

(define-pango pango_layout_get_context (_pfun PangoLayout -> PangoContext)) ;; not an allocator
(define-pango pango_layout_get_extents (_pfun PangoLayout  _pointer _PangoRectangle-pointer -> _void))
(define-pango pango_layout_get_baseline (_pfun PangoLayout -> _int)
  ;; The convenince function pango_layout_get_baseline() is in 1.22 and later
  #:fail (lambda ()
           (lambda (layout)
             (let ([iter (pango_layout_get_iter layout)])
               (begin0
                (pango_layout_iter_get_baseline iter)
                (pango_layout_iter_free iter))))))
(define-pango pango_layout_get_spacing (_pfun PangoLayout -> _int))

(define-pango pango_layout_new (_pfun PangoContext -> PangoLayout)
  #:wrap (allocator g_object_unref))

(define-pangocairo pango_cairo_context_get_font_options (_pfun PangoContext -> _cairo_font_options_t)) ;; not an allocator
(define-pangocairo pango_cairo_context_set_font_options (_pfun PangoContext _cairo_font_options_t -> _void)) ;; makes a copy

(define-pango pango_layout_set_font_description (_pfun PangoLayout PangoFontDescription -> _void)) ;; makes a copy
(define-pango pango_context_get_font_map (_pfun PangoContext -> PangoFontMap)) ;; not an allocator
(define-pango pango_context_set_font_map (_pfun PangoContext PangoFontMap -> _void))
(define-pango pango_font_family_get_name (_pfun PangoFontFamily -> _string)) ;; not an allocator
(define-pango pango_font_family_is_monospace (_pfun PangoFontFamily -> _bool))

(define-pango pango_language_get_default (_pfun -> PangoLanguage)
  ;; not available before 1.16
  #:fail (lambda () (lambda () #f)))
(define-pango pango_font_map_load_font (_pfun PangoFontMap PangoContext PangoFontDescription -> (_or-null PangoFont)))
(define-pango pango_coverage_unref (_pfun PangoCoverage -> _void)
  #:wrap (deallocator))
(define-pango pango_font_get_coverage (_pfun PangoFont PangoLanguage -> PangoCoverage)
  #:wrap (allocator pango_coverage_unref))
(define-pango pango_coverage_get (_pfun PangoCoverage _int -> _int))

(define-pango pango_font_metrics_unref (_pfun PangoFontMetrics -> _void)
  #:wrap (deallocator))
(define-pango pango_font_get_metrics (_pfun PangoFont (_or-null PangoLanguage) -> PangoFontMetrics)
  #:wrap (allocator pango_font_metrics_unref))
(define-pango pango_font_metrics_get_approximate_char_width (_pfun PangoFontMetrics -> _int))
(define-pango pango_font_metrics_get_ascent (_pfun PangoFontMetrics -> _int))
(define-pango pango_font_metrics_get_descent (_pfun PangoFontMetrics -> _int))

(define-pango pango_layout_get_unknown_glyphs_count (_pfun PangoLayout -> _int)
  ;; not available in old versions:
  #:fail (lambda () (lambda (lo) 0)))

(define-pango pango_attr_list_unref (_pfun PangoAttrList -> _void)
  #:wrap (deallocator))
(define-pango pango_attr_list_new (_pfun -> PangoAttrList)
  #:wrap (allocator pango_attr_list_unref))
(define-pango pango_attr_list_insert (_pfun PangoAttrList PangoAttribute -> _void)
  ;; takes ownership of the attribute
  #:wrap (deallocator cadr))

(define-pango pango_attribute_destroy (_pfun PangoAttribute -> _void)
  #:wrap (deallocator))
(define-pango pango_attr_underline_new (_pfun _int -> PangoAttribute)
  #:wrap (allocator pango_attribute_destroy))
(define-pango pango_attr_fallback_new (_pfun _bool -> PangoAttribute)
  #:wrap (allocator pango_attribute_destroy))

(define-pango pango_layout_set_attributes (_pfun PangoLayout PangoAttrList -> _void))

(define-pango pango_font_map_list_families (_pfun PangoFontMap
                                                 (fams : (_ptr o _pointer))
                                                 (len : (_ptr o _int))
                                                 -> _void
                                                 -> (begin0
                                                      (for/list ([i (in-range len)])
                                                        (ptr-ref fams PangoFontFamily i))
                                                      (g_free fams))))
(define-pango pango_font_family_list_faces (_pfun PangoFontFamily
                                                  (faces : (_ptr o _pointer))
                                                  (len : (_ptr o _int))
                                                  -> _void
                                                  -> (begin0
                                                      (for/list ([i (in-range len)])
                                                        (ptr-ref faces PangoFontFace i))
                                                      (g_free faces))))
(define-pango pango_font_face_get_face_name (_pfun PangoFontFace -> _string))

(define-pango pango_font_description_free (_pfun PangoFontDescription -> _void)
  #:wrap (deallocator))
(define-pango pango_font_description_new (_pfun -> PangoFontDescription)
  #:wrap (allocator pango_font_description_free))
(define-pango pango_font_description_from_string (_pfun _string -> PangoFontDescription)
  #:wrap (allocator pango_font_description_free))
(define-pango pango_font_description_set_family (_pfun PangoFontDescription _string -> _void))
(define-pango pango_font_description_set_style (_pfun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_weight (_pfun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_size (_pfun PangoFontDescription _int -> _void))
(define-pango pango_font_description_set_absolute_size (_pfun PangoFontDescription _double* -> _void))
(define-pango pango_font_description_get_family (_pfun PangoFontDescription ->  _string))

(define _PangoWin32FontCache (_cpointer 'PangoWin32FontCache))
(define _HFONT (_cpointer 'HFONT))
(define _LOGFONT-pointer _pointer)
(define-pangowin32 pango_win32_font_map_for_display (_pfun -> PangoFontMap)
  #:make-fail make-not-available)
(define-pangowin32 pango_win32_font_logfont (_pfun PangoFont -> _LOGFONT-pointer)
  #:make-fail make-not-available
  #:wrap (allocator g_free))
(define-pangowin32 pango_win32_font_description_from_logfont (_pfun _LOGFONT-pointer -> PangoFontDescription)
  #:make-fail make-not-available
  #:wrap (allocator pango_font_description_free))
(define-pangowin32 pango_win32_font_cache_unload (_pfun _PangoWin32FontCache _HFONT -> _void)
  #:make-fail make-not-available)
(define-pangowin32 pango_win32_font_cache_load (_pfun _PangoWin32FontCache _LOGFONT-pointer -> _HFONT)
  #:make-fail make-not-available)
(define-pangowin32 pango_win32_font_cache_new (_pfun -> _PangoWin32FontCache)
  #:make-fail make-not-available)

(define-enum
  0
  PANGO_STYLE_NORMAL
  PANGO_STYLE_OBLIQUE
  PANGO_STYLE_ITALIC)

(define-enum
  0
  PANGO_UNDERLINE_NONE
  PANGO_UNDERLINE_SINGLE
  PANGO_UNDERLINE_DOUBLE
  PANGO_UNDERLINE_LOW
  PANGO_UNDERLINE_ERROR)

(define/provide PANGO_WEIGHT_LIGHT 300)
(define/provide PANGO_WEIGHT_MEDIUM 500)
(define/provide PANGO_WEIGHT_BOLD 700)

(define/provide PANGO_SCALE 1024)
