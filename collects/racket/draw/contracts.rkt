#lang racket

;; Contracts for racket/draw

(require racket/class
         "private/color.rkt"
         "private/point.rkt"
         "private/font.rkt"
         "private/font-dir.rkt"
         "private/pen.rkt"
         "private/brush.rkt"
         "private/gradient.rkt"
         "private/region.rkt"
         "private/bitmap.rkt"
         "private/dc-path.rkt"
         "private/dc-intf.rkt"
         "private/bitmap-dc.rkt"
         "private/post-script-dc.rkt"
         "private/ps-setup.rkt"
         "private/svg-dc.rkt"
         "private/gl-config.rkt"
         "private/gl-context.rkt")

(provide (all-defined-out))

;; dummy values to avoid cycles
(define-values (frame% dialog%) (values object% object%))

(define brush-style/c
  (one-of/c 'transparent 'solid 'opaque
	    'xor 'hilite 'panel
	    'bdiagonal-hatch 'crossdiag-hatch
	    'fdiagonal-hatch 'cross-hatch
	    'horizontal-hatch 'vertical-hatch))

(define pen-cap-style/c
  (one-of/c 'round 'projecting 'butt))

(define pen-join-style/c
  (one-of/c 'round 'bevel 'miter))

(define pen-style/c
  (one-of/c 'transparent 'solid 'xor 'hilite
	    'dot 'long-dash 'short-dash 'dot-dash
	    'xor-dot 'xor-long-dash 'xor-short-dash
	    'xor-dot-dash))

(define transformation-vector/c
  (vector/c (vector/c real? real? real? real? real? real?)
            real? real? real? real? real?))


(define dc<%>/c
  (class/c
    [cache-font-metrics-key (->m exact-integer?)]
    [clear (->m void?)]
    [copy (->m real? real?
               (and/c real? (not/c negative?))
               (and/c real? (not/c negative?))
               real? real?
               void?)]
    [draw-arc (->m real? real?
                   (and/c real? (not/c negative?))
                   (and/c real? (not/c negative?))
                   real? real?
                   void?)]
    [draw-bitmap (->*m ((is-a?/c bitmap%)
                        real? real?)
                       ((one-of/c 'solid 'opaque 'xor)
                        (is-a?/c color%)
                        (or/c (is-a?/c bitmap%) #f))
                       boolean?)]
    [draw-bitmap-section (->*m ((is-a?/c bitmap%)
                                real? real?
                                real? real?
                                (and/c real? (not/c negative?))
                                (and/c real? (not/c negative?)))
                               ((one-of/c 'solid 'opaque 'xor)
                                (is-a?/c color%)
                                (or/c (is-a?/c bitmap%) #f))
                               boolean?)]
    [draw-ellipse (->m real? real?
                       (and/c real? (not/c negative?))
                       (and/c real? (not/c negative?))
                       void?)]
    [draw-line (->m real? real?
                    real? real?
                    void?)]
    [draw-lines (->*m ((or/c (listof (is-a?/c point%))
                             (listof (cons/c real? real?))))
                      (real? real?)
                      void?)]
    [draw-path (->*m ((is-a?/c dc-path%))
                     (real? real? (one-of/c 'odd-even 'winding))
                     void?)]
    [draw-point (->m real? real? void?)]
    [draw-polygon (->*m ((or/c (listof (is-a?/c point%))
                               (listof (cons/c real? real?))))
                        (real? real? (one-of/c 'odd-even 'winding))
                        void?)]
    [draw-rectangle (->m real? real?
                         (and/c real? (not/c negative?))
                         (and/c real? (not/c negative?))
                         void?)]
    [draw-rounded-rectangle (->*m (real? real?
                                         (and/c real? (not/c negative?))
                                         (and/c real? (not/c negative?)))
                                  (real?)
                                  void?)]
    [draw-spline (->m real? real? real?
                      real? real? real?
                      void?)]
    [draw-text (->*m (string? real? real?)
                     (any/c exact-nonnegative-integer? real?)
                     void?)]
    [end-doc (->m void?)]
    [end-page (->m void?)]
    [erase (->m void?)]
    [flush (->m void?)]
    [get-alpha (->m real?)]
    [get-background (->m (is-a?/c color%))]
    [get-brush (->m (is-a?/c brush%))]
    [get-char-height (->m (and/c real? (not/c negative?)))]
    [get-char-width (->m (and/c real? (not/c negative?)))]
    [get-clipping-region (->m (or/c (is-a?/c region%) #f))]
    [get-device-scale (->m (values (and/c real? (not/c negative?))
                                   (and/c real? (not/c negative?))))]
    [get-font (->m (is-a?/c font%))]
    [get-gl-context (->m (or/c (is-a?/c gl-context<%>) #f))]
    [get-initial-matrix (->m (vector/c real? real? real?
                                       real? real? real?))]
    [get-origin (->m (values real? real?))]
    [get-pen (->m (is-a?/c pen%))]
    [get-rotation (->m real?)]
    [get-scale (->m (values real? real?))]
    [get-size (->m (values (and/c real? (not/c negative?))
                           (and/c real? (not/c negative?))))]
    [get-smoothing (->m (one-of/c 'unsmoothed 'smoothed 'aligned))]
    [get-text-background (->m (is-a?/c color%))]
    [get-text-extent (->*m (string?)
                           ((or/c (is-a?/c font%) #f)
                            any/c
                            exact-nonnegative-integer?)
                           (values
                             (and/c real? (not/c negative?))
                             (and/c real? (not/c negative?))
                             (and/c real? (not/c negative?))
                             (and/c real? (not/c negative?))))]
    [get-text-foreground (->m (is-a?/c color%))]
    [get-text-mode (->m (one-of/c 'solid 'transparent))]
    [get-transformation (->m (vector/c (vector/c real? real? real?
                                                 real? real? real?)
                                       real? real? real? real? real?))]
    [glyph-exists? (->m char? boolean?)]
    [ok? (->m boolean?)]
    [resume-flush (->m void?)]
    [rotate (->m real? void?)]
    [scale (->m real? real? void?)]
    [set-alpha (->m real? void?)]
    [set-background (->m (or/c (is-a?/c color%) string?) void?)]
    [set-brush (case->m (-> (is-a?/c brush%) void?)
                        (-> (or/c (is-a?/c color%) string?)
                            brush-style/c
                            void?))]
    [set-clipping-rect (->m real? real?
                            (and/c real? (not/c negative?))
                            (and/c real? (not/c negative?))
                            void?)]
    [set-clipping-region (->m (or/c (is-a?/c region%) #f) void?)]
    [set-font (->m (is-a?/c font%) void?)]
    [set-initial-matrix (->m (vector/c real? real? real?
                                       real? real? real?)
                             void?)]
    [set-origin (->m real? real? void?)]
    [set-pen (case->m (-> (is-a?/c pen%) void?)
                      (-> (or/c (is-a?/c color%) string?)
                          real?
                          pen-style/c
                          void?))]
    [set-rotation (->m real? void?)]
    [set-scale (->m real? real? void?)]
    [set-smoothing (->m (one-of/c 'unsmoothed 'smoothed 'aligned) void?)]
    [set-text-background (->m (or/c (is-a?/c color%) string?) void?)]
    [set-text-foreground (->m (or/c (is-a?/c color%) string?) void?)]
    [set-text-mode (->m (one-of/c 'solid 'transparent) void?)]
    [set-transformation (->m (vector/c (vector/c real? real? real?
                                                 real? real? real?)
                                       real? real? real? real? real?)
                             void?)]
    [start-doc (->m string? void?)]
    [start-page (->m void?)]
    [suspend-flush (->m void?)]
    [transform (->m (vector/c real? real? real? real? real? real?)
                    void?)]
    [translate (->m real? real? void?)]
    [try-color (->m (is-a?/c color%) (is-a?/c color%) void?)]))

(define color%/c
  (class/c
    (red  (->m (integer-in 0 255)))
    (blue (->m (integer-in 0 255)))
    (green (->m (integer-in 0 255)))
    (copy-from (->m (is-a?/c color%) (is-a?/c color%)))
    (ok? (->m boolean?))
    (set (->*m (byte? byte? byte?)
               ((real-in 0 1))
               void?))))

(define point%/c
  (class/c
    (get-x (->m real?))
    (get-y (->m real?))
    (set-x (->m real? void?))
    (set-y (->m real? void?))))

(define font%/c
  (class/c
    (get-face (->m (or/c string? false/c)))
    (get-family (->m (one-of/c 'default 'decorative 'roman 'script
                               'swiss 'modern 'symbol 'system)))
    (get-font-id (->m exact-integer?))
    (get-point-size (->m (integer-in 1 255)))
    (get-size-in-pixels (->m boolean?))
    (get-smoothing (->m (one-of/c 'default 'partly-smoothed
                                  'smoothed 'unsmoothed)))
    (get-style (->m (one-of/c 'normal 'italic 'slant)))
    (get-underlined (->m boolean?))
    (get-weight (->m (one-of/c 'normal 'bold 'light)))
    (screen-glyph-exists? (->*m (char?) (any/c) boolean?))))

(define pen%/c
  (class/c
    (get-cap (->m pen-cap-style/c))
    (get-color (->m (is-a?/c color%)))
    (get-join (->m pen-join-style/c))
    (get-stipple (->m (or/c (is-a?/c bitmap%) false/c)))
    (get-style (->m pen-style/c))
    (get-width (->m (real-in 0 255)))
    (set-cap (->m pen-cap-style/c void?))
    (set-color (case->m
                 (-> (or/c (is-a?/c color%) string?) void?)
                 (-> (integer-in 0 255)
                     (integer-in 0 255)
                     (integer-in 0 255)
                     void?)))
    (set-join (->m pen-join-style/c void?))
    (set-stipple (->m (or/c (is-a?/c bitmap%) false/c) void?))
    (set-style (->m pen-style/c void?))
    (set-width (->m (real-in 0 255) void?))))

(define pen-list%/c
  (class/c
    (find-or-create-pen
      (->*m ((or/c (is-a?/c color%) string?)
             real?
             pen-style/c)
            (pen-cap-style/c
              pen-join-style/c)
            (or/c (is-a?/c pen%) #f)))))

(define brush%/c
  (class/c
    (get-color (->m (is-a?/c color%)))
    (get-stipple (->m (or/c (is-a?/c bitmap%) false/c)))
    (get-style (->m brush-style/c))
    (set-color (case->m
                 (-> (or/c (is-a?/c color%) string?) void?)
                 (-> (integer-in 0 255)
                     (integer-in 0 255)
                     (integer-in 0 255)
                     void?)))
    (set-stipple (->*m ((or/c (is-a?/c bitmap%) #f))
                       ((or/c transformation-vector/c #f))
                       void?))
    (set-style (->m brush-style/c void?))))

(define brush-list%/c
  (class/c
    (find-or-create-brush
      (->m (or/c (is-a?/c color%) string?)
           brush-style/c
           (or/c (is-a?/c brush%) #f)))))

(define linear-gradient%/c
  (class/c
    (init
      [x0 real?]
      [y0 real?]
      [x1 real?]
      [y1 real?]
      [stops (listof (list/c real? (is-a?/c color%)))])
    [get-line (->m (values real? real? real? real?))]
    [get-stops (->m (listof (list/c real? (is-a?/c color%))))]))

(define radial-gradient%/c
  (class/c
    (init
      [x0 real?]
      [y0 real?]
      [r0 real?]
      [x1 real?]
      [y1 real?]
      [r1 real?]
      [stops (listof (list/c real? (is-a?/c color%)))])
    [get-circles (->m (values real? real? real? real? real? real?))]
    [get-stops (->m (listof (list/c real? (is-a?/c color%))))]))

(define bitmap-dc%/c
  (and/c dc<%>/c
         (class/c
           (init [bitmap (or/c (is-a?/c bitmap%) #f)])
           [draw-bitmap-section-smooth
             (->*m ((is-a?/c bitmap%)
                    real? real?
                    (and/c real? (not/c negative?))
                    (and/c real? (not/c negative?))
                    real? real?
                    (and/c real? (not/c negative?))
                    (and/c real? (not/c negative?)))
                   ((one-of/c 'solid 'opaque 'xor)
                    (or/c (is-a?/c color%) #f)
                    (or/c (is-a?/c bitmap%) #f))
                  boolean?)]
           [get-argb-pixels
             (->*m (exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    (and/c bytes? (not/c immutable?)))
                   (any/c any/c)
                   void?)]
           [get-bitmap (->m (or/c (is-a?/c bitmap%) #f))]
           [get-pixel (->m real? real? (is-a?/c color%) boolean?)]
           [set-argb-pixels
             (->*m (exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    exact-nonnegative-integer?
                    (and/c bytes? (not/c immutable?)))
                   (any/c any/c)
                   void?)]
           [set-bitmap (->m (or/c (is-a?/c bitmap%) #f) void?)]
           [set-pixel (->m real? real? (is-a?/c color%) void?)])))

(define post-script-dc%/c
  (and/c dc<%>/c
         (class/c
           (init [interactive any/c]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
                 [use-paper-bbox any/c]
                 [as-eps any/c]
                 [width (or/c (and/c real? (not/c negative?)) #f)]
                 [height (or/c (and/c real? (not/c negative?)) #f)]
                 [output (or/c path-string? output-port? #f)]))))

(define pdf-dc%/c
  (and/c dc<%>/c
         (class/c
           (init [interactive any/c]
                 [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
                 [use-paper-bbox any/c]
                 [as-eps any/c]
                 [width (or/c (and/c real? (not/c negative?)) #f)]
                 [height (or/c (and/c real? (not/c negative?)) #f)]
                 [output (or/c path-string? output-port? #f)]))))

(define svg-dc%/c
  (and/c dc<%>/c
         (class/c
           (init [width (or/c (and/c real? (not/c negative?)) #f)]
                 [height (or/c (and/c real? (not/c negative?)) #f)]
                 [output (or/c path-string? output-port? #f)]
                 [exists (or/c 'error 'append 'update 'can-update
                               'replace 'truncate
                               'must-truncate 'truncate/replace)]))))

(define record-dc%/c
  (and/c dc<%>/c
         (class/c
           (init [width (>=/c 0)]
                 [height (>=/c 0)])
           [get-recorded-datum (->m any/c)]
           [get-recorded-procedure (->m ((is-a?/c dc<%>) . -> . void?))])))

(define region%/c
  (class/c
    (init [dc (or/c (is-a?/c dc<%>) #f)])
    (get-bounding-box (->m (values real? real? real? real?)))
    (get-dc (->m (is-a?/c dc<%>)))
    (in-region? (->m real? real? boolean?))
    (intersect (->m (is-a?/c region%) void?))
    (is-empty? (->m boolean?))
    (set-arc (->m real?
                  real?
                  (and/c real? (not/c negative?))
                  (and/c real? (not/c negative?))
                  real?
                  real?
                  void?))
    (set-ellipse (->m real?
                      real?
                      (and/c real? (not/c negative?))
                      (and/c real? (not/c negative?))
                      void?))
    (set-path (->*m ((is-a?/c dc-path%))
                    (real?
                     real?
                     (one-of/c 'odd-even 'winding))
                    void?))
    (set-polygon (->*m ((or/c (listof (is-a?/c point%))
                              (listof (cons/c real? real?))))
                       (real?
                        real?
                        (one-of/c 'odd-even 'winding))
                       void?))
    (set-rectangle (->m real?
                        real?
                        (and/c real? (not/c negative?))
                        (and/c real? (not/c negative?))
                        void?))
    (set-rounded-rectangle (->*m (real?
                                  real?
                                  (and/c real? (not/c negative?))
                                  (and/c real? (not/c negative?)))
                                 (real?)
                                 void?))
    (subtract (->m (is-a?/c region%) void?))
    (union (->m (is-a?/c region%) void?))
    (xor (->m (is-a?/c region%) void?))))

(define dc-path%/c
  (class/c
    (append (->m (is-a?/c dc-path%) void?))
    (arc (->*m (real?
                real?
                real?
                real?
                real?
                real?)
               (any/c)
               void?))
    (close (->m void?))
    (curve-to (->m real? real? real? real? real? real? void?))
    (ellipse (->m real?
                  real?
                  (and/c real? (not/c negative?))
                  (and/c real? (not/c negative?))
                  void?))
    (get-bounding-box (->m (values real? real? real? real?)))
    (line-to (->m real? real? void?))
    (lines (->*m ((or/c (listof (is-a?/c point%))
                        (listof (cons/c real? real?))))
                 (real? real?)
                 void?))
    (move-to (->m real? real? void?))
    (open? (->m boolean?))
    (rectangle (->m real?
                    real?
                    (and/c real? (not/c negative?))
                    (and/c real? (not/c negative?))
                    void?))
    (reset (->m void?))
    (reverse (->m void?))
    (rotate (->m real? void?))
    (rounded-rectangle (->*m (real?
                              real?
                              (and/c real? (not/c negative?))
                              (and/c real? (not/c negative?)))
                             (real?)
                             void?))
    (scale (->m real? real? void?))
    (text-outline (->*m ((is-a?/c font%)
                         string?
                         real? real?)
                        (any/c)
                        void?))
    (transform (->m (vector/c real? real? real? real? real? real?)
                    void?))
    (translate (->m real? real? void?))))

(define gl-config%/c
  (class/c
    (get-accum-size (->m (integer-in 0 256)))
    (get-depth-size (->m (integer-in 0 256)))
    (get-double-buffered (->m boolean?))
    (get-multisample-size (->m (integer-in 0 256)))
    (get-stencil-size (->m (integer-in 0 256)))
    (get-stereo (->m boolean?))
    (set-accum-size (->m (integer-in 0 256) void?))
    (set-depth-size (->m (integer-in 0 256) void?))
    (set-double-buffered (->m any/c void?))
    (set-multisample-size (->m (integer-in 0 256) void?))
    (set-stencil-size (->m (integer-in 0 256) void?))
    (set-stereo (->m any/c void?))))

(define bitmap%/c
  (class/c
    (get-argb-pixels (->*m
                       (exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        (and/c bytes? (not/c immutable?)))
                       (any/c any/c)
                       void?))
    (get-depth (->m exact-nonnegative-integer?))
    (get-height (->m exact-nonnegative-integer?))
    (get-loaded-mask (->m (or/c (is-a?/c bitmap%) false/c)))
    (get-width (->m exact-nonnegative-integer?))
    (has-alpha-channel? (->m boolean?))
    (is-color? (->m boolean?))
    (load-file (->*m ((or/c path-string? input-port?))
                     ((one-of/c 'unknown 'unknown/mask 'unknown/alpha
                                'gif 'gif/mask 'gif/alpha
                                'jpeg 'jpeg/alpha
                                'png 'png/mask 'png/alpha
                                'xbm 'xbm/alpha 'xpm 'xpm/alpha
                                'bmp 'bmp/alpha)
                      (or/c (is-a?/c color%) false/c)
                      any/c)
                     boolean?))
    (ok? (->m boolean?))
    (save-file (->*m ((or/c path-string? output-port?)
                      (one-of/c 'png 'jpeg 'xbm 'xpm 'bmp))
                     ((integer-in 0 100))
                     boolean?))
    (set-argb-pixels (->*m
                       (exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        bytes?)
                       (any/c any/c)
                       void?))
    (set-loaded-mask (->m (is-a?/c bitmap%) void?))))
