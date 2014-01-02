#lang racket/base

;; Contracts for racket/draw

(require racket/contract/base
         racket/class
         "color.rkt"
         "point.rkt"
         "font.rkt"
         "font-dir.rkt"
         "font-syms.rkt"
         "pen.rkt"
         "brush.rkt"
         "gradient.rkt"
         "region.rkt"
         "bitmap.rkt"
         "dc-path.rkt"
         "dc-intf.rkt"
         "bitmap-dc.rkt"
         "post-script-dc.rkt"
         "ps-setup.rkt"
         "svg-dc.rkt"
         "gl-config.rkt"
         "gl-context.rkt")

(provide brush-style/c
         pen-cap-style/c
         pen-join-style/c
         pen-style/c
         font-family/c
         font-weight/c
         font-style/c
         transformation-vector/c
         make-color/c
         make-brush/c
         make-pen/c
         color%/c
         point%/c
         font%/c
         pen%/c
         pen-list%/c
         brush%/c
         brush-list%/c
         linear-gradient%/c
         radial-gradient%/c
         bitmap-dc%/c
         post-script-dc%/c
         pdf-dc%/c
         svg-dc%/c
         record-dc%/c
         region%/c
         dc-path%/c
         gl-config%/c
         bitmap%/c)

;; dummy values to avoid cycles
(define-values (frame% dialog%) (values object% object%))

(define brush-style/c
  (or/c 'transparent 'solid 'opaque
        'xor 'hilite 'panel
        'bdiagonal-hatch 'crossdiag-hatch
        'fdiagonal-hatch 'cross-hatch
        'horizontal-hatch 'vertical-hatch))

(define pen-cap-style/c
  (or/c 'round 'projecting 'butt))

(define pen-join-style/c
  (or/c 'round 'bevel 'miter))

(define pen-style/c
  (or/c 'transparent 'solid 'xor 'hilite
        'dot 'long-dash 'short-dash 'dot-dash
        'xor-dot 'xor-long-dash 'xor-short-dash
        'xor-dot-dash))

(define transformation-vector/c
  (vector/c (vector/c real? real? real? real? real? real?)
            real? real? real? real? real?))

(define make-color/c
  (->* (byte? byte? byte?)
       ((real-in 0 1))
       (is-a?/c color%)))

(define make-brush/c
  (->* ()
       (#:color (or/c string? (is-a?/c color%))
        #:style brush-style/c
        #:stipple (or/c #f (is-a?/c bitmap%))
        #:gradient (or/c #f
                         (is-a?/c linear-gradient%)
                         (is-a?/c radial-gradient%))
        #:transformation (or/c #f transformation-vector/c)
        #:immutable? any/c)
       (is-a?/c brush%)))

(define make-pen/c
  (->* ()
       (#:color (or/c string? (is-a?/c color%))
        #:width (real-in 0 255)
        #:style pen-style/c
        #:cap pen-cap-style/c
        #:join pen-join-style/c
        #:stipple (or/c #f (is-a?/c bitmap%))
        #:immutable? any/c)
       (is-a?/c pen%)))

(define color%/c
  (class/c
    (alpha (->m (real-in 0 1)))
    (red  (->m byte?))
    (blue (->m byte?))
    (green (->m byte?))
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
    (get-face (->m (or/c string? #f)))
    (get-family (->m font-family/c))
    (get-font-id (->m exact-integer?))
    (get-hinting (->m font-hinting/c))
    (get-point-size (->m (integer-in 1 255)))
    (get-size-in-pixels (->m boolean?))
    (get-smoothing (->m font-smoothing/c))
    (get-style (->m font-style/c))
    (get-underlined (->m boolean?))
    (get-weight (->m font-weight/c))
    (screen-glyph-exists? (->*m (char?) (any/c) boolean?))))

(define pen%/c
  (class/c
    (get-cap (->m pen-cap-style/c))
    (get-color (->m (is-a?/c color%)))
    (get-join (->m pen-join-style/c))
    (get-stipple (->m (or/c (is-a?/c bitmap%) #f)))
    (get-style (->m pen-style/c))
    (get-width (->m (real-in 0 255)))
    (set-cap (->m pen-cap-style/c void?))
    (set-color (case->m
                 (-> (or/c (is-a?/c color%) string?) void?)
                 (-> byte? byte? byte? void?)))
    (set-join (->m pen-join-style/c void?))
    (set-stipple (->m (or/c (is-a?/c bitmap%) #f) void?))
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
    (get-stipple (->m (or/c (is-a?/c bitmap%) #f)))
    (get-style (->m brush-style/c))
    (set-color (case->m
                 (-> (or/c (is-a?/c color%) string?) void?)
                 (-> byte? byte? byte? void?)))
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
            ((or/c 'solid 'opaque 'xor)
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
             bytes?)
            (any/c any/c)
            void?)]
    [set-bitmap (->m (or/c (is-a?/c bitmap%) #f) void?)]
    [set-pixel (->m real? real? (is-a?/c color%) void?)]))

(define post-script-dc%/c
  (class/c
    (init [interactive any/c]
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
          [use-paper-bbox any/c]
          [as-eps any/c]
          [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)])))

(define pdf-dc%/c
  (class/c
    (init [interactive any/c]
          [parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f)]
          [use-paper-bbox any/c]
          [as-eps any/c]
          [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)])))

(define svg-dc%/c
  (class/c
    (init [width (or/c (and/c real? (not/c negative?)) #f)]
          [height (or/c (and/c real? (not/c negative?)) #f)]
          [output (or/c path-string? output-port? #f)]
          [exists (or/c 'error 'append 'update 'can-update
                        'replace 'truncate
                        'must-truncate 'truncate/replace)])))

(define record-dc%/c
  (class/c
    (init [width (>=/c 0)]
          [height (>=/c 0)])
    [get-recorded-datum (->m any/c)]
    [get-recorded-procedure (->m ((is-a?/c dc<%>) . -> . void?))]))

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
                     (or/c 'odd-even 'winding))
                    void?))
    (set-polygon (->*m ((or/c (listof (is-a?/c point%))
                              (listof (cons/c real? real?))))
                       (real?
                        real?
                        (or/c 'odd-even 'winding))
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
    (set-stereo (->m any/c void?))
    (set-share-context (->m (or/c (is-a?/c gl-context%) #f) void?))))

(define bitmap%/c
  (class/c
    (get-argb-pixels (->*m
                       (exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        (and/c bytes? (not/c immutable?)))
                       (any/c any/c #:unscaled? any/c)
                       void?))
    (get-depth (->m exact-nonnegative-integer?))
    (get-height (->m exact-nonnegative-integer?))
    (get-loaded-mask (->m (or/c (is-a?/c bitmap%) #f)))
    (get-width (->m exact-nonnegative-integer?))
    (has-alpha-channel? (->m boolean?))
    (is-color? (->m boolean?))
    (load-file (->*m ((or/c path-string? input-port?))
                     ((or/c 'unknown 'unknown/mask 'unknown/alpha
                            'gif 'gif/mask 'gif/alpha
                            'jpeg 'jpeg/alpha
                            'png 'png/mask 'png/alpha
                            'xbm 'xbm/alpha 'xpm 'xpm/alpha
                            'bmp 'bmp/alpha)
                      (or/c (is-a?/c color%) #f)
                      any/c)
                     boolean?))
    (ok? (->m boolean?))
    (save-file (->*m ((or/c path-string? output-port?)
                      (or/c 'png 'jpeg 'xbm 'xpm 'bmp))
                     ((integer-in 0 100)
                      #:unscaled? any/c)
                     boolean?))
    (set-argb-pixels (->*m
                       (exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        exact-nonnegative-integer?
                        bytes?)
                       (any/c any/c #:unscaled? any/c)
                       void?))
    (set-loaded-mask (->m (is-a?/c bitmap%) void?))))
