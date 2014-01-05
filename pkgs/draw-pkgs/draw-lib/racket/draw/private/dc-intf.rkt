#lang racket/base

;; drawing context interface

(require "bitmap.rkt"
         "brush.rkt"
         "color.rkt"
         "dc-path.rkt"
         "font.rkt"
         "gl-context.rkt"
         "pen.rkt"
         "point.rkt"
         racket/class
         racket/contract)

(provide dc<%>)

;; dummy value to avoid cycles via "region.rkt"
(define region% object%)

;; repeated here from "contract.rkt" to avoid cycles
(define pen-style/c
  (or/c 'transparent 'solid 'xor 'hilite
        'dot 'long-dash 'short-dash 'dot-dash
        'xor-dot 'xor-long-dash 'xor-short-dash
        'xor-dot-dash))

(define brush-style/c
  (or/c 'transparent 'solid 'opaque
        'xor 'hilite 'panel
        'bdiagonal-hatch 'crossdiag-hatch
        'fdiagonal-hatch 'cross-hatch
        'horizontal-hatch 'vertical-hatch))

(define dc<%>
  (interface ()
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
                       ((or/c 'solid 'opaque 'xor)
                        (is-a?/c color%)
                        (or/c (is-a?/c bitmap%) #f))
                       boolean?)]
    [draw-bitmap-section (->*m ((is-a?/c bitmap%)
                                real? real?
                                real? real?
                                (and/c real? (not/c negative?))
                                (and/c real? (not/c negative?)))
                               ((or/c 'solid 'opaque 'xor)
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
                     (real? real? (or/c 'odd-even 'winding))
                     void?)]
    [draw-point (->m real? real? void?)]
    [draw-polygon (->*m ((or/c (listof (is-a?/c point%))
                               (listof (cons/c real? real?))))
                        (real? real? (or/c 'odd-even 'winding))
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
    [get-smoothing (->m (or/c 'unsmoothed 'smoothed 'aligned))]
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
    [get-text-mode (->m (or/c 'solid 'transparent))]
    [get-transformation (->m (vector/c (vector/c real? real? real?
                                                 real? real? real?)
                                       real? real? real? real? real?))]
    [glyph-exists? (->m char? boolean?)]
    [ok? (->m boolean?)]
    [resume-flush (->m void?)]
    [rotate (->m real? void?)]
    [scale (->m real? real? void?)]
    [set-alignment-scale (->m (>/c 0.0) void?)]
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
    [set-smoothing (->m (or/c 'unsmoothed 'smoothed 'aligned) void?)]
    [set-text-background (->m (or/c (is-a?/c color%) string?) void?)]
    [set-text-foreground (->m (or/c (is-a?/c color%) string?) void?)]
    [set-text-mode (->m (or/c 'solid 'transparent) void?)]
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
