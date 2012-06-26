#lang racket/base

(require racket/class racket/draw
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (activate-contract-out
          flat-right-arrow-flomap
          flat-right-over-arrow-flomap
          right-arrow-icon right-arrow-flomap
          left-arrow-icon left-arrow-flomap
          up-arrow-icon up-arrow-flomap
          down-arrow-icon down-arrow-flomap
          right-over-arrow-icon right-over-arrow-flomap
          left-over-arrow-icon left-over-arrow-flomap
          right-under-arrow-icon right-under-arrow-flomap
          left-under-arrow-icon left-under-arrow-flomap)
         (only-doc-out (all-defined-out)))

(defproc (flat-right-arrow-flomap [color (or/c string? (is-a?/c color%))]
                                  [height (and/c rational? (>=/c 0))]
                                  ) flomap?
  (let ([color  (->color% color)])
    (draw-icon-flomap
     (λ (dc)
       (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
       (send dc set-brush color 'solid)
       (send dc draw-polygon (list '(0 . 9) '(15 . 9) '(14 . 0)
                                   '(31 . 15.5)
                                   '(14 . 31) '(15 . 22) '(0 . 22))))
     32 32 (/ height 32))))

(defproc (flat-right-over-arrow-flomap [color (or/c string? (is-a?/c color%))]
                                       [height (and/c rational? (>=/c 0))]
                                       ) flomap?
  (draw-icon-flomap
   (λ (dc)
     (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
     (send dc set-brush color 'solid)
     (draw-path-commands dc '((m 0 15)
                              (c 9 -14 19.5 -8 24 -2)
                              (l 5 -7 2 20 -20 -2 7 -5)
                              (c -2.5 -4 -8 -8.5 -14 0)
                              (l -4 -4))
                         0 0))
   32 32 (/ height 32)))

(defproc (right-arrow-flomap [#:color color (or/c string? (is-a?/c color%))]
                             [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                             [#:material material deep-flomap-material-value? (default-icon-material)]
                             ) flomap?
  (make-cached-flomap
   [height color material]
   (flomap-render-thin-icon (flat-right-arrow-flomap color height) material)))

(defproc (up-arrow-flomap [#:color color (or/c string? (is-a?/c color%))]
                          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                          [#:material material deep-flomap-material-value? (default-icon-material)]
                          ) flomap?
  (make-cached-flomap
   [height color material]
   (flomap-render-icon (flomap-cw-rotate (flat-right-arrow-flomap color height)) material)))

(defproc (down-arrow-flomap [#:color color (or/c string? (is-a?/c color%))]
                            [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                            [#:material material deep-flomap-material-value? (default-icon-material)]
                            ) flomap?
  (make-cached-flomap
   [height color material]
   (flomap-render-icon (flomap-ccw-rotate (flat-right-arrow-flomap color height)) material)))

(defproc (right-over-arrow-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height color material]
   (flomap-render-thin-icon (flat-right-over-arrow-flomap color height) material)))

(defproc (right-under-arrow-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height color material]
   (flomap-render-thin-icon
    (flomap-flip-vertical (flat-right-over-arrow-flomap color height)) material)))

(defproc (left-arrow-flomap [#:color color (or/c string? (is-a?/c color%))]
                            [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                            [#:material material deep-flomap-material-value? (default-icon-material)]
                            ) flomap?
  (flomap-flip-horizontal
   (right-arrow-flomap #:color color #:height height #:material material)))


(defproc (left-over-arrow-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-flip-horizontal
   (right-over-arrow-flomap #:color color #:height height #:material material)))

(defproc (left-under-arrow-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-flip-horizontal
   (right-under-arrow-flomap #:color color #:height height #:material material)))

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%))]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  [left-arrow-icon left-arrow-flomap]
  [right-arrow-icon right-arrow-flomap]
  [up-arrow-icon up-arrow-flomap]
  [down-arrow-icon down-arrow-flomap]
  [right-over-arrow-icon right-over-arrow-flomap]
  [left-over-arrow-icon left-over-arrow-flomap]
  [right-under-arrow-icon right-under-arrow-flomap]
  [left-under-arrow-icon left-under-arrow-flomap])
