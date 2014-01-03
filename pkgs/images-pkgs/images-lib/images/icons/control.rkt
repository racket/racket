#lang racket/base

(require racket/class racket/draw
         racket/contract unstable/latent-contract unstable/latent-contract/defthing
         "../private/flomap.rkt"
         "../private/deep-flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (activate-contract-out
          play-icon play-flomap
          back-icon back-flomap
          fast-forward-icon fast-forward-flomap
          rewind-icon rewind-flomap
          bar-icon bar-flomap
          stop-icon stop-flomap
          record-icon record-flomap
          pause-icon pause-flomap
          step-icon step-flomap
          step-back-icon step-back-flomap
          continue-forward-icon continue-forward-flomap
          continue-backward-icon continue-backward-flomap
          search-forward-icon search-forward-flomap
          search-backward-icon search-backward-flomap)
         (only-doc-out (all-defined-out)))

(define (flat-play-flomap color height)
  (draw-icon-flomap
   (λ (dc)
     (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
     (send dc set-brush color 'solid)
     (send dc draw-polygon (list (cons 0 0) (cons 4 0)
                                 (cons 23 13) (cons 23 18)
                                 (cons 4 31) (cons 0 31))))
   24 32 (/ height 32)))

(defproc (play-flomap [#:color color (or/c string? (is-a?/c color%))]
                      [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [#:material material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (make-cached-flomap
   [height color material]
   (define fm (flat-play-flomap color height))
   (flomap-render-icon fm material)))

(defproc (fast-forward-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (make-cached-flomap
   [height color material]
   (define fm
     (draw-rendered-icon-flomap
      (λ (dc)
        (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
        (send dc set-brush color 'solid)
        (send dc draw-polygon (list (cons 0 0) (cons 4 0)
                                    (cons 19 13) (cons 19 18)
                                    (cons 4 31) (cons 0 31))))
      20 32 (/ height 32) material))
   (flomap-hc-append fm fm)))

(defproc (stop-flomap [#:color color (or/c string? (is-a?/c color%))]
                      [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [#:material material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    (λ (dc)
      (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
      (send dc set-brush color 'solid)
      (send dc draw-polygon (list '(0 . 0) '(31 . 0) '(31 . 31) '(0 . 31))))
    32 32 (/ height 32) material)))

(defproc (record-flomap [#:color color (or/c string? (is-a?/c color%))]
                        [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                        [#:material material deep-flomap-material-value? (default-icon-material)]
                        ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    (λ (dc)
      (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
      (send dc set-brush color 'solid)
      (send dc draw-ellipse 0 0 31 31))
    32 32 (/ height 32) material)))

(defproc (bar-flomap [#:color color (or/c string? (is-a?/c color%))]
                     [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                     [#:material material deep-flomap-material-value? (default-icon-material)]
                     ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    (λ (dc)
      (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
      (send dc set-brush color 'solid)
      (send dc draw-polygon (list '(0 . 0) '(7 . 0) '(7 . 31) '(0 . 31))))
    8 32 (/ height 32) material)))

(defproc (back-flomap [#:color color (or/c string? (is-a?/c color%))]
                      [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [#:material material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (flomap-flip-horizontal (play-flomap #:color color #:height height #:material material)))

(defproc (rewind-flomap [#:color color (or/c string? (is-a?/c color%))]
                        [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                        [#:material material deep-flomap-material-value? (default-icon-material)]
                        ) flomap?
  (flomap-flip-horizontal (fast-forward-flomap #:color color #:height height #:material material)))

(defproc (pause-flomap [#:color color (or/c string? (is-a?/c color%))]
                       [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                       [#:material material deep-flomap-material-value? (default-icon-material)]
                       ) flomap?
  (define bar (bar-flomap #:color color #:height height #:material material))
  (flomap-hc-append bar (make-flomap 4 (max 1 (inexact->exact (round (* 1/8 height)))) 0) bar))

(defproc (step-flomap [#:color color (or/c string? (is-a?/c color%))]
                      [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [#:material material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (flomap-hc-append
   (play-flomap #:color color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap #:color color #:height height #:material material)))

(defproc (step-back-flomap [#:color color (or/c string? (is-a?/c color%))]
                           [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
                           [#:material material deep-flomap-material-value? (default-icon-material)]
                           ) flomap?
  (flomap-hc-append
   (bar-flomap #:color color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (back-flomap #:color color #:height height #:material material)))

(defproc (continue-forward-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-hc-append
   (bar-flomap #:color color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (play-flomap #:color color #:height height #:material material)))

(defproc (continue-backward-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-hc-append
   (back-flomap #:color color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap #:color color #:height height #:material material)))

(defproc (search-forward-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-hc-append
   (fast-forward-flomap #:color color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap #:color color #:height height #:material material)))

(defproc (search-backward-flomap
          [#:color color (or/c string? (is-a?/c color%))]
          [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
          [#:material material deep-flomap-material-value? (default-icon-material)]
          ) flomap?
  (flomap-hc-append
   (bar-flomap #:color color #:height height #:material material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (rewind-flomap #:color color #:height height #:material material)))

(define-icon-wrappers
  ([#:color color (or/c string? (is-a?/c color%))]
   [#:height height (and/c rational? (>=/c 0)) (default-icon-height)]
   [#:material material deep-flomap-material-value? (default-icon-material)])
  (height)
  [play-icon play-flomap]
  [back-icon back-flomap]
  [fast-forward-icon fast-forward-flomap]
  [rewind-icon rewind-flomap]
  [bar-icon bar-flomap]
  [stop-icon stop-flomap]
  [record-icon record-flomap]
  [pause-icon pause-flomap]
  [step-icon step-flomap]
  [step-back-icon step-back-flomap]
  [continue-forward-icon continue-forward-flomap]
  [continue-backward-icon continue-backward-flomap]
  [search-forward-icon search-forward-flomap]
  [search-backward-icon search-backward-flomap])
