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
   24 32
   (λ (dc)
     (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
     (send dc set-brush color 'solid)
     (send dc draw-polygon (list (cons 0 0) (cons 4 0)
                                 (cons 23 13) (cons 23 18)
                                 (cons 4 31) (cons 0 31))))
   (/ height 32)))

(defproc (play-flomap [color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (make-cached-flomap
   [height color material]
   (define fm (flat-play-flomap color height))
   (flomap-render-icon fm material)))

(defproc (fast-forward-flomap [color (or/c string? (is-a?/c color%))]
                              [height (and/c rational? (>=/c 0)) (default-icon-height)]
                              [material deep-flomap-material-value? (default-icon-material)]
                              ) flomap?
  (make-cached-flomap
   [height color material]
   (define fm (draw-rendered-icon-flomap
               20 32 (λ (dc)
                       (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
                       (send dc set-brush color 'solid)
                       (send dc draw-polygon (list (cons 0 0) (cons 4 0)
                                                   (cons 19 13) (cons 19 18)
                                                   (cons 4 31) (cons 0 31))))
               (/ height 32)
               material))
   (flomap-hc-append fm fm)))

(defproc (stop-flomap [color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
            (send dc set-brush color 'solid)
            (send dc draw-polygon (list '(0 . 0) '(31 . 0) '(31 . 31) '(0 . 31))))
    (/ height 32)
    material)))

(defproc (record-flomap [color (or/c string? (is-a?/c color%))]
                        [height (and/c rational? (>=/c 0)) (default-icon-height)]
                        [material deep-flomap-material-value? (default-icon-material)]
                        ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
            (send dc set-brush color 'solid)
            (draw-ellipse/smoothed dc 0 0 32 32))
    (/ height 32)
    material)))

(defproc (bar-flomap [color (or/c string? (is-a?/c color%))]
                     [height (and/c rational? (>=/c 0)) (default-icon-height)]
                     [material deep-flomap-material-value? (default-icon-material)]
                     ) flomap?
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    8 32 (λ (dc)
           (set-icon-pen dc (icon-color->outline-color color) 1 'solid)
           (send dc set-brush color 'solid)
           (send dc draw-polygon (list '(0 . 0) '(7 . 0) '(7 . 31) '(0 . 31))))
    (/ height 32)
    material)))

(defproc (back-flomap [color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (flomap-flip-horizontal (play-flomap color height material)))

(defproc (rewind-flomap [color (or/c string? (is-a?/c color%))]
                        [height (and/c rational? (>=/c 0)) (default-icon-height)]
                        [material deep-flomap-material-value? (default-icon-material)]
                        ) flomap?
  (flomap-flip-horizontal (fast-forward-flomap color height material)))

(defproc (pause-flomap [color (or/c string? (is-a?/c color%))]
                       [height (and/c rational? (>=/c 0)) (default-icon-height)]
                       [material deep-flomap-material-value? (default-icon-material)]
                       ) flomap?
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/8 height)))) 0)
   (bar-flomap color height material)))

(defproc (step-flomap [color (or/c string? (is-a?/c color%))]
                      [height (and/c rational? (>=/c 0)) (default-icon-height)]
                      [material deep-flomap-material-value? (default-icon-material)]
                      ) flomap?
  (flomap-hc-append
   (play-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap color height material)))

(defproc (step-back-flomap [color (or/c string? (is-a?/c color%))]
                           [height (and/c rational? (>=/c 0)) (default-icon-height)]
                           [material deep-flomap-material-value? (default-icon-material)]
                           ) flomap?
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (back-flomap color height material)))

(defproc (continue-forward-flomap [color (or/c string? (is-a?/c color%))]
                                  [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                  [material deep-flomap-material-value? (default-icon-material)]
                                  ) flomap?
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (play-flomap color height material)))

(defproc (continue-backward-flomap [color (or/c string? (is-a?/c color%))]
                                   [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                   [material deep-flomap-material-value? (default-icon-material)]
                                   ) flomap?
  (flomap-hc-append
   (back-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap color height material)))

(defproc (search-forward-flomap [color (or/c string? (is-a?/c color%))]
                                [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                [material deep-flomap-material-value? (default-icon-material)]
                                ) flomap?
  (flomap-hc-append
   (fast-forward-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap color height material)))

(defproc (search-backward-flomap [color (or/c string? (is-a?/c color%))]
                                 [height (and/c rational? (>=/c 0)) (default-icon-height)]
                                 [material deep-flomap-material-value? (default-icon-material)]
                                 ) flomap?
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (rewind-flomap color height material)))

(define-icon-wrappers
  ([color (or/c string? (is-a?/c color%))]
   [height (and/c rational? (>=/c 0)) (default-icon-height)]
   [material deep-flomap-material-value? (default-icon-material)])
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
