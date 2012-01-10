#lang racket/base

(require racket/class
         "../private/flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (all-defined-out))

(define play-points
  (list '(0 . 0) '(4 . 0)
        '(23 . 13) '(23 . 18)
        '(4 . 31) '(0 . 31)))

(define (play-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    24 32 (λ (dc)
            (send dc set-brush color 'solid)
            (send dc draw-polygon play-points))
    (/ height 32)
    material)))

(define (fast-forward-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (send dc set-brush color 'solid)
            (send dc draw-polygon (list '(0 . 0) '(4 . 0)
                                        '(17 . 13) '(17 . 18)
                                        '(4 . 31) '(0 . 31)))
            (send dc translate 2 0)
            (send dc draw-polygon (list
                                   ;; right side
                                   '(14 . 2)
                                   '(27 . 13) '(27 . 18)
                                   '(14 . 29)
                                   ;; left side
                                   '(8 . 29)
                                   '(18 . 19) '(18 . 12)
                                   '(8 . 2))))
    (/ height 32)
    material)))

(define (stop-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (send dc set-brush color 'solid)
            (send dc draw-polygon (list '(0 . 0) '(31 . 0) '(31 . 31) '(0 . 31))))
    (/ height 32)
    material)))

(define (record-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    32 32 (λ (dc)
            (send dc set-brush color 'solid)
            (draw-ellipse/smoothed dc 0 0 32 32))
    (/ height 32)
    material)))

(define (bar-flomap color height material)
  (make-cached-flomap
   [height color material]
   (draw-rendered-icon-flomap
    8 32 (λ (dc)
           (send dc set-brush color 'solid)
           (send dc draw-polygon (list '(0 . 0) '(7 . 0) '(7 . 31) '(0 . 31))))
    (/ height 32)
    material)))

(define back-flomap (compose flomap-flip-horizontal play-flomap))
(define reverse-flomap (compose flomap-flip-horizontal fast-forward-flomap))

(define (pause-flomap color [height (default-icon-height)] [material (default-icon-material)])
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/8 height)))) 0)
   (bar-flomap color height material)))

(define (step-flomap color [height  (default-icon-height)]
                          [material  (default-icon-material)])
  (flomap-hc-append
   (play-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap color height material)))

(define (step-back-flomap color [height  (default-icon-height)]
                               [material  (default-icon-material)])
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (back-flomap color height material)))

(define (continue-flomap color [height  (default-icon-height)]
                              [material  (default-icon-material)])
  (flomap-hc-append
   (bar-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (play-flomap color height material)))

(define (continue-back-flomap color [height  (default-icon-height)]
                                   [material  (default-icon-material)])
  (flomap-hc-append
   (back-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-flomap color height material)))

(define play-icon (compose flomap->bitmap play-flomap))
(define back-icon (compose flomap->bitmap back-flomap))
(define fast-forward-icon (compose flomap->bitmap fast-forward-flomap))
(define reverse-icon (compose flomap->bitmap reverse-flomap))
(define bar-icon (compose flomap->bitmap bar-flomap))
(define stop-icon (compose flomap->bitmap stop-flomap))
(define record-icon (compose flomap->bitmap record-flomap))
(define pause-icon (compose flomap->bitmap pause-flomap))
(define step-icon (compose flomap->bitmap step-flomap))
(define step-back-icon (compose flomap->bitmap step-back-flomap))
(define continue-icon (compose flomap->bitmap continue-flomap))
(define continue-back-icon (compose flomap->bitmap continue-back-flomap))
