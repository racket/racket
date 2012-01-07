#lang racket/base

(require racket/class
         "../private/flomap.rkt"
         "../private/utils.rkt"
         "style.rkt")

(provide (all-defined-out))

(define (play-flomap color height)
  (draw-icon-flomap
   24 32 (λ (dc)
           (send dc set-brush color 'solid)
           (send dc draw-polygon (list '(0 . 0) '(4 . 0)
                                       '(23 . 13) '(23 . 18)
                                       '(4 . 31) '(0 . 31))))
   (/ height 32)))

(define (fast-forward-flomap color height)
  (draw-icon-flomap
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
   (/ height 32)))

(define (play-icon-flomap* color height material)
  (flomap-render-icon (play-flomap color height) material))

(define (fast-forward-icon-flomap* color height material)
  (flomap-render-icon (fast-forward-flomap color height) material))

(define (stop-icon-flomap* color height material)
  (draw-rendered-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (send dc draw-polygon (list '(0 . 0) '(31 . 0) '(31 . 31) '(0 . 31))))
   (/ height 32)
   material))

(define (record-icon-flomap* color height material)
  (draw-rendered-icon-flomap
   32 32 (λ (dc)
           (send dc set-brush color 'solid)
           (draw-ellipse/smoothed dc 0 0 32 32))
   (/ height 32)
   material))

(define (bar-icon-flomap* color height material)
  (draw-rendered-icon-flomap
   8 32 (λ (dc)
          (send dc set-brush color 'solid)
          (send dc draw-polygon (list '(0 . 0) '(7 . 0) '(7 . 31) '(0 . 31))))
   (/ height 32)
   material))

(define-icon-flomap-proc play-icon-flomap play-icon-flomap* 32 color)
(define-icon-flomap-proc fast-forward-icon-flomap fast-forward-icon-flomap* 32 color)
(define-icon-flomap-proc record-icon-flomap record-icon-flomap* 32 color)
(define-icon-flomap-proc bar-icon-flomap bar-icon-flomap* 32 color)
(define-icon-flomap-proc stop-icon-flomap stop-icon-flomap* 32 color)

(define back-icon-flomap (compose flomap-flip-horizontal play-icon-flomap))
(define reverse-icon-flomap (compose flomap-flip-horizontal fast-forward-icon-flomap))

(define (pause-icon-flomap color [height  (default-icon-height)]
                           [material  (default-icon-material)])
  (flomap-hc-append
   (bar-icon-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/8 height)))) 0)
   (bar-icon-flomap color height material)))

(define (step-icon-flomap color [height  (default-icon-height)]
                          [material  (default-icon-material)])
  (flomap-hc-append
   (play-icon-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-icon-flomap color height material)))

(define (step-back-icon-flomap color [height  (default-icon-height)]
                               [material  (default-icon-material)])
  (flomap-hc-append
   (bar-icon-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (back-icon-flomap color height material)))

(define (continue-icon-flomap color [height  (default-icon-height)]
                              [material  (default-icon-material)])
  (flomap-hc-append
   (bar-icon-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (play-icon-flomap color height material)))

(define (continue-back-icon-flomap color [height  (default-icon-height)]
                                   [material  (default-icon-material)])
  (flomap-hc-append
   (back-icon-flomap color height material)
   (make-flomap 4 (max 1 (inexact->exact (round (* 1/16 height)))) 0)
   (bar-icon-flomap color height material)))

(define play-icon (compose flomap->bitmap play-icon-flomap))
(define back-icon (compose flomap->bitmap back-icon-flomap))
(define fast-forward-icon (compose flomap->bitmap fast-forward-icon-flomap))
(define reverse-icon (compose flomap->bitmap reverse-icon-flomap))
(define bar-icon (compose flomap->bitmap bar-icon-flomap))
(define stop-icon (compose flomap->bitmap stop-icon-flomap))
(define record-icon (compose flomap->bitmap record-icon-flomap))
(define pause-icon (compose flomap->bitmap pause-icon-flomap))
(define step-icon (compose flomap->bitmap step-icon-flomap))
(define step-back-icon (compose flomap->bitmap step-back-icon-flomap))
(define continue-icon (compose flomap->bitmap continue-icon-flomap))
(define continue-back-icon (compose flomap->bitmap continue-back-icon-flomap))
