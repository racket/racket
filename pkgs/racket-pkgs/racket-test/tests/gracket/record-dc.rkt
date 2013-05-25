#lang racket/base
(require racket/class
         racket/draw
         (only-in racket/draw/private/record-dc 
                  record-dc-mixin
                  get-recorded-command))

(define bm1 (make-bitmap 100 100))
(define bm2 (make-bitmap 100 100))
(define bm3 (make-bitmap 100 100))

(define dc1 (make-object bitmap-dc% bm1))
(define dc2 (make-object (record-dc-mixin bitmap-dc%) bm2))
(define dc3 (make-object bitmap-dc% bm3))

(define (config dc)
  (send dc set-origin 2 3)
  (send dc set-scale 1.1 0.9)
  (send dc set-rotation 0.1)
  (send dc set-initial-matrix '#(1.0 -0.1 0.1 1.0 1.0 2.0))
  (send dc set-pen "red" 2 'solid)
  (send dc set-brush "blue" 'solid)
  (send dc set-font (make-font #:size 32))
  (send dc set-smoothing 'smoothed)
  (send dc set-text-mode 'solid)
  (send dc set-alpha 0.8)
  (send dc set-clipping-rect 5 5 95 95))

(define (draw dc)
  (send dc draw-ellipse 2 2 100 100)
  (send dc draw-text "Hello" 10 10))

(define (get-bytes bm)
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define bstr (make-bytes (* 4 w h)))
  (send bm get-argb-pixels 0 0 w h bstr)
  bstr)

(config dc1)
(draw dc1)

(define pre-bytes (get-bytes bm1))

(config dc2)
(send dc2 erase)
(draw dc2)

(define middle-bytes (get-bytes bm2))

(define cms (send dc2 get-recorded-command))

(void (cms dc3))

(define post-bytes (get-bytes bm3))

(unless (equal? pre-bytes middle-bytes)
  (error "middle != pre"))

(unless (equal? pre-bytes post-bytes)
  (error "post != pre"))
