#lang racket/base

(require racket/draw racket/class racket/match
         racket/unsafe/ops
         "flomap-struct.rkt"
         "flomap-pointwise.rkt"
         "flomap-resize.rkt")

(provide bitmap->flomap flomap->bitmap draw-flomap)

(define (bitmap->flomap bm #:unscaled? [unscaled? #f])
  (unless (is-a? bm bitmap%)
    (raise-type-error 'bitmap->flomap "bitmap% instance" bm))
  
  (define backing-scale (send bm get-backing-scale))
  (define (scale d)
    (if unscaled? (inexact->exact (ceiling (* d backing-scale))) d))
  
  (define w (scale (send bm get-width)))
  (define h (scale (send bm get-height)))
  (define bs (make-bytes (* 4 w h)))
  (send bm get-argb-pixels 0 0 w h bs #t #t #:unscaled? unscaled?)
  (send bm get-argb-pixels 0 0 w h bs #f #t #:unscaled? unscaled?)
  
  (define argb-fm (make-flomap 4 w h))
  (define argb-vs (flomap-values argb-fm))
  (for ([i0  (in-range 0 (* 4 w h) 4)])
    (define i1 (unsafe-fx+ i0 1))
    (define i2 (unsafe-fx+ i0 2))
    (define i3 (unsafe-fx+ i0 3))
    (define a (unsafe-bytes-ref bs i0))
    (define r (unsafe-bytes-ref bs i1))
    (define g (unsafe-bytes-ref bs i2))
    (define b (unsafe-bytes-ref bs i3))
    (unsafe-flvector-set! argb-vs i0 (unsafe-fl/ (unsafe-fx->fl a) 255.0))
    (unsafe-flvector-set! argb-vs i1 (unsafe-fl/ (unsafe-fx->fl r) 255.0))
    (unsafe-flvector-set! argb-vs i2 (unsafe-fl/ (unsafe-fx->fl g) 255.0))
    (unsafe-flvector-set! argb-vs i3 (unsafe-fl/ (unsafe-fx->fl b) 255.0)))
  
  argb-fm)

(define (unsafe-fl->byte x)
  (unsafe-fl->fx
   (unsafe-flround
    (unsafe-flmax 0.0 (unsafe-flmin 255.0 (unsafe-fl* x 255.0))))))

(define (flomap->bitmap fm #:backing-scale [backing-scale 1.0])
  (match-define (flomap vs c w h) fm)
  (let* ([fm  (case c
                [(0)  (make-flomap 4 w h)]
                [(1)  (flomap-append-components (make-flomap 1 w h 1.0) fm fm fm)]
                [(2)  (define alpha-fm (flomap-ref-component fm 0))
                      (define value-fm (flomap-drop-components fm 1))
                      (flomap-append-components alpha-fm value-fm value-fm value-fm)]
                [(3)  (flomap-append-components (make-flomap 1 w h 1.0) fm)]
                [(4)  fm]
                [else  (raise-type-error 'flomap->bitmap "flomap with 1, 2, 3 or 4 components" fm)])]
         ;; inset if zero (bitmaps can't have zero size)
         [fm  (flomap-inset fm 0 0 (if (= w 0) 1 0) (if (= h 0) 1 0))])
    ;; guaranteed an ARGB flomap now
    (match-define (flomap vs 4 w h) fm)
    (define bs (make-bytes (* 4 w h)))
    (for ([i0  (in-range 0 (* 4 w h) 4)])
      (define i1 (unsafe-fx+ i0 1))
      (define i2 (unsafe-fx+ i0 2))
      (define i3 (unsafe-fx+ i0 3))
      (define a (unsafe-flvector-ref vs i0))
      (define r (unsafe-flvector-ref vs i1))
      (define g (unsafe-flvector-ref vs i2))
      (define b (unsafe-flvector-ref vs i3))
      (unsafe-bytes-set! bs i0 (unsafe-fl->byte a))
      (unsafe-bytes-set! bs i1 (unsafe-fl->byte r))
      (unsafe-bytes-set! bs i2 (unsafe-fl->byte g))
      (unsafe-bytes-set! bs i3 (unsafe-fl->byte b)))
    
    (define (scale d) (inexact->exact (ceiling (/ d backing-scale))))
    (define bm (make-bitmap (scale w) (scale h) #:backing-scale backing-scale))
    (send bm set-argb-pixels 0 0 w h bs #t #t #:unscaled? #t)
    (send bm set-argb-pixels 0 0 w h bs #f #t #:unscaled? #t)
    bm))

(define (draw-flomap draw-proc w h)
  (unless (w . >= . 0) (raise-type-error 'draw-flomap "nonnegative fixnum" 0 w h draw-proc))
  (unless (h . >= . 0) (raise-type-error 'draw-flomap "nonnegative fixnum" 1 w h draw-proc))
  
  (define bm (make-bitmap (max w 1) (max h 1)))
  (define dc (make-object bitmap-dc% bm))
  (send dc set-smoothing 'smoothed)
  (draw-proc dc)
  (flomap-inset (bitmap->flomap bm) 0 0 (if (= w 0) -1 0) (if (= h 0) -1 0)))
