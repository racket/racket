#lang racket/base

#|
;; This is how "logo-and-text.png" was generated, but it's commented
;; out here to avoid a dependency on `racket/draw`.

(require racket/draw)

(define logo (read-bitmap "logo.png"))
(define s 0.72)

(define font (make-font #:face "Optima" #:weight 'bold #:size 44 #:size-in-pixels? #t))

(define-values (tw th)
  (let ()
    (define bm (make-bitmap 2 2))
    (define dc (send bm make-dc))
    (define-values (w h d a) (send dc get-text-extent "Racket" font #t))
    (values (inexact->exact (ceiling w))
            (inexact->exact (ceiling h)))))

(define sep 0)

(define bm (make-bitmap (+ (inexact->exact (ceiling (* s (send logo get-width))))
                           sep
                           tw
                           5)
                        60
                        #:backing-scale 2))
(define dc (send bm make-dc))
(send dc set-smoothing 'smoothed)

(send dc set-brush (make-brush #:color "black"))
(send dc draw-rectangle -10 -10 1000 1000)

(send dc set-scale s s)
(send dc draw-bitmap logo 1
      (+ 2
         (/ (- (send bm get-height)
               (* s (send logo get-height)))
            2)))

(send dc set-scale 1 1)
(send dc set-font font)
(send dc set-text-foreground "white")
(send dc draw-text
      "Racket"
      (+ (* s (send logo get-width))
         1
         sep)
      (/ (- (send bm get-height) th) 2)
      #t)

; (send bm save-file "/tmp/x.png" 'png #:unscaled? #t)
|#

