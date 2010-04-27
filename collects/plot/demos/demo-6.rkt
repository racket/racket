#lang scheme
(require plot)

(define (trig x y) (* (sin x) (sin y)))

(plot (contour trig)
      #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
      #:title "contours of F(x,y) = sin(x) * sin(y)")
