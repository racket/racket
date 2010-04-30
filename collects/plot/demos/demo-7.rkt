#lang scheme
(require plot)

(define (trig x y) (* (sin x) (sin y)))

(plot (vector-field (gradient trig) #:samples 25)
      #:x-min -1.5 #:x-max 1.5 #:y-min -1.5 #:y-max 1.5
      #:title "gradient field of F(x,y) = sin(x) * sin(y)")
