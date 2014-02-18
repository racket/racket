#lang typed/racket

;; Test typed/pict

(require typed/pict)

(pict-children (blank 50 50))
(hline 1 5)
(vline 1 5)
(frame (circle 5) #:segment 2)

(define hl (hline 1 5))
(define-values (x y) (lt-find hl hl))

(standard-fish 40 20 #:direction 'left #:color "olive")

