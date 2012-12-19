#lang typed/racket

;; Test the define: non-function form

(define: (a) x : (Sequenceof a) empty-sequence)
(define: (a) f : (Integer a -> (Vectorof a)) make-vector)
