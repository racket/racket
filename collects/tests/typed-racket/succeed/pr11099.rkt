#lang typed/racket

(struct: (X) b ([bar : (Vectorof X)]))

(define: b-val : (b Integer)
  (b (ann (vector 1) (Vectorof Integer))))


(if (b? b-val)
    (b-bar b-val) 
    #f)
