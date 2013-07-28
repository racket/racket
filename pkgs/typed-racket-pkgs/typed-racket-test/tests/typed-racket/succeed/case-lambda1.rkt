#lang typed/racket


(: f (case->
       (String Symbol * -> (U String Symbol))))
(define f
  (case-lambda
    (w (first w))))

(f "x" 'y)
