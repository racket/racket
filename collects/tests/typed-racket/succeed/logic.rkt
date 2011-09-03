
#lang typed-scheme

(: f ((U Number #f) (cons Any Any) -> Number))
(define (f x y)
 (cond
   [(and (number? x) (number? (car y))) (+ x (car y))]
   [(number? (car y)) (+ (bool-to-0-or-1 x) (car y))]
   [(number? x) x]
   [else 0]))

(: bool-to-0-or-1 (Boolean -> Number))
(define (bool-to-0-or-1 b)
 (if b 1 0))
