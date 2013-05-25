#lang typed/racket
(: g (All (a ...) (a ... a -> (List a ... a))))
(define (g . rst)
  (map
   ;; 'a' is in scope due to map rule
   (lambda: ([y : a]) 
     (map
      ;; in scope again
      (lambda: ([z : a])
        (set! y z)
        z)
      rst)
     y)
   rst))
