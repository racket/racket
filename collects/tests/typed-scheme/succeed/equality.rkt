#lang typed/racket


(boolean? true)
(boolean? (not 6))
(immutable? (cons 3 4))

(boolean=? #t false)
(symbol=? 'foo 'foo)
(false? 'foo)

(equal? 1 2)
(eqv? 1 2)
(eq? 1 2)
(equal?/recur 'foo 'bar eq?)
