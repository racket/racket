(define (add-1 x) (+ 1 x))

(define g add-1)

g

(define h g)

(h 4)
