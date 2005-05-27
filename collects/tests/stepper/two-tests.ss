(define (mult a b) (* a b))
(define g (if #f mult +))
(define (f a b)  (g 3 4))
(f 5 6)

;(define (f x) x)
;(define g +)
;g