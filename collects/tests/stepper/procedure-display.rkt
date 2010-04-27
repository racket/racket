(define a (lambda (b c) (+ b c)))

(define (b c) (+ c 5))

(+ (a 3 5) (b 9))
