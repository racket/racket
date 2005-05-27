(define (create-closure x)
  (local ((define a 13)
          (define (closure y)
            (+ a x y)))
    closure))

(define closure-1 (create-closure 1))
(define closure-2 (create-closure 2))

(closure-1 100)