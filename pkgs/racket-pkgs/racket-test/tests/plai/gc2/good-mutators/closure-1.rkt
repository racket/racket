#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 60)

(define lst '(2 -10)) ; (cons 2 (cons -10 empty)))

(define (map f lst)
  (if (cons? lst)
      (cons (f (first lst)) (map f (rest lst)))
      empty))

(define x 'gc-garbage)

(test/value=? (map (λ (x) (add1 x)) lst) '(3 -9))
