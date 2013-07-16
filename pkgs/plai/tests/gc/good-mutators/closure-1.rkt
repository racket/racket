#lang plai/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 60)

(define lst '(2 -10)) ; (cons 2 (cons -10 empty)))

(define (map f lst)
  (if (cons? lst)
      (cons (f (first lst)) (map f (rest lst)))
      empty))

(define x 'gc-garbage)

(test/value=? (map add1 lst) '(3 -9))
