#lang typed/racket


(struct (A) foo ([x : A]))
(struct (A) bar foo ())


(: test 'b)
(define test
  (let ((x (ann (bar 3) (foo Number))))
    (if (bar? x) 'a 'b)))
