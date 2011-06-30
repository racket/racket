#lang typed-scheme
(require scheme/port)

(: foo : Number Number -> Number)
(define (foo x y)
  (* x y))

(: bar : Number -> Number)
(define (bar c)
  (: loop : Real Number -> Number)
  (define (loop n acc)
    (if (< 0 n)
	(loop (- n 1) (+ (foo c n) acc))
	acc))
  (loop 10000000 0))
(parameterize ([current-output-port (open-output-nowhere)])
  (time (bar 0)))

