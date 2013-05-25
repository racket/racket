#lang typed-scheme

(define-type-alias Nb Number)

(let: ([x : Nb 3]) x)

(let ([x '(1 3 5 7 9)])
  (let: doloop : Nb
	([x : (Listof Nb) x]
	 [sum : Number 0])
	(if (null? x) sum
	    (doloop (cdr x) (+ sum (car x))))))

(let ((x '(1 3 5 7 9)))
  (do: : Nb ((x : (Listof Nb) x (cdr x))
	     (sum : Number 0 (+ sum (car x))))
       ((null? x) sum)))
