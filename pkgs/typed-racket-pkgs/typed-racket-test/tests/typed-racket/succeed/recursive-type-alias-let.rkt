#lang typed/racket

(let ()
  (define-type MyList (U Null (Pairof Integer MyList)))
  (: x MyList)
  (define x '(1 2 3))
  (cast x MyList))

(let ()
  (define-type MyList (U Null (Pairof Integer MyList)))
  (: x MyList)
  (define x '())
  (make-predicate MyList))
