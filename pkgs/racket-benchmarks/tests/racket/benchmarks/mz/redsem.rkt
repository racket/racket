#lang racket/load

#|

Time the cache-less version of the pattern matcher
under the theory that if this speeds up, the cache
can be used less, leading to less memory use, and
better performance.

(and I'm pretty sure that measuing the cache isn't
super useful.)

Robby

|#

(require redex/reduction-semantics)
(caching-enabled? #f)
(require redex/examples/beginner)
(collect-garbage)
(printf "Now\n")

;; At one point, the struct test was failing,
;; and this flag could turn it off:
(define run-struct-test? #t)

(time (begin (run-tests run-struct-test?)
	     (run-tests run-struct-test?)
	     (run-tests run-struct-test?)))
