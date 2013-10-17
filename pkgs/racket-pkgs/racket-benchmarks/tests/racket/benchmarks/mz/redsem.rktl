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
;; Check for the command line flag --skip-struct-test
;; If it's set, don't run the (currently-failing) test
;; for define-struct in beginner
;; This flag is so that DrDr can avoid raising an error here. 
;; -- samth
(define run-struct-test?
  (let ([run? #t])
    (command-line
     #:once-each
     ["--skip-struct-test" "skip failing struct test" (set! run? #f)])
    run?))

(time (begin (run-tests run-struct-test?)
	     (run-tests run-struct-test?)
	     (run-tests run-struct-test?)))
