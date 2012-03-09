#lang racket/base

(module fac1 racket/base
  (printf "fac1 running\n")
  (require racket/submodule)  
  (define (! n)
    (if (zero? n)
      1
      (* n (! (sub1 n)))))
  (module** test #f
    (printf "fac1 testing\n")
    (require rackunit)
    (check-equal? (! 0) 1))
  (module** test #f
    (check-equal? (! 1) 1)
    (check-equal? (! 5) 120)))

(require (submod 'fac1 test))

(module fac2 racket/base
  (printf "fac2 running\n")
  (require racket/submodule)
  (define (! n)
    (if (zero? n)
      1
      (* n (! (sub1 n)))))
  (when-testing
    (printf "fac2 testing\n")
    (require rackunit)
    (check-equal? (! 0) 1))
  (when-testing
    (check-equal? (! 1) 1)
    (check-equal? (! 5) 120)))

(require (submod 'fac2 test))

(module fac3 racket/base
  (printf "fac3 running\n")
  (require racket/submodule)
  (define (! n)
    (if (zero? n)
      1
      (* n (! (sub1 n)))))
  (module** test #f
    (printf "fac3 testing\n")
    (require rackunit)
    (check-equal? (! 0) 1))
  (when-testing
    (check-equal? (! 1) 1)
    (check-equal? (! 5) 120)))

(require (submod 'fac3 test))
