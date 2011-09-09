#lang scheme
(require "test-utils.rkt" rackunit)

(provide module-tests)

(define (module-tests)
  (test-suite "Tests for whole modules"
              #;(test-not-exn "name" (lambda () (expand #'(module m (planet "typed-scheme.rkt" ("plt" "typed-scheme.plt"))
                                                            (define: x : number 3)))))
              ))


(define-go module-tests)

