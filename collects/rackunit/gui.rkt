#lang racket/base
(require racket/contract
         (rename-in "private/base.rkt")
         "private/gui/gui.rkt")

(define (test/gui . tests)
  (apply (make-gui-runner) tests))

(define test/c (or/c rackunit-test-case? rackunit-test-suite?))

(provide/contract
 [test/gui
  (->* () () #:rest (listof test/c)
       any)]
 [make-gui-runner
  (->
   (->* () () #:rest (listof test/c)
        any))])
