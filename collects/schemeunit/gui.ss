#lang scheme/base
(require scheme/contract
         (rename-in "private/base.ss")
         "private/gui/gui.ss")

(define (test/gui . tests)
  (apply (make-gui-runner) tests))

(define test/c (or/c schemeunit-test-case? schemeunit-test-suite?))

(provide/contract
 [test/gui
  (->* () () #:rest (listof test/c)
       any)]
 [make-gui-runner
  (->
   (->* () () #:rest (listof test/c)
        any))])
