#lang racket/base
(require racket/sandbox)

(sandbox-output 'bytes)
(define e (make-evaluator 'tests/compiler/demodularizer/sandbox-test/main))
(unless (equal? (get-output e) #"\"ok\"\n")
  (error "demod sandbox test failed"))

