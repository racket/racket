#lang racket/base

(require "test-util.rkt")

(parameterize ([current-contract-namespace
                (make-basic-contract-namespace)])
  (test/spec-passed/result 'any/c '(contract any/c 1 'pos 'neg) 1)
  (test/pos-blame 'none/c '(contract none/c 1 'pos 'neg)))
