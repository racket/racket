#lang racket

(require "test-util.rkt"
         redex/private/error
         redex/private/defined-checks)

(reset-count)

(define expected-message "reference to thing x before its definition")

(test (with-handlers ([exn:fail:redex? exn-message])
        (check-defined-lexical x 'x "thing")
        (define x 4)
        "")
      expected-message)

(test (with-handlers ([exn:fail:redex? exn-message])
        (check-defined-module (λ () x) 'x "thing")
        "")
      expected-message)

(define x 4)

(print-tests-passed 'defined-checks-test.rkt)
