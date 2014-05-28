#lang racket

(void (putenv "PLT_TR_CONTRACTS" "1"))

(define ns (make-base-namespace))
(current-namespace ns)
(use-compiled-file-paths null)

((dynamic-require 'tests/typed-racket/main 'go/text)
 (dynamic-require 'tests/typed-racket/main 'unit-tests))
