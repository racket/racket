#lang info

(define compile-omit-paths '("embed-me9.rkt"
                             "embed-planet-1"
                             
                             ;; Could be compiled, but we skip them to avoid
                             ;; dependencies. This needs to be cleaned up.
                             "embed-me5.rkt"
                             "embed-me19.rkt"
                             "embed-bsl.rkt"
                             "embed-bsla.rkt"
                             "embed-isl.rkt"
                             "embed-isll.rkt"
                             "embed-asl.rkt"
                             "embed-me36.rkt"))

(define test-omit-paths '("embed-me9.rkt"
                          "embed-planet-1"
                          "embed-planet-2"
                          "embed-me37.rkt"))

(define test-timeouts '(("test.rkt" 1800)))

(define test-responsibles '((all mflatt)))
