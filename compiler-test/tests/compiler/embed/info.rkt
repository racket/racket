#lang info

(define compile-omit-paths '("embed-me9.rkt"
                             "embed-planet-1"
                             
                             ;; Could be compiled, but we skep them to avoid
                             ;; dependencies. This needs to be cleaned up.
                             "embed-me5.rkt"
                             "embed-me19.rkt"
                             "embed-bsl.rkt"
                             "embed-bsla.rkt"
                             "embed-isl.rkt"
                             "embed-isll.rkt"
                             "embed-asl.rkt"))

(define test-omit-paths '("embed-me9.rkt"
                          "embed-planet-1"
                          "embed-planet-2"))
