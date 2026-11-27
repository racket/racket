#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)
(require '#%unsafe)

(provide check-not-unsafe-undefined
         check-not-unsafe-undefined/assign
         unsafe-undefined
         prop:chaperone-unsafe-undefined
         chaperone-struct-unsafe-undefined)
