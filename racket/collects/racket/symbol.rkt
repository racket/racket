#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)
(require (only-in '#%kernel symbol->immutable-string))

(provide symbol->immutable-string)
