#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)
(require (only-in '#%kernel keyword->immutable-string))

(provide keyword->immutable-string)
