#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)
(require (only-in '#%kernel
                  immutable-string?
                  immutable-bytes?
                  immutable-vector?
                  immutable-box?
                  immutable-hash?
                  mutable-string?
                  mutable-bytes?
                  mutable-vector?
                  mutable-box?
                  mutable-hash?))

(provide immutable-string?
         immutable-bytes?
         immutable-vector?
         immutable-box?
         immutable-hash?
         mutable-string?
         mutable-bytes?
         mutable-vector?
         mutable-box?
         mutable-hash?)
