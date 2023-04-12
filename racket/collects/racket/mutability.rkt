#lang racket/base
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
