#lang scheme/load

(module m scheme
  (define-struct X (x) #:transparent)
  (define-struct (Y X) (y) #:transparent)
  (provide (all-defined-out)))

(module n typed-scheme
  (require-typed-struct X ([x : Number]) #:extra-constructor-name make-X 'm)
  (require-typed-struct (Y X) ([y : Number]) #:extra-constructor-name make-Y 'm)
  (make-X 43)
  (define: x : Any 3)
  (if (Y? x)
      (X-x x)
      4))

(require 'n)
