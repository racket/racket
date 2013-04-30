#lang racket/load

(module promise typed/racket/base
  (require (for-syntax racket/base))
  (provide MyPromise)
  (define-struct: (a) MyPromise ([thunk : (-> a)])
    #:mutable))

(module user racket/base
  (require 'promise)
  (MyPromise (lambda () #f)))
  

(require 'user)
