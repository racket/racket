#lang scheme/base

(require scheme/contract)

(define-struct binding (name) #:transparent)
(define-struct (def-binding binding) (ty) #:transparent)
(define-struct (def-stx-binding binding) () #:transparent)

(provide/contract (struct binding ([name identifier?]))
                  (struct (def-binding binding) ([name identifier?] [ty any/c]))
                  (struct (def-stx-binding binding) ([name identifier?])))
