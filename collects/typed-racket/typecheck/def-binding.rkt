#lang scheme/base

(require scheme/contract "../utils/utils.rkt" scheme/struct-info)

(define-struct binding (name) #:transparent)
(define-struct (def-binding binding) (ty) #:transparent)
(define-struct (def-stx-binding binding) () #:transparent)
(define-struct (def-struct-stx-binding def-stx-binding) (static-info) #:transparent)

(provide/cond-contract
 (struct binding ([name identifier?]))
 (struct (def-binding binding) ([name identifier?] [ty any/c]))
 (struct (def-stx-binding binding) ([name identifier?]))
 (struct (def-struct-stx-binding binding) ([name identifier?] [static-info (or/c #f struct-info?)])))
