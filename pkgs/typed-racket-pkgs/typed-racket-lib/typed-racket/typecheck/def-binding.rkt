#lang racket/base

(require "../utils/utils.rkt" (contract-req))

(require-for-cond-contract racket/struct-info)

(define-struct binding (name) #:transparent)
(define-struct (def-binding binding) (ty) #:transparent)
(define-struct (def-stx-binding binding) () #:transparent)
(define-struct (def-struct-stx-binding def-stx-binding) (static-info constructor-type) #:transparent)

(provide/cond-contract
 (struct binding ([name identifier?]))
 (struct (def-binding binding) ([name identifier?] [ty any/c]))
 (struct (def-stx-binding binding) ([name identifier?]))
 (struct (def-struct-stx-binding binding) ([name identifier?]
                                           [static-info (or/c #f struct-info?)]
                                           [constructor-type any/c])))
