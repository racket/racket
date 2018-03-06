#lang racket/base
(require "../compile/serialize-property.rkt")

(provide (struct-out full-binding)
         binding-frame-id
         binding-free=id)

;; A base struct for bindings with a frame identity or
;; `free-identifier=?` equivalence
(struct full-binding (frame-id   ; used to trigger use-site scopes
                      free=id)   ; `free-identifier=?` equivalence via a rename-transformer binding
  #:authentic
  #:property prop:binding-reach-scopes
  (lambda (b)
    (binding-free=id b)))

(define (binding-frame-id b)
  (and (full-binding? b)
       (full-binding-frame-id b)))

(define (binding-free=id b)
  (and (full-binding? b)
       (full-binding-free=id b)))
