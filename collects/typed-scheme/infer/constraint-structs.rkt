#lang scheme/base

(require "../utils/utils.rkt" (rep type-rep) scheme/contract scheme/match (for-syntax scheme/base syntax/parse))

;; S, T types
;; X a var
;; represents S <: X <: T
(d-s/c c ([S Type/c] [X symbol?] [T Type/c]) #:transparent)

;; fixed : Listof[c]
;; rest : option[c]
(d-s/c dcon ([fixed (listof c?)] [rest (or/c c? #f)]) #:transparent)

;; fixed : Listof[c]
;; rest : c
(d-s/c dcon-exact ([fixed (listof c?)] [rest c?]) #:transparent)

;; type : c
;; bound : var
(d-s/c dcon-dotted ([type c?] [bound symbol?]) #:transparent)

;; map : hash mapping variable to dcon or dcon-dotted
(d-s/c dmap ([map (hash/c symbol? (or/c dcon? dcon-exact? dcon-dotted?))]) #:transparent)

;; maps is a list of pairs of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(d-s/c cset ([maps (listof (cons/c (hash/c symbol? c?) dmap?))]) #:transparent)

(define-match-expander c:
  (lambda (stx)
    (syntax-parse stx
      [(_ s x t)
       #'(struct c (s x t))])))

(provide (struct-out cset) (struct-out dmap) (struct-out dcon) (struct-out dcon-dotted) (struct-out dcon-exact) (struct-out c)
         c:)
