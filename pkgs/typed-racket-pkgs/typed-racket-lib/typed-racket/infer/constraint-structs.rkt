#lang racket/base

(require "../utils/utils.rkt" (contract-req))

(require-for-cond-contract (rep type-rep))

;; S, T types
;; represents S <: X <: T (see "Local Type Inference" pg. 12)
(define-struct/cond-contract c ([S Type/c] [T Type/c]) #:transparent)

;; fixed : Listof[c]
;; rest : option[c]
;; a constraint on an index variable
;; the index variable must be instantiated with |fixed| arguments, each meeting the appropriate constraint
;; and further instantions of the index variable must respect the rest constraint, if it exists
(define-struct/cond-contract dcon ([fixed (listof c?)] [rest (or/c c? #f)]) #:transparent)

;; fixed : Listof[c]
;; rest : c
(define-struct/cond-contract dcon-exact ([fixed (listof c?)] [rest c?]) #:transparent)

;; fixed : Listof[c]
;; type : c
;; bound : var
(define-struct/cond-contract dcon-dotted ([fixed (listof c?)] [type c?] [bound symbol?]) #:transparent)

(define-for-cond-contract dcon/c (or/c dcon? dcon-exact? dcon-dotted?))

;; map : hash mapping index variables to dcons
(define-struct/cond-contract dmap ([map (hash/c symbol? dcon/c)]) #:transparent)

;; maps is a list of pairs of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.rkt)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(define-struct/cond-contract cset ([maps (listof (cons/c (hash/c symbol? c? #:immutable #t) dmap?))]) #:transparent)

(provide-for-cond-contract dcon/c)
(provide
  (struct-out cset)
  (struct-out dmap)
  (struct-out dcon)
  (struct-out dcon-dotted)
  (struct-out dcon-exact)
  (struct-out c))
