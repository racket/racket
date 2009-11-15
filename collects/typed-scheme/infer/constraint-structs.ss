#lang scheme/base

(require "../utils/utils.ss" (rep type-rep) scheme/contract)

;; S, T types
;; X a var
(define-struct c (S X T) #:prefab)

;; fixed : Listof[c]
;; rest : option[c]
(define-struct dcon (fixed rest) #:prefab)

;; fixed : Listof[c]
;; rest : c
(define-struct dcon-exact (fixed rest) #:prefab)

;; type : c
;; bound : var
(define-struct dcon-dotted (type bound) #:prefab)

;; map : hash mapping variable to dcon or dcon-dotted
(define-struct dmap (map) #:prefab)

;; maps is a list of pairs of
;;    - functional maps from vars to c's
;;    - dmaps (see dmap.ss)
;; we need a bunch of mappings for each cset to handle case-lambda
;; because case-lambda can generate multiple possible solutions, and we
;; don't want to rule them out too early
(define-struct cset (maps) #:prefab)

(p/c (struct c ([S Type?] [X symbol?] [T Type?]))
     (struct dcon ([fixed (listof c?)] [rest (or/c c? false/c)]))
     (struct dcon-exact ([fixed (listof c?)] [rest c?]))
     (struct dcon-dotted ([type c?] [bound symbol?]))
     (struct dmap ([map (hash/c symbol? (or/c dcon? dcon-exact? dcon-dotted?))]))
     (struct cset ([maps (listof (cons/c (hash/c symbol? c?) dmap?))])))
