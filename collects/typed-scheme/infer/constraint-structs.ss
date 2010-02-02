#lang scheme/base

(require "../utils/utils.ss" (rep type-rep) scheme/contract scheme/match (for-syntax scheme/base syntax/parse))

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

(define-match-expander c:
  (lambda (stx)
    (syntax-parse stx
      [(_ s x t)
       #'(struct c ((app (lambda (v) (if (Type? v) v (Un))) s) x (app (lambda (v) (if (Type? v) v Univ)) t)))])))

(p/c (struct c ([S (or/c boolean? Type?)] [X symbol?] [T (or/c boolean? Type?)]))
     (struct dcon ([fixed (listof c?)] [rest (or/c c? false/c)]))
     (struct dcon-exact ([fixed (listof c?)] [rest c?]))
     (struct dcon-dotted ([type c?] [bound symbol?]))
     (struct dmap ([map (hash/c symbol? (or/c dcon? dcon-exact? dcon-dotted?))]))
     (struct cset ([maps (listof (cons/c (hash/c symbol? c?) dmap?))])))
