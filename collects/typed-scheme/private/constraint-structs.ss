#lang scheme/base

(require "type-rep.ss"
         scheme/contract)

;; S, T types
;; X a var
(define-struct c (S X T) #:prefab)

;; fixed : Listof[c]
;; rest : option[c]
(define-struct dcon (fixed rest) #:prefab)

;; type : c
;; bound : vars
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

(define (hashof k/c v/c)
  (flat-named-contract
   (format "#<hashof ~a ~a>" k/c v/c)
   (lambda (h)
     (and (hash? h)
          (for/and ([(k v) h])
                   (and (k/c k) (v/c v)))))))

(provide/contract (struct c ([S Type?] [X symbol?] [T Type?]))
                  (struct dcon ([fixed (listof c?)] [rest (or/c c? false/c)]))
                  (struct dcon-dotted ([type c?] [bound symbol?]))
                  (struct dmap ([map (hashof symbol? (lambda (e) (or (dcon? e) (dcon-dotted? e))))]))
                  (struct cset ([maps (listof (cons/c (hashof symbol? c?) dmap?))])))