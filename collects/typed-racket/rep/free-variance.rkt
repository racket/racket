#lang racket/base
(require "../utils/utils.rkt" (for-syntax racket/base) (contract-req)) 

(provide Covariant Contravariant Invariant Constant Dotted
         combine-frees flip-variances without-below
         fix-bound make-invariant make-constant variance?)

;; this file contains support for calculating the free variables/indexes of types
;; actual computation is done in rep-utils.rkt  and type-rep.rkt
(define-values (Covariant Contravariant Invariant Constant Dotted)
  (let ()
    (define-struct Variance () #:transparent)
    (define-struct (Covariant Variance) () #:transparent)
    (define-struct (Contravariant Variance) () #:transparent)
    (define-struct (Invariant Variance) () #:transparent)
    (define-struct (Constant Variance) () #:transparent)
    ;; not really a variance, but is disjoint with the others
    (define-struct (Dotted Variance) () #:transparent)
    (values (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant) (make-Dotted))))


(define (variance? e)
  (memq e (list Covariant Contravariant Invariant Constant Dotted)))

(struct frees ())
(struct (ht-frees frees) (table))

;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-frees freess)
  (define ((combine-var v) w)
    (cond
      [(eq? v w) v]
      [(eq? v Dotted) w]
      [(eq? w Dotted) v]
      [(eq? v Constant) w]
      [(eq? w Constant) v]
      [else Invariant]))
  (ht-frees
    (for*/fold ([ht #hasheq()])
      ([old-free (in-list freess)]
       [(sym var) (in-hash (ht-frees-table old-ht))])
      (hash-update ht sym (combine-var var) var))))

;; given a set of free variables, change bound to ...
;; (if bound wasn't free, this will add it as Dotted
;;  appropriately so that things that expect to see
;;  it as "free" will -- fixes the case where the
;;  dotted pre-type base doesn't use the bound).
(define (fix-bound vs bound)
  (match vs
    ((ht-frees ht)
     (ht-frees (hash-set ht bound Dotted)))))

;; frees -> frees
(define (flip-variances vs)
  (ht-frees
    (for/hasheq ([(k v) (in-hash (ht-frees-table vs))])
      (values k 
              (cond [(eq? v Covariant) Contravariant]
                    [(eq? v Contravariant) Covariant]
                    [else v])))))

(define (make-invariant vs)
  (ht-frees
    (for/hasheq ([(k v) (in-hash (ht-frees-table vs))])
      (values k Invariant))))

(define (make-constant vs)
  (ht-frees
    (for/hasheq ([(k v) (in-hash (ht-frees-table vs))])
      (values k Constant))))

(define (without-below n frees)
  (ht-frees
    (for/hasheq ([(k v) (in-hash (ht-frees-table frees))]
                 #:when (>= k n))
      (values k v))))

(define (single-free-var name (variance Covariant))
  (ht-frees (hasheq name variance)))

(define empty-free-vars 
  (ht-frees (hasheq)))

(define (free-vars-remove vars name)
  (ht-frees
    (hash-remove (ht-frees-table vars) name)))

;; Only valid after full type resolution
(define (free-vars-hash vars)
  (ht-frees-table vars))


