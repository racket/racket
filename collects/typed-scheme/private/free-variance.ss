#lang scheme/base

(require (for-syntax scheme/base)
         mzlib/etc)

;; this file contains support for calculating the free variables/indexes of types
;; actual computation is done in rep-utils.ss  and type-rep.ss

(define-values (Covariant Contravariant Invariant Constant)
  (let ()
    (define-struct Variance () #:inspector #f)
    (define-struct (Covariant Variance) () #:inspector #f)
    (define-struct (Contravariant Variance) () #:inspector #f)
    (define-struct (Invariant Variance) () #:inspector #f)
    (define-struct (Constant Variance) () #:inspector #f)
    (values (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant))))


(provide Covariant Contravariant Invariant Constant)

;; hashtables for keeping track of free variables and indexes
(define index-table (make-weak-hasheq))
;; maps Type to List[Cons[Number,Variance]]
(define var-table (make-weak-hasheq))
;; maps Type to List[Cons[Symbol,Variance]]

(define (free-idxs* t) (hash-ref index-table t (lambda _ (error "type not in index-table" (syntax-e t)))))
(define (free-vars* t) (hash-ref var-table t (lambda _ (error "type not in var-table" (syntax-e t)))))


(define empty-hash-table (make-immutable-hasheq null))

(provide free-vars* free-idxs* empty-hash-table make-invariant)

;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-frees freess)    
  (define ht (make-hasheq))
  (define (combine-var v w)
    (cond
      [(eq? v w) v]
      [(eq? v Constant) w]
      [(eq? w Constant) v]
      [else Invariant]))
  (for-each
   (lambda (old-ht)
     (hash-for-each 
      old-ht
      (lambda (sym var)
        (let* ([sym-var (hash-ref ht sym (lambda () #f))])
          (if sym-var
              (hash-set! ht sym (combine-var var sym-var))
              (hash-set! ht sym var))))))
   freess)
  ht)

;; frees -> frees
(define (flip-variances vs)
  (hash-map* 
   (lambda (k v) 
     (evcase 
         v
       [Covariant Contravariant]
       [Contravariant Covariant]
       [v v]))
   vs))

(define (make-invariant vs)
  (hash-map* 
   (lambda (k v) Invariant)
   vs))

(define (hash-map* f ht)
  (define new-ht (hash-copy ht))
  (hash-for-each 
   new-ht
   (lambda (k v)
     (hash-set! 
      new-ht
      k
      (f k v))))
  new-ht)

(define (without-below n frees)
  (define new-ht (hash-copy frees))
  (hash-for-each 
   new-ht
   (lambda (k v)
     (when (< k n) (hash-remove! new-ht k))))
  new-ht)

(provide combine-frees flip-variances without-below unless-in-table var-table index-table empty-hash-table)

(define-syntax (unless-in-table stx) 
  (syntax-case stx ()
    [(_ table val . body)
     (quasisyntax/loc stx
       (hash-ref table val #,(syntax/loc #'body (lambda () . body))))]))
