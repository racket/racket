#lang scheme/base
(require "../utils/utils.rkt"
	 (for-syntax scheme/base)
         (utils tc-utils) scheme/list
         mzlib/etc scheme/contract)

(provide Covariant Contravariant Invariant Constant Dotted
         combine-frees flip-variances without-below unless-in-table empty-hash-table
         fix-bound make-invariant variance?)

;; this file contains support for calculating the free variables/indexes of types
;; actual computation is done in rep-utils.rkt  and type-rep.rkt
(define-values (Covariant Contravariant Invariant Constant Dotted)
  (let ()
    (define-struct Variance () #:inspector #f)
    (define-struct (Covariant Variance) () #:inspector #f)
    (define-struct (Contravariant Variance) () #:inspector #f)
    (define-struct (Invariant Variance) () #:inspector #f)
    (define-struct (Constant Variance) () #:inspector #f)
    ;; not really a variance, but is disjoint with the others
    (define-struct (Dotted Variance) () #:inspector #f)
    (values (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant) (make-Dotted))))


(define (variance? e)
  (memq e (list Covariant Contravariant Invariant Constant Dotted)))

(define empty-hash-table (make-immutable-hasheq null))

;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-frees freess)    
  (define ht (make-hasheq))
  (define (combine-var v w)
    (cond
      [(eq? v w) v]
      [(eq? v Dotted) w]
      [(eq? w Dotted) v]
      [(eq? v Constant) w]
      [(eq? w Constant) v]
      [else Invariant]))
  (for* ([old-ht (in-list freess)]
         [(sym var) (in-hash old-ht)])
        (let* ([sym-var (hash-ref ht sym (lambda () #f))])
          (if sym-var
              (hash-set! ht sym (combine-var var sym-var))
              (hash-set! ht sym var))))
  ht)

;; given a set of free variables, change bound to ...
;; (if bound wasn't free, this will add it as Dotted
;;  appropriately so that things that expect to see
;;  it as "free" will -- fixes the case where the
;;  dotted pre-type base doesn't use the bound).
(define (fix-bound vs bound)
  (define vs* (hash-map* (lambda (k v) v) vs))
  (hash-set! vs* bound Dotted)
  vs*)

;; frees -> frees
(define (flip-variances vs)
  (hash-map* 
   (lambda (k v) 
     (evcase v
       [Covariant Contravariant]
       [Contravariant Covariant]
       [v v]))
   vs))

(define (make-invariant vs)
  (hash-map* 
   (lambda (k v) Invariant)
   vs))

(define (hash-map* f ht)
  (define new-ht (make-hasheq))
  (for ([(k v) (in-hash ht)])
     (hash-set! new-ht k (f k v)))
  new-ht)

(define (without-below n frees)
  (define new-ht (make-hasheq))
  (for ([(k v) (in-hash frees)])
       (when (>= k n) (hash-set! new-ht k v)))
  new-ht)

(define-syntax (unless-in-table stx) 
  (syntax-case stx ()
    [(_ table val . body)
     (quasisyntax/loc stx
       (hash-ref table val #,(syntax/loc #'body (lambda () . body))))]))
