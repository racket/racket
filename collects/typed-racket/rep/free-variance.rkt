#lang racket/base
(require "../utils/utils.rkt" 
         racket/match
         (for-syntax racket/base)
         unstable/lazy-require
         (contract-req)) 

;; Ugly hack - should use units
(lazy-require
  ("../env/type-name-env.rkt" (lookup-type-variance)))

(provide Covariant Contravariant Invariant Constant Dotted
         combine-frees flip-variances without-below
         fix-bound make-invariant make-constant variance?
         instantiate-frees
         empty-free-vars
         single-free-var
         free-vars-remove
         free-vars-hash
         free-vars-has-key?
         variance->binding
         (struct-out named-poly-variance))


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

(define (variance->binding var)
  (match var
   ((== Covariant) #'Covariant)
   ((== Contravariant) #'Contravariant)
   ((== Invariant) #'Contravariant)
   ((== Constant) #'Constant)
   ((== Dotted) #'Dotted)))


(define (variance? e)
  (memq e (list Covariant Contravariant Invariant Constant Dotted)))

(define (flip-variance v)
  (match v
   ((== Covariant) Contravariant)
   ((== Contravariant) Covariant)
   (else v)))

;; Represents how a struct varies
(struct named-poly-variance (name) #:transparent)

(struct frees () #:transparent)
(struct empty-frees frees () #:transparent)
(struct single-frees frees (name bound) #:transparent)
(struct app-frees frees (variance args) #:transparent)
(struct combined-frees frees (inner) #:transparent)
(struct remove-frees frees (inner name) #:transparent)
(struct without-below-frees frees (inner bound) #:transparent)
(struct update-frees frees (inner name value) #:transparent)
(struct update-all-frees frees (inner value) #:transparent)
(struct flip-variance-frees frees (inner) #:transparent)


;; given a set of free variables, change bound to ...
;; (if bound wasn't free, this will add it as Dotted
;;  appropriately so that things that expect to see
;;  it as "free" will -- fixes the case where the
;;  dotted pre-type base doesn't use the bound).
(define (fix-bound vs bound)
  (update-frees vs bound Dotted))

;; frees -> frees
(define (flip-variances vs)
  (flip-variance-frees vs))


(define (make-invariant vs)
  (update-all-frees vs Invariant))

(define (make-constant vs)
  (update-all-frees vs Constant))

(define (combine-frees frees)
  (combined-frees frees))

(define (instantiate-frees variance frees)
  (app-frees variance frees))

(define (without-below n frees)
  (without-below-frees frees n))

(define (single-free-var name (variance Covariant))
  (single-frees name variance))

(define empty-free-vars 
  (empty-frees))

(define (free-vars-remove vars name)
  (remove-frees vars name))


(define (free-vars-has-key? vars key)
  (hash-has-key? (free-vars-hash vars) key))

;; Only valid after full type resolution
(define (free-vars-hash vars)
  (match vars
    ((empty-frees) (hasheq))
    ((single-frees name bound) (hasheq name bound))
    ((combined-frees inner) (combine-hashes (map free-vars-hash inner)))
    ((remove-frees inner name) (hash-remove (free-vars-hash inner) name))
    ((without-below-frees inner bound) (without-below-hash (free-vars-hash inner) bound))
    ((update-frees inner name value) (hash-set (free-vars-hash inner) name value))
    ((update-all-frees inner value)
     (set-variance-hash (free-vars-hash inner) value))
    ((app-frees (named-poly-variance name) args)
     (combine-hashes
       (for/list ((var (lookup-type-variance name)) (arg args))
         (define hash (free-vars-hash arg))
         (cond
          ((eq? var Covariant) hash)
          ((eq? var Contravariant) (flip-variance-hash hash))
          ((eq? var Invariant) (set-variance-hash hash Invariant))
          ((eq? var Constant) (set-variance-hash hash Constant))))))
    ((flip-variance-frees inner)
     (flip-variance-hash (free-vars-hash inner)))))


(define (flip-variance-hash hash)
 (for/hasheq (((k v) hash))
   (values k (flip-variance v))))

(define (set-variance-hash hash value)
 (for/hasheq (((k v) hash))
   (values k value)))


(define (without-below-hash frees n)
    (for/hasheq ([(k v) (in-hash frees)]
                 #:when (>= k n))
      (values k v)))

;; frees = HT[Idx,Variance] where Idx is either Symbol or Number
;; (listof frees) -> frees
(define (combine-hashes hashes)
  (define ((combine-var v) w)
    (cond
      [(eq? v w) v]
      [(eq? v Dotted) w]
      [(eq? w Dotted) v]
      [(eq? v Constant) w]
      [(eq? w Constant) v]
      [else Invariant]))
    (for*/fold ([ht #hasheq()])
      ([old-free (in-list hashes)]
       [(sym var) (in-hash old-free)])
      (hash-update ht sym (combine-var var) var)))


