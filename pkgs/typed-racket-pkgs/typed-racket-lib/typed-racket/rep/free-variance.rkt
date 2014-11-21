#lang racket/base
(require racket/match
         racket/set
         racket/lazy-require) 

;; Ugly hack - should use units
(lazy-require
  ("../env/type-name-env.rkt" (lookup-type-variance)))

(provide 
  ;; Variances
  Covariant Contravariant Invariant Constant Dotted
  variance? variance->binding

  ;; Construcing frees
  combine-frees flip-variances
  make-invariant make-constant 
  instantiate-frees
  empty-free-vars
  single-free-var
  free-vars-remove

  ;; Examining frees
  free-vars-hash
  free-vars-names
  free-vars-has-key?)


;; this file contains support for calculating the free variables/indexes of types
;; actual computation is done in rep-utils.rkt  and type-rep.rkt
(define-values (variance? Covariant Contravariant Invariant Constant Dotted)
  (let ()
    (define-struct Variance () #:transparent)
    (define-struct (Covariant Variance) () #:transparent)
    (define-struct (Contravariant Variance) () #:transparent)
    (define-struct (Invariant Variance) () #:transparent)
    (define-struct (Constant Variance) () #:transparent)
    ;; not really a variance, but is disjoint with the others
    (define-struct (Dotted Variance) () #:transparent)
    (values Variance? (make-Covariant) (make-Contravariant) (make-Invariant) (make-Constant) (make-Dotted))))

(define (variance->binding var)
  (match var
   ((== Covariant) #'Covariant)
   ((== Contravariant) #'Contravariant)
   ((== Invariant) #'Invariant)
   ((== Constant) #'Constant)
   ((== Dotted) #'Dotted)))

(define (flip-variance v)
  (match v
   ((== Covariant) Contravariant)
   ((== Contravariant) Covariant)
   (else v)))

;;All of these are used internally
;;Only combined-frees is used externally
(struct combined-frees (table computed) #:transparent)
(struct app-frees (name args) #:transparent)
(struct remove-frees (inner name) #:transparent)


;; Base constructors
(define (single-free-var name (variance Covariant))
  (combined-frees (hasheq name variance) null))

(define empty-free-vars 
  (combined-frees (hasheq) null))

;; Computed constructor
(define (instantiate-frees name frees)
  (combined-frees (hasheq) (list (app-frees name frees))))


;; frees -> frees
(define (flip-variances frees)
  (match frees
   ((combined-frees hash computed)
    (combined-frees
      (for/hasheq (((k v) hash))
        (values k (flip-variance v)))
      (map flip-variances computed)))
   ((app-frees name args)
    (app-frees name (map flip-variances args)))
   ((remove-frees inner name)
    (remove-frees (flip-variances inner) name))))


(define (make-invariant frees)
  (combined-frees
    (for/hasheq ((name (free-vars-names frees)))
      (values name Invariant))
    null))

(define (make-constant frees)
  (combined-frees
    (for/hasheq ((name (free-vars-names frees)))
      (values name Constant))
    null))

;; Listof[frees] -> frees
(define (combine-frees freess)
  (define-values (hash computed)
    (for/fold ((hash (hasheq)) (computed null))
              ((frees freess))
      (match frees
       ((combined-frees new-hash new-computed)
        (values (combine-hashes (list hash new-hash))
                (append new-computed computed))))))
  (combined-frees hash computed))


(define (free-vars-remove frees name)
  (match frees
   ((combined-frees hash computed)
    (combined-frees (hash-remove hash name)
                    (map (λ (v) (remove-frees v name)) computed)))))

;;
(define (free-vars-names vars)
  (match vars
    ((combined-frees hash computed)
     (apply set-union
            (list->seteq (hash-keys hash))
            (map free-vars-names computed)))
    ((remove-frees inner name) (set-remove (free-vars-names inner) name))
    ((app-frees name args)
     (apply set-union (map free-vars-names args)))))

(define (free-vars-has-key? vars key)
  (set-member? (free-vars-names vars) key))

;; Only valid after full type resolution
(define (free-vars-hash vars)
  (match vars
    ((combined-frees hash computed)
     (combine-hashes (cons hash (map free-vars-hash computed))))
    ((remove-frees inner name) (hash-remove (free-vars-hash inner) name))
    ((app-frees name args)
     (combine-hashes
       (for/list ((var (lookup-type-variance name)) (arg args))
        (free-vars-hash
         (cond
          ((eq? var Covariant) arg)
          ((eq? var Contravariant) (flip-variances arg))
          ((eq? var Invariant) (make-invariant arg))
          ((eq? var Constant) (make-constant arg)))))))))


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


