#lang racket/base

;; Environment for type names

(require "../utils/utils.rkt")

(require syntax/id-table
         (env type-alias-env)
         (utils tc-utils)
         (rep type-rep free-variance)
         (types utils))

(provide register-type-name
         lookup-type-name
         register-type-names
         add-alias
         type-name-env-map

         register-type-variance!
         lookup-type-variance
         type-variance-env-map
         add-constant-variance!
         refine-variance!)

;; a mapping from id -> type (where id is the name of the type)
(define the-mapping
  (make-free-id-table))

;; add a name to the mapping
;; identifier Type -> void
(define (register-type-name id [type #t])
  (free-id-table-set! the-mapping id type))

;; add a bunch of names to the mapping
;; listof[identifier] listof[type] -> void
(define (register-type-names ids types)
  (for-each register-type-name ids types))

;; given an identifier, return the type associated with it
;; optional argument is failure continuation - default calls lookup-fail
;; identifier (-> error) -> type
(define (lookup-type-name id [k (lambda () (lookup-type-fail id))])
  (begin0
    (free-id-table-ref the-mapping id k)
    (add-disappeared-use id)))


;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-name-env-map f)
  (free-id-table-map the-mapping f))

(define (add-alias from to)
  (when (lookup-type-name to (lambda () #f))
    (register-resolved-type-alias
     from
     (make-Name to null #f #t))))


;; a mapping from id -> listof[Variance] (where id is the name of the type)
(define variance-mapping
  (make-free-id-table))

;; add a name to the mapping
;; identifier Type -> void
(define (register-type-variance! id variance)
  (free-id-table-set! variance-mapping id variance))

(define (lookup-type-variance id)
  (free-id-table-ref
   variance-mapping id
   (lambda () (lookup-variance-fail id))))

;; map over the-mapping, producing a list
;; (id variance -> T) -> listof[T]
(define (type-variance-env-map f)
  (free-id-table-map variance-mapping f))

;; Listof<Type> Listof<Option<Listof<Type-Var>>> -> Void
;; Refines the variance of a type in the name environment
(define (refine-variance! names types tvarss)
  (let loop ()
    (define sames?
      (for/and ([name (in-list names)]
                [type (in-list types)]
                [tvars (in-list tvarss)])
        (cond
          [(or (not tvars) (null? tvars)) #t]
          [else
            (define free-vars (free-vars-hash (free-vars* type)))
            (define variance (map (λ (v) (hash-ref free-vars v Constant)) tvars))
            (define old-variance (lookup-type-variance name))

            (register-type-variance! name variance)
            (equal? variance old-variance)])))
    (unless sames? (loop))))

;; Id Option<Listof<Type-Var>> -> Void
;; Initialize variance of the given id to Constant for all type vars
(define (add-constant-variance! name vars)
  (unless (or (not vars) (null? vars))
    (register-type-variance! name (map (lambda (_) Constant) vars))))

