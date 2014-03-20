#lang racket/base

;; Top-level type environment
;; maps identifiers to their types, updated by mutation

(require "../types/tc-error.rkt"
         "../utils/tc-utils.rkt"
         syntax/parse
         syntax/id-table
         racket/lazy-require) 
(provide register-type register-type-if-undefined
         finish-register-type
         maybe-finish-register-type
         register-type/undefined
         lookup-type
         typed-id^
         register-types
         unregister-type
         check-all-registered-types
         type-env-map)

(lazy-require ["../rep/type-rep.rkt" (Type/c? type-equal?)])

;; free-id-table from id -> type or Box[type]
;; where id is a variable, and type is the type of the variable
;; if the result is a box, then the type has not actually been defined, just registered
(define the-mapping (make-free-id-table))

;; add a single type to the mapping
;; identifier type -> void
(define (register-type id type)
  (free-id-table-set! the-mapping id type))

(define (register-type-if-undefined id type)
  (cond [(free-id-table-ref the-mapping id (lambda _ #f))
         => (lambda (e)
              (define t (if (box? e) (unbox e) e))
              (unless (and (Type/c? t) (type-equal? t type))
                (tc-error/delayed #:stx id "Duplicate type annotation of ~a for ~a, previous was ~a" type (syntax-e id) t))
              (when (box? e)
                (free-id-table-set! the-mapping id t)))]
        [else (register-type id type)]))

;; add a single type to the mapping
;; identifier type -> void
(define (register-type/undefined id type)
  ;(printf "register-type/undef ~a\n" (syntax-e id))
  (cond [(free-id-table-ref the-mapping id (lambda _ #f))
         =>
         (λ (t) ;; it's ok to annotate with the same type
           (define t* (if (box? t) (unbox t) t))
           (unless (and (Type/c? t*) (type-equal? type t*))
             (tc-error/delayed #:stx id "Duplicate type annotation of ~a for ~a, previous was ~a" type (syntax-e id) t*)))]
        [else (free-id-table-set! the-mapping id (box type))]))

;; add a bunch of types to the mapping
;; listof[id] listof[type] -> void
(define (register-types ids types)
  (for-each register-type ids types))

;; given an identifier, return the type associated with it
;; if none found, calls lookup-fail
;; identifier -> type
(define (lookup-type id [fail-handler (λ () (lookup-fail id))])
  (define v (free-id-table-ref the-mapping id fail-handler))
  (cond [(box? v) (unbox v)] 
        [(procedure? v) (define t (v)) (register-type id t) t]
        [else v]))

(define-syntax-class typed-id^
  #:attributes (type)
  (pattern i:id
    #:attr type (lookup-type #'i #f)
    #:when (attribute type)))

(define (maybe-finish-register-type id)
  (let ([v (free-id-table-ref the-mapping id)])
    (if (box? v)
        (register-type id (unbox v))
        #f)))

(define (unregister-type id)
  (free-id-table-remove! the-mapping id))

(define (finish-register-type id [top-level? #f])
  (unless (or (maybe-finish-register-type id) top-level?)
    (tc-error/delayed #:stx id "Duplicate definition for ~a" (syntax-e id))))

(define (check-all-registered-types)
  (free-id-table-for-each
   the-mapping
   (lambda (id e)
     (when (box? e)
       (let ([bnd (identifier-binding id)])
         (tc-error/delayed #:stx id
                           "Declaration for `~a' provided, but `~a' ~a"
                           (syntax-e id) (syntax-e id)
                           (cond [(eq? bnd 'lexical) "is a lexical binding"] ;; should never happen
                                 [(not bnd) "has no definition"]
                                 [else "is defined in another module"])))))))

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-env-map f)
  (free-id-table-map the-mapping f))
