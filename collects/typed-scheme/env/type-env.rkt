#lang scheme/base

(require "../utils/utils.rkt" 
	 syntax/id-table
         (utils tc-utils)
         (types utils))

(provide register-type register-type-if-undefined
         finish-register-type
         maybe-finish-register-type
         register-type/undefined
         lookup-type
         register-types
         unregister-type
         check-all-registered-types
         type-env-map)

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
              (tc-error/expr #:stx id "Duplicate type annotation for ~a" (syntax-e id))
              (when (box? e)
                (free-id-table-set! the-mapping id (unbox e))))]
        [else (register-type id type)]))

;; add a single type to the mapping
;; identifier type -> void
(define (register-type/undefined id type)
  ;(printf "register-type/undef ~a~n" (syntax-e id))
  (if (free-id-table-ref the-mapping id (lambda _ #f))
      (void (tc-error/expr #:stx id "Duplicate type annotation for ~a" (syntax-e id)))
      (free-id-table-set! the-mapping id (box type))))

;; add a bunch of types to the mapping
;; listof[id] listof[type] -> void
(define (register-types ids types)
  (for-each register-type ids types))

;; given an identifier, return the type associated with it
;; if none found, calls lookup-fail
;; identifier -> type 
(define (lookup-type id [fail-handler (lambda () (lookup-type-fail id))])
  (let ([v (free-id-table-ref the-mapping id fail-handler)])
    (if (box? v) (unbox v) v)))

(define (maybe-finish-register-type id)
  (let ([v (free-id-table-ref the-mapping id)])
    (if (box? v)
        (register-type id (unbox v))
        #f)))

(define (unregister-type id)
  (free-id-table-remove! the-mapping id))

(define (finish-register-type id)
  (unless (maybe-finish-register-type id)
    (tc-error/expr #:stx id "Duplicate defintion for ~a" (syntax-e id)))
  (void))

(define (check-all-registered-types)
  (free-id-table-for-each 
   the-mapping 
   (lambda (id e) 
     (when (box? e) 
       (let ([bnd (identifier-binding id)])
         (tc-error/expr #:stx id
                        "Declaration for ~a provided, but ~a ~a" 
                        (syntax-e id) (syntax-e id)
                        (cond [(eq? bnd 'lexical) "is a lexical binding"] ;; should never happen
                              [(not bnd) "has no definition"] 
                              [else "is defined in another module"]))))
     (void))))

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]  
(define (type-env-map f)
  (free-id-table-map the-mapping f))
