#lang racket/base

(require "../utils/utils.rkt"
	 syntax/id-table racket/dict
         (utils tc-utils)         
         racket/match)

(provide register-type-alias
         lookup-type-alias
         resolve-type-aliases
         register-resolved-type-alias
         type-alias-env-map)

(define-struct alias-def () #:inspector #f)
(define-struct (unresolved alias-def) (stx [in-process #:mutable]) #:inspector #f)
(define-struct (resolved alias-def) (ty) #:inspector #f)

;; a mapping from id -> alias-def (where id is the name of the type)
(define the-mapping
  (make-free-id-table))

(define (mapping-put! id v) (dict-set! the-mapping id v))

;(trace mapping-put!)

;; add a name to the mapping
;; identifier type-stx -> void
(define (register-type-alias id stx)
  ;(printf "registering type ~a\n~a\n" (syntax-e id) id)
  (mapping-put! id (make-unresolved stx #f)))

(define (register-resolved-type-alias id ty)
  (mapping-put! id (make-resolved ty)))

(define (lookup-type-alias id parse-type [k (lambda () (tc-error "Unknown type alias: ~a" (syntax-e id)))])
  (let/ec return
    (match (dict-ref the-mapping id (lambda () (return (k))))
      [(struct unresolved (stx #f))
       (resolve-type-alias id parse-type)]
      [(struct unresolved (stx #t))
       (tc-error/stx stx "Recursive Type Alias Reference")]
      [(struct resolved (t)) t])))

(define (resolve-type-alias id parse-type)
  (define v (dict-ref the-mapping id))
  (match v
    [(struct unresolved (stx _))
     (set-unresolved-in-process! v #t)
     (let ([t (parse-type stx)])
       (mapping-put! id (make-resolved t))
       t)]
    [(struct resolved (t))
     t]))

(define (resolve-type-aliases parse-type)
  (for ([(id _) (in-dict the-mapping)])
    (resolve-type-alias id parse-type)))

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-alias-env-map f)  
  (for/list ([(id t) (in-dict the-mapping)]
             #:when (resolved? t))
    (f id (resolved-ty t))))
