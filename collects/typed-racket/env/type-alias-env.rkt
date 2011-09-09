#lang scheme/base

(require "../utils/utils.rkt"
	 syntax/boundmap
         (utils tc-utils)
         mzlib/trace
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
  (make-module-identifier-mapping))

(define (mapping-put! id v) (module-identifier-mapping-put! the-mapping id v))

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
    (match (module-identifier-mapping-get the-mapping id (lambda () (return (k))))
      [(struct unresolved (stx #f))
       (resolve-type-alias id parse-type)]
      [(struct unresolved (stx #t))
       (tc-error/stx stx "Recursive Type Alias Reference")]
      [(struct resolved (t)) t])))

(define (resolve-type-alias id parse-type)
  (define v (module-identifier-mapping-get the-mapping id))
  (match v
    [(struct unresolved (stx _))
     (set-unresolved-in-process! v #t)
     (let ([t (parse-type stx)])
       (mapping-put! id (make-resolved t))
       t)]
    [(struct resolved (t))
     t]))

(define (resolve-type-aliases parse-type)
  (module-identifier-mapping-for-each the-mapping (lambda (id _) (resolve-type-alias id parse-type))))

;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-alias-env-map f)
  (define sym (gensym))
  (filter (lambda (e) (not (eq? sym e)))
          (module-identifier-mapping-map the-mapping (lambda (id t) (if (resolved? t)
                                                                        (f id (resolved-ty t))
                                                                        sym)))))
