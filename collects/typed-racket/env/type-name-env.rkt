#lang racket/base
(require "../utils/utils.rkt")

(require syntax/boundmap
         racket/dict
         (env type-alias-env)
         (utils tc-utils)
         (rep type-rep)
         (types utils))

(provide register-type-name
         lookup-type-name
         register-type-names
         add-alias
         type-name-env-map

         register-type-variance!
         lookup-type-variance
         type-variance-env-map)

;; a mapping from id -> type (where id is the name of the type)
(define the-mapping
  (make-module-identifier-mapping))

;; add a name to the mapping
;; identifier Type -> void
(define (register-type-name id [type #t])
  (module-identifier-mapping-put! the-mapping id type))

;; add a bunch of names to the mapping
;; listof[identifier] listof[type] -> void
(define (register-type-names ids types)
  (for-each register-type-name ids types))

;; given an identifier, return the type associated with it
;; optional argument is failure continuation - default calls lookup-fail
;; identifier (-> error) -> type
(define (lookup-type-name id [k (lambda () (lookup-type-fail id))])
  (begin0
    (module-identifier-mapping-get the-mapping id k)
    (add-disappeared-use id)))


;; map over the-mapping, producing a list
;; (id type -> T) -> listof[T]
(define (type-name-env-map f)
  (module-identifier-mapping-map the-mapping f))

(define (add-alias from to)
  (when (lookup-type-name to (lambda () #f))
    (register-resolved-type-alias from (make-Name to))))


;; a mapping from id -> listof[Variance] (where id is the name of the type)
(define variance-mapping
  (make-module-identifier-mapping))

;; add a name to the mapping
;; identifier Type -> void
(define (register-type-variance! id variance)
  (module-identifier-mapping-put! variance-mapping id variance))

(define (lookup-type-variance id )
  (module-identifier-mapping-get variance-mapping id))

;; map over the-mapping, producing a list
;; (id variance -> T) -> listof[T]
(define (type-variance-env-map f)
  (module-identifier-mapping-map variance-mapping f))



