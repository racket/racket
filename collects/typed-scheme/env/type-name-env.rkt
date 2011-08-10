#lang scheme/base
(require "../utils/utils.rkt")

(require syntax/boundmap
         mzlib/trace
         (env type-alias-env)
         (utils tc-utils)
         (rep type-rep)
         (types utils))

(provide register-type-name
         lookup-type-name
         register-type-names
         add-alias
         type-name-env-map)

;; a mapping from id -> type (where id is the name of the type)
(define the-mapping
  (make-module-identifier-mapping))

(define (mapping-put! id v) (module-identifier-mapping-put! the-mapping id v))
;(trace mapping-put!)

;; add a name to the mapping
;; identifier Type -> void
(define (register-type-name id [type #t])
  ;(printf "registering type ~a\n~a\n" (syntax-e id) id)
  (mapping-put! id type))

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
