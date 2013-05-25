#lang racket/base

;; this implements the Delta environment from the TOPLAS paper
;; (as well as every other paper on System F)

;; this environment maps type variables names (symbols)
;; to types representing the type variable
;;
;; The mapped-to type is used to distinguish type variables bound
;; at different scopes

(require "../utils/utils.rkt"
         (rep type-rep)
         racket/dict)

(provide initial-tvar-env
         current-tvars
         extend-tvars
         extend-tvars/new
         bound-tvar?
         lookup-tvar)

;; the initial type variable environment - empty
;; this is used in the parsing of types
(define initial-tvar-env '())

;; a parameter for the current type variables
(define current-tvars (make-parameter initial-tvar-env))

;; extend-tvars
;; takes a list of vars and extends the current type variable
;; environment
(define-syntax-rule (extend-tvars vars . body)
  (parameterize ([current-tvars (extend/many (current-tvars) vars)])
    . body))

;; extend-tvars/new
;; extend with new type variables (provided by, e.g., Poly-fresh:)
(define-syntax-rule (extend-tvars/new vars fresh-vars . body)
  (parameterize ([current-tvars
                  (extend/many (current-tvars) vars fresh-vars)])
    . body))

;; bound-tvar? : symbol -> boolean
;; returns #t if the given type variable is bound
(define (bound-tvar? v)
  (dict-has-key? (current-tvars) v))

;; lookup-tvar : symbol -> type
;; returns the mapped-to type or #f
(define (lookup-tvar var)
  (dict-ref (current-tvars) var #f))

;; extend : type-env symbol option<symbol> -> type-env
;; extend type environment with a free type reference
(define (extend env var [fresh-var #f])
  (dict-set env var (make-F (or fresh-var var))))

;; extend/many : type-env list<symbol> option<list<symbol>> -> type-env
;; extend type environment for many symbols
(define (extend/many env vars [fresh-vars #f])
  (let ([fresh-vars (or fresh-vars (for/list ([_ (in-list vars)]) #f))])
   (for/fold ([env env]) ([var (in-list vars)] [fresh-var (in-list fresh-vars)])
     (extend env var fresh-var))))

