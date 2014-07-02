#lang racket/base

(require racket/dict racket/match 
         syntax/id-table
         (except-in "../utils/utils.rkt" env)
         (contract-req)
         (rep filter-rep type-rep)
         (except-in (utils tc-utils) make-env))

;; types is a free-id-table of identifiers to types
;; props is a list of known propositions
(define-struct/cond-contract env ([types immutable-free-id-table?] [props (listof Filter/c)])
  #:transparent
  #:property prop:custom-write
  (lambda (e prt mode)
    (fprintf prt "(env ~a ~a)" (free-id-table-map (env-types e) list) (env-props e))))

(provide/cond-contract
  [env? predicate/c]
  [extend (env? identifier? Type/c . -> . env?)]
  [extend/values (env? (listof identifier?) (listof Type/c) . -> . env?)]
  [lookup (env? identifier? (identifier? . -> . any) . -> . any)]
  [env-props (env? . -> . (listof Filter/c))]
  [replace-props (env? (listof Filter/c) . -> . env?)]
  [empty-prop-env env?])

(define empty-prop-env
  (env
    (make-immutable-free-id-table)
    null))


(define (replace-props e props)
  (match e
    [(env tys _)
     (env tys props)]))

(define (lookup e key fail)
  (match e
    [(env tys _) (free-id-table-ref tys key (λ () (fail key)))]))


;; extend that works on single arguments
(define (extend e k v)
  (extend/values e (list k) (list v)))

;; takes two lists of identifiers and types to be added
(define (extend/values e ks vs)
  (match e
    [(env tys p)
     (env
       (for/fold ([tys tys]) ([k (in-list ks)] [v (in-list vs)])
         (free-id-table-set tys k v))
       p)]))
