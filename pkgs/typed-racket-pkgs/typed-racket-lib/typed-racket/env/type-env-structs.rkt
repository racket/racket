#lang racket/base

(require racket/dict racket/match 
         syntax/id-table
         (except-in "../utils/utils.rkt" env)
         (contract-req)
         (rep filter-rep type-rep object-rep)
         (except-in (utils tc-utils) make-env))

;; types is a free-id-table of identifiers to types
;; props is a list of known propositions
;; objs is a free-id-table of identifiers to paths
;;
;; objs represents variables which alias another variable (or a subpart of one).
;; Some paths in objs point at an identifier in types, others point at identifiers in the module level
;; environment.
(define-struct/cond-contract env
  ([types immutable-free-id-table?]
   [props (listof Filter/c)]
   [objs immutable-free-id-table?])
  #:transparent
  #:property prop:custom-write
  (lambda (e prt mode)
    (fprintf prt "~a"
      (list 'env
        (free-id-table-map (env-types e) list)
        (env-props e)
        (free-id-table-map (env-objs e) list)))))

(provide/cond-contract
  [env? predicate/c]
  [refine (env? identifier? Type/c . -> . env?)]
  [extend/values (env? (listof identifier?) (listof Type/c) (listof Object/c) . -> . env?)]
  [lookup (env? identifier? (identifier? . -> . any) . -> . any)]
  [env-props (env? . -> . (listof Filter/c))]
  [replace-props (env? (listof Filter/c) . -> . env?)]
  [empty-prop-env env?])

(define empty-prop-env
  (env
    (make-immutable-free-id-table)
    null
    (make-immutable-free-id-table)))


(define (replace-props e props)
  (match e
    [(env tys _ objs)
     (env tys props objs)]))

(define (lookup e key fail)
  (match e
    [(env tys _ objs)
     ;; TODO: Support all paths
     (match-define (Path: (list) root-id)
       (free-id-table-ref objs key (make-Path null key)))
     ;; Since we only support the trivial path we do not need to do any work on the type.
     (free-id-table-ref tys root-id (λ () (fail root-id)))]))


;; Refine an identifier to a specific type.
(define (refine e id ty)
  (match e
    [(env tys p objs)
     ;; TODO: Support all paths
     (match-define (Path: (list) root-id)
       (free-id-table-ref objs id (make-Path null id)))
     ;; Since we only support the trivial path we do not need to do any work on the type.
     (env (free-id-table-set tys root-id ty) p objs)]))

;; takes three lists: the identifiers, types, and objects to be added
(define (extend/values e ks vs os)
  ;; TODO: Support all paths
  (define (simple-path? o)
    (match o
      [(Path: (list) _) #t]
      [_ #f]))
  (match e
    [(env tys p objs)
     (env
       (for/fold ([tys tys]) ([k (in-list ks)] [v (in-list vs)] [o (in-list os)])
         (match o
           [(Empty:) (free-id-table-set tys k v)]
           [(Path: (list) id) (free-id-table-set tys id v)]
           ;; TODO: Do refinement of the type mapped to id in the cases where we use non simple paths.
           [(Path: _ _) (free-id-table-set tys k v)]))
       p
       (for/fold ([objs objs]) ([k (in-list ks)] [o (in-list os)]
                                #:when (simple-path? o))
         (match-define (Path: (list) root-id) o)
         (free-id-table-set objs k (free-id-table-ref objs root-id o))))]))
