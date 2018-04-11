#lang racket/base
(require "../common/phase.rkt"
         "../common/contract.rkt"
         "../syntax/module-binding.rkt"
         "../syntax/api.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../namespace/provided.rkt"
         "../common/module-path.rkt"
         "../namespace/api.rkt"
         "main.rkt")

(provide dynamic-require
         dynamic-require-for-syntax
         default-dynamic-require-fail-thunk)

(define (do-dynamic-require who mod-path sym [fail-k default-dynamic-require-fail-thunk])
  (unless (or (module-path? mod-path)
              (module-path-index? mod-path)
              (resolved-module-path? mod-path))
    (raise-argument-error who
                          "(or/c module-path? module-path-index? resolved-module-path?)"
                          mod-path))
  (unless (or (symbol? sym)
              (not sym)
              (equal? sym 0)
              (void? sym))
    (raise-argument-error who "(or/c symbol? #f 0 void?)" sym))
  (unless (and (procedure? fail-k) (procedure-arity-includes? fail-k 0))
    (raise-argument-error who "(-> any)" fail-k))
  (define ns (current-namespace))
  (define mpi
    (cond
     [(module-path? mod-path) (module-path-index-join mod-path #f)]
     [(module-path-index? mod-path) mod-path]
     [else (module-path-index-join (resolved-module-path->module-path mod-path) #f)]))
  (define mod-name (module-path-index-resolve mpi #t))
  (define phase (namespace-phase ns))
  ;; Dispatch to the variant of `dynamic-require` that is determined
  ;; by the second argument:
  (cond
   [(not sym)
    ;; Run phase 0; don't visit or make available
    (namespace-module-instantiate! ns mpi phase #:run-phase phase
                                   #:otherwise-available? #f)]
   [(equal? sym 0)
    ;; Run phase 0, also make available
    (namespace-module-instantiate! ns mpi phase #:run-phase phase)]
   [(void? sym)
    ;; Just visit
    (namespace-module-visit! ns mpi phase #:visit-phase phase)]
   [else
    ;; Extract a particular value via phase 0....
    (define m (namespace->module ns mod-name))
    (unless m (raise-unknown-module-error 'dynamic-require mod-name))
    (define binding/p (hash-ref (hash-ref (module-provides m) 0 #hasheq())
                                sym
                                #f))
    (cond
     [(not binding/p)
      (if (eq? fail-k default-dynamic-require-fail-thunk)
          (raise-arguments-error 'dynamic-require
                                 "name is not provided"
                                 "name" sym
                                 "module" mod-name)
          (fail-k))]
     [else
      ;; The provided binding may correspond to an immediate provide,
      ;; or it may by re-provided from a different module
      (define binding (provided-as-binding binding/p))
      (define ex-sym (module-binding-sym binding))
      (define ex-phase (module-binding-phase binding))
      (namespace-module-instantiate! ns mpi phase #:run-phase phase
                                     #:otherwise-available? #f)
      (define ex-mod-name (module-path-index-resolve
                           (module-path-index-shift
                            (module-binding-module binding)
                            (module-self m)
                            mpi)))
      (define m-ns (namespace->module-namespace ns ex-mod-name (phase- phase ex-phase)
                                                #:complain-on-failure? #t))
      ;; Before continuing, make sure that we're allowed to access the binding
      (define ex-m (namespace->module ns ex-mod-name))
      (define access (or (module-access ex-m) (module-compute-access! ex-m)))
      (when (and (not (eq? 'provided (hash-ref (hash-ref access ex-phase #hasheq()) ex-sym #f)))
                 (and (not (inspector-superior? (current-code-inspector) (namespace-inspector m-ns)))
                      (not (and (module-binding-extra-inspector binding)
                                (inspector-superior? (module-binding-extra-inspector binding)
                                                     (namespace-inspector m-ns))))))
        (raise-arguments-error 'dynamic-require
                               "name is protected"
                               "name" sym
                               "module" mod-name))
      (define (fail)
        (if (eq? fail-k default-dynamic-require-fail-thunk)
            (raise-arguments-error 'dynamic-require
                                   "name's binding is missing"
                                   "name" sym
                                   "module" mod-name)
            (fail-k)))
      (cond
       [(not (provided-as-transformer? binding/p))
        (namespace-get-variable m-ns ex-phase ex-sym fail)]
       [else
        (define missing (gensym 'missing))
        (namespace-module-visit! ns mpi phase #:visit-phase phase)
        (define t (namespace-get-transformer m-ns ex-phase ex-sym missing))
        (cond
         [(eq? t missing)
          (fail)]
         [else
          ;; Found transformer; expand in a fresh namespace
          (define tmp-ns (new-namespace ns))
          (define mod-path (resolved-module-path->module-path mod-name))
          (namespace-require mod-path tmp-ns)
          (parameterize ([current-namespace tmp-ns])
            (eval sym tmp-ns))])])])]))

;; The `dynamic-require` function cheats by recognizing this failure
;; thunk and substituting a more specific error:
(define (default-dynamic-require-fail-thunk)
  (error "failed"))

(define/who (dynamic-require mod-path sym [fail-k default-dynamic-require-fail-thunk])
  (do-dynamic-require who mod-path sym fail-k))

(define/who (dynamic-require-for-syntax mod-path sym [fail-k default-dynamic-require-fail-thunk])
  (parameterize ([current-namespace
                  (let ([ns (current-namespace)])
                    (namespace->namespace-at-phase ns (add1 (namespace-phase ns))))])
    (do-dynamic-require who mod-path sym fail-k)))
