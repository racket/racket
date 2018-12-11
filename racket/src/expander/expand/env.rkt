#lang racket/base
(require "../common/memo.rkt"
         "../syntax/syntax.rkt"
         "../syntax/error.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../common/phase.rkt"
         "../syntax/binding.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "protect.rkt"
         "binding-to-module.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "../common/module-path.rkt")

(provide empty-env
         env-extend
         
         variable
         (struct-out core-form)
         
         transformer? transformer->procedure
         variable?

         (struct-out local-variable)
         substitute-variable

         add-binding!
         add-bulk-binding!
         add-local-binding!
         
         binding-lookup)

;; ----------------------------------------

;; An expansion environment maps keys to either `variable` or a
;; compile-time value:
(define empty-env #hasheq())
(define (env-extend env key val)
  (hash-set env key val))
(define (lookup env key default)
  (hash-ref env key default))

;; `variable` is a token to represent a binding to a run-time variable
(define variable (gensym 'variable))
(define (variable? t) (or (eq? t variable)
                          (local-variable? t)))

;; A `local-variable` records a binding identifier, so that a
;; reference can be replaced with the binding identifier
(struct local-variable (id) #:authentic)

;; If a variable binding corresponds to a local binding, substitute
;; the binding identifier in place of the original reference
(define (substitute-variable id t #:no-stops? no-stops?)
  (if (and no-stops? (local-variable? t))
      (let ([bind-id (local-variable-id t)])
        ;; Keep source locations and properties of original reference:
        (syntax-rearm (datum->syntax (syntax-disarm bind-id) (syntax-e bind-id) id id)
                      id))
      id))

;; `missing` is a token to represent the absence of a binding; a
;; distinct token is needed so that it's distinct from all compile-time
;; values
(define missing (gensym 'missing))
(define (missing? t) (eq? t missing))

;; A subset of compile-time values are macro transformers
(define (transformer? t) (or (procedure? t)
                             (set!-transformer? t)
                             (rename-transformer? t)))
(define (transformer->procedure t)
  (cond
   [(set!-transformer? t) (set!-transformer-procedure t)]
   [(rename-transformer? t) (lambda (s) s)] ; "expansion" handled via #:alternate-id
   [else t]))

;; A subset of compile-time values are primitive forms
(struct core-form (expander name) #:transparent #:authentic)

;; ---------------------------------------- 

(define (add-binding! id binding phase #:in [in-s #f] #:just-for-nominal? [just-for-nominal? #f])
  (check-id-taint id in-s)
  (add-binding-in-scopes! (syntax-scope-set id phase) (syntax-e id) binding
                          #:just-for-nominal? just-for-nominal?))

(define (add-bulk-binding! s binding phase
                           #:in [in-s #f]
                           #:shadow-except [shadow-except #f])
  (when (syntax-tainted? s)
    (raise-syntax-error #f "cannot bind from tainted syntax" in-s s))
  (add-bulk-binding-in-scopes! (syntax-scope-set s phase) binding
                               #:shadow-except shadow-except))

;; Helper for registering a local binding in a set of scopes:
(define (add-local-binding! id phase counter
                            #:local-sym local-sym
                            #:frame-id [frame-id #f]
                            #:in [in-s #f])
  (check-id-taint id in-s)
  (define c (add1 (unbox counter)))
  (set-box! counter c)
  (define sym (syntax-content id))
  (define key (string->uninterned-symbol (string-append (symbol->string (or local-sym sym))
                                                        "_"
                                                        (number->string c))))
  (add-binding-in-scopes! (syntax-scope-set id phase) sym (make-local-binding key #:frame-id frame-id))
  key)

(define (check-id-taint id in-s)
  (when (syntax-tainted? id)
    (raise-syntax-error #f "cannot bind tainted identifier" in-s id)))

;; ---------------------------------------- 

;; Returns: `variable` or a compile-time value
;;          #f or #t indicating whether the binding is to a primitive
;;          #f or (for a transformer) an inspector for the defining module
;;          #f or #t for a protected binding
;; A binding provided to `binding-lookup` should be obtained either by
;; passing `#:immediate? #t` to `resolve+shift` or by using `resolve+shift/extra-inspector`,
;; where the latter checks protected access for `free-identifier=?` equivalence
;; chains to provide an inspector associated with the endpoint identifier; using
;; just `resolve+shift` may leave the access with a too-weak inspector.
(define (binding-lookup b env lift-envs ns phase id
                        #:in [in-s #f]
                        #:out-of-context-as-variable? [out-of-context-as-variable? #f])
  (cond
   [(module-binding? b)
    (define top-level? (top-level-module-path-index? (module-binding-module b)))
    (define mi (and (not top-level?) (binding->module-instance b ns phase id)))
    (define m (and mi (module-instance-module mi)))
    (define primitive? (and m (module-primitive? m)))
    (define m-ns (if top-level? ns (and mi (module-instance-namespace mi))))
    (check-taint id)
    (define t (namespace-get-transformer m-ns (module-binding-phase b) (module-binding-sym b)
                                         variable))
    (define protected?
      (and mi (check-access b mi id in-s (if (variable? t) "variable" "transformer"))))
    (define insp (and mi (module-instance-module mi) (module-inspector (module-instance-module mi))))
    (values t primitive? insp protected?)]
   [(local-binding? b)
    (define t (lookup env (local-binding-key b) missing))
    (cond
     [(eq? t missing)
      (values (or
               ;; check in lift envs, if any
               (for/or ([lift-env (in-list lift-envs)])
                 (lookup (unbox lift-env) (local-binding-key b) #f))
               (if out-of-context-as-variable?
                   variable
                   (error "identifier used out of context:" id)))
              #f
              #f
              #f)]
     [else
      (check-taint id)
      (values t #f #f #f)])]
   [else (error "internal error: unknown binding for lookup:" b)]))

;; Check for taints on a variable reference
(define (check-taint id)
  (when (syntax-tainted? id)
    (raise-syntax-error #f
                        "cannot use identifier tainted by macro transformation"
                        id)))
