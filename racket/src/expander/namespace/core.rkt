#lang racket/base
(require racket/private/place-local
         "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../expand/env.rkt"
         "../syntax/match.rkt"
         "../common/module-path.rkt"
         "provided.rkt"
         "namespace.rkt"
         "module.rkt")

(provide core-stx
         core-id
         
         add-core-form!
         add-core-primitive!
         
         declare-core-module!
         
         core-module-name
         core-mpi
         core-form-sym

         core-place-init!)

;; Accumulate all core bindings in `core-scope`, so we can
;; easily generate a reference to a core form using `core-stx`:
(define core-scope (new-multi-scope))
(define core-stx (add-scope empty-syntax core-scope))

(define core-module-name (make-resolved-module-path '#%core))
(define core-mpi (module-path-index-join ''#%core #f))

;; The expander needs to synthesize some core references

(define-place-local id-cache-0 (make-hasheq))
(define-place-local id-cache-1 (make-hasheq))

(define (core-place-init!)
  (set! id-cache-0 (make-hasheq))
  (set! id-cache-1 (make-hasheq)))

(define (core-id sym phase)
  (cond
   [(eqv? phase 0)
    (or (hash-ref id-cache-0 sym #f)
        (let ([s (datum->syntax core-stx sym)])
          (hash-set! id-cache-0 sym s)
          s))]
   [(eq? phase 1)
    (or (hash-ref id-cache-1 sym #f)
        (let ([s (datum->syntax (syntax-shift-phase-level core-stx 1) sym)])
          (hash-set! id-cache-1 sym s)
          s))]
   [else
    (datum->syntax (syntax-shift-phase-level core-stx phase) sym)]))

;; Core forms and primitives are added by `require`s in "expander.rkt"

;; Accumulate added core forms and primitives:
(define core-forms #hasheq())
(define core-primitives #hasheq())

(define-syntax-rule (add-core-form! sym proc)
  ;; The `void` wrapper suppress a `print-values` wrapper:
  (void (add-core-form!* sym proc)))
  
(define (add-core-form!* sym proc)
  (add-core-binding! sym)
  (set! core-forms (hash-set core-forms
                             sym
                             proc)))

(define (add-core-primitive! sym val)
  (add-core-binding! sym)
  (set! core-primitives (hash-set core-primitives
                                  sym
                                  val)))

(define (add-core-binding! sym)
  (add-binding! (datum->syntax core-stx sym)
                (make-module-binding core-mpi 0 sym)
                0))

;; Used only after filling in all core forms and primitives:
(define (declare-core-module! ns)
  (declare-module!
   ns
   (make-module #:cross-phase-persistent? #t
                #:no-protected? #t
                #:predefined? #t
                #:self core-mpi
                #:provides
                (hasheqv 0 (for/hasheq ([syms (in-list (list core-primitives
                                                             core-forms))]
                                        [syntax? (in-list '(#f #t))]
                                        #:when #t
                                        [sym (in-hash-keys syms)])
                             (define b (make-module-binding core-mpi 0 sym))
                             (values sym (if syntax? (provided b #f #t) b))))
                #:phase-level-linklet-info-callback
                (lambda (phase-level ns insp)
                  (and (zero? phase-level)
                       (let ([ns (namespace->module-namespace ns core-module-name 0)])
                         (and ns
                              (module-linklet-info (namespace->instance ns 0)
                                                   #f
                                                   core-mpi
                                                   #f
                                                   #f
                                                   #f)))))
                #:instantiate-phase-callback
                (lambda (data-box ns phase phase-level self bulk-binding-registry insp)
                  (case phase-level
                    [(0)
                     (for ([(sym val) (in-hash core-primitives)])
                       (namespace-set-consistent! ns 0 sym val))
                     (for ([(sym proc) (in-hash core-forms)])
                       (namespace-set-transformer! ns 0 sym (if (procedure-arity-includes? proc 2)
                                                                ;; An actual core form:
                                                                (core-form proc sym)
                                                                ;; A macro:
                                                                proc)))])))
   core-module-name))

;; Helper for recognizing and dispatching on core forms:
(define (core-form-sym s phase)
  (define-match m s #:try '(id . _))
  (and (m)
       (let ([b (resolve+shift (m 'id) phase)])
         (and (module-binding? b)
              (eq? core-module-name (module-path-index-resolve (module-binding-module b)))
              (module-binding-sym b)))))
