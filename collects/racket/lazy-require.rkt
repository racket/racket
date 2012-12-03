#lang racket/base
(require (for-syntax racket/base)
         compiler/cm-accomplice
         racket/runtime-path
         racket/promise)
(provide lazy-require)

(define-syntax (lazy-require stx)
  (syntax-case stx ()
    [(lazy-require #:requires-for-path (extra-req ...)
                   [modpath (thing ...)] ...)
     #`(begin
         (lazy-require1 modpath (extra-req ...) (thing ...) #,stx)
         ...)]
    [(lazy-require [modpath (thing ...)] ...)
     (syntax/loc stx
       (lazy-require #:requires-for-path ()
                     [modpath (thing ...)] ...))]))

(define-for-syntax counter 0)

(define-syntax (lazy-require1 stx)
  (syntax-case stx ()
    [(lazy-require1 modpath (extra-req ...) (name ...) orig-stx)
     (with-syntax ([(defn ...)
                    (for/list ([name (in-list (syntax->list #'(name ...)))])
                      (unless (identifier? name)
                        (raise-syntax-error #f "expected identifier" #'orig-stx name))
                      (with-syntax ([name name]
                                    [(aux) (generate-temporaries (list name))])
                        #`(begin (define aux (make-lazy-function 'name get-sym))
                                 (define-syntax name
                                   (make-rename-transformer
                                    (syntax-property (quote-syntax aux)
                                                     'not-provide-all-defined #t))))))]
                   [define-mpi-var
                     (let ([phase (sub1 (variable-reference->phase (#%variable-reference)))])
                       (if (zero? phase)
                           ;; `define-runtime-module-path-index' works right at phase-level 0:
                           #'(define-runtime-module-path-index mpi-var (quasiquote modpath))
                           ;; need a submodule:
                           (with-syntax ([lazy-require-path-n
                                          (string->symbol
                                           (format "lazy-require-path-~a-~a" 
                                                   phase
                                                   counter))])
                             (set! counter (add1 counter))
                             #'(begin
                                 (module lazy-require-path-n racket/base
                                   (require racket/runtime-path
                                            (for-syntax racket/base)
                                            extra-req ...)
                                   (provide mpi-var)
                                   (define-runtime-module-path-index mpi-var (quasiquote modpath)))
                                 (require 'lazy-require-path-n)))))])
       ;; implicit quasiquote, so can use normal module-path syntax
       ;; or escape to compute a the module-path via expression
       #'(begin 
           define-mpi-var
           (define (get-sym sym)
             (parameterize ((current-namespace (variable-reference->namespace (#%variable-reference))))
               (begin0
                (dynamic-require mpi-var sym)
                (do-registration (#%variable-reference) (quasiquote modpath)))))
           defn ...))]))

(define (make-lazy-function name get-sym)
  ;; Use 'delay/sync' because 'delay' promise is not reentrant.
  ;; FIXME: OTOH, 'delay/sync' promise is not kill-safe.
  (let ([fun-p (delay/sync (get-sym name))])
    (procedure-rename
     (make-keyword-procedure
      (lambda (kws kwargs . args)
        (keyword-apply (force fun-p) kws kwargs args)))
     name)))

(define (do-registration vr modpath)
  (let ([path (resolved-module-path-name
               (module-path-index-resolve
                (module-path-index-join
                 modpath
                 (variable-reference->resolved-module-path vr))))])
    (when (path? path)
      (register-external-module path))))
