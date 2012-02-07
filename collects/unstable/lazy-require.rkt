#lang racket/base
(require (for-syntax racket/base
                     compiler/cm-accomplice)
         racket/runtime-path
         racket/promise)
(provide lazy-require)

(define-syntax (lazy-require stx)
  (syntax-case stx ()
    [(lazy-require [modpath (thing ...)] ...)
     #`(begin (define-namespace-anchor anchor)
              (lazy-require1 modpath (thing ...) anchor #,stx)
              ...)]))

(define-syntax (lazy-require1 stx)
  (syntax-case stx ()
    [(lazy-require1 modpath (name ...) anchor orig-stx)
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
                                                     'not-provide-all-defined #t))))))])
       ;; implicit quasiquote, so can use normal module-path syntax
       ;; or escape to compute a the module-path via expression
       #'(begin (define-runtime-module-path-index mpi-var (quasiquote modpath))
                (define-values ()
                  (let-syntax ([_ (do-registration (#%variable-reference) (quasiquote modpath))])
                    (values)))
                (define (get-sym sym)
                  (parameterize ((current-namespace (namespace-anchor->namespace anchor)))
                    (dynamic-require mpi-var sym)))
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

(begin-for-syntax
 (define (do-registration vr modpath)
   (let ([path (resolved-module-path-name
                (module-path-index-resolve
                 (module-path-index-join
                  modpath
                  (variable-reference->resolved-module-path vr))))])
     (when (path? path)
       (register-external-file path)))))
