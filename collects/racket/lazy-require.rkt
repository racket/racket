#lang racket/base
(require (for-syntax racket/base)
         compiler/cm-accomplice
         racket/runtime-path
         racket/promise)
(provide lazy-require)

(define-syntax (lazy-require stx)
  (syntax-case stx ()
    [(lazy-require [modpath (thing ...)] ...)
     #`(begin (lazy-require1 modpath (thing ...) #,stx) ...)]))

(define-for-syntax counter 0)

(begin-for-syntax
 ;; like (collapse-module-path modpath '(submod "..")), but avoids
 ;; the dependencies of syntax/modcollapse
 (define (module-path-add-submod-up modpath)
   (let ([d (syntax->list modpath)])
     (cond [(and d (list? d) (>= (length d) 2) (eq? (syntax-e (car d)) 'submod))
            ;; was a submod module-path
            (let ([parts (cdr d)]) ;; length >= 1
              (cond [(or (equal? (syntax-e (car parts)) ".")
                         (equal? (syntax-e (car parts)) ".."))
                     ;; (submod "." part ...) => (submod "." ".." part ...)
                     ;; (submod ".." part ...) => (submod ".." ".." part ...)
                     (datum->syntax modpath
                                    (list* (car d) (car parts) ".." (cdr parts))
                                    modpath)]
                    [else
                     ;; wasn't relative
                     ;; FIXME: what about 'mod = (submod "." mod) abbreviation?
                     modpath]))]
           [else
            ;; wasn't a submod module-path
            modpath]))))

(define-syntax (lazy-require1 stx)
  (syntax-case stx ()
    [(lazy-require1 modpath (name ...) orig-stx)
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
                           #'(define-runtime-module-path-index mpi-var (quote modpath))
                           ;; need a submodule:
                           (with-syntax ([lazy-require-path-n
                                          (string->symbol
                                           (format "lazy-require-path-~a-~a" 
                                                   phase
                                                   counter))]
                                         ;; May need to adjust modpath, since we're interpreting it
                                         ;; relative to a *submodule* of the original context.
                                         ;; ie, module-path interpretation is not hygienic!
                                         [modpath* (module-path-add-submod-up #'modpath)])
                             (set! counter (add1 counter))
                             #'(begin
                                 (module lazy-require-path-n racket/base
                                   (require racket/runtime-path
                                            (for-syntax racket/base))
                                   (provide mpi-var)
                                   (define-runtime-module-path-index mpi-var (quote modpath*)))
                                 (require 'lazy-require-path-n)))))])
       #'(begin 
           define-mpi-var
           (define (get-sym sym)
             (parameterize ((current-namespace (variable-reference->namespace (#%variable-reference))))
               (begin0
                (dynamic-require mpi-var sym)
                (do-registration (#%variable-reference) (quote modpath)))))
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
