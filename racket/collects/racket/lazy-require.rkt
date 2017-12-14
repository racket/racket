#lang racket/base
(require (for-syntax racket/base)
         compiler/cm-accomplice
         racket/runtime-path
         racket/promise)
(provide lazy-require
         lazy-require-syntax)

(define-syntax (lazy-require stx)
  (syntax-case stx ()
    [(lazy-require [modpath (import ...)] ...)
     #`(begin (lazy-require1 modpath (import ...) #,stx) ...)]))

(begin-for-syntax

 (define counter 0)

 (define (gen-aux-mod-name phase)
   (begin0 (string->symbol (format "lazy-require-aux-~a-~a" phase counter))
     (set! counter (add1 counter))))

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
            modpath])))

 (define (process-imports ctx imports)
   (for/list ([import (in-list imports)])
     (syntax-case import ()
       [name
        (identifier? #'name)
        (list #'name #'name)]
       [[exp-name bind-name]
        (begin
          (unless (identifier? #'exp-name)
            (raise-syntax-error #f "expected identifier" #'orig-stx #'exp-name))
          (unless (identifier? #'bind-name)
            (raise-syntax-error #f "expected identifier" #'orig-stx #'bind-name))
          (list #'exp-name #'bind-name))]
       [bad
        (raise-syntax-error #f "expected identifier or pair of identifiers"
                            #'orig-stx #'bad)])))
 )

(define-syntax (lazy-require1 stx)
  (syntax-case stx ()
    [(lazy-require1 modpath (import ...) orig-stx)
     (with-syntax ([((exp-name bind-name) ...)
                    (process-imports #'orig-stx (syntax->list #'(import ...)))]
                   [mpi-var-defn
                    (let ([phase (sub1 (variable-reference->phase (#%variable-reference)))])
                      (if (zero? phase)
                          ;; `define-runtime-module-path-index' works right at phase-level 0:
                          #'(define-runtime-module-path-index mpi-var (quote modpath))
                          ;; need a submodule:
                          (with-syntax ([lazy-require-path-n (gen-aux-mod-name phase)]
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
                                (require (submod "." lazy-require-path-n))))))])
       (with-syntax ([(aux-name ...) (generate-temporaries #'(bind-name ...))])
         #'(begin 
             mpi-var-defn
             (define (get-sym sym)
               (parameterize ((current-namespace (variable-reference->namespace (#%variable-reference))))
                 (begin0
                     (dynamic-require mpi-var sym)
                   (do-registration (#%variable-reference) (quote modpath)))))
             (define aux-name (make-lazy-function 'exp-name 'bind-name get-sym)) ...
             (define-syntax bind-name
               (make-rename-transformer
                (syntax-property (quote-syntax aux-name)
                                 'not-provide-all-defined #t))) ...)))]))

(define (make-lazy-function exp-name bind-name get-sym)
  ;; Use 'delay/sync' because 'delay' promise is not reentrant.
  ;; FIXME: OTOH, 'delay/sync' promise is not kill-safe.
  (let ([fun-p (delay/sync (get-sym exp-name))])
    (procedure-rename
     (make-keyword-procedure
      (lambda (kws kwargs . args)
        (keyword-apply (force fun-p) kws kwargs args)))
     bind-name)))

(define (do-registration vr modpath)
  (let ([path (resolved-module-path-name
               (module-path-index-resolve
                (module-path-index-join
                 modpath
                 (variable-reference->resolved-module-path vr))))])
    (when (path? path)
      (register-external-module path #:indirect? #t))))

;; ----

(define-syntax (lazy-require-syntax stx)
  (syntax-case stx ()
    [(_ [modpath (import ...)] ...)
     #`(begin (lazy-require-syntax1/rename modpath (import ...) #,stx) ...)]))

#|
;; Implementation 1: syntax-local-value to get macro transformer

;; This version only works for "standard" macros. It doesn't work with
;; set!-transformers. It only supports the macro-nature of syntax
;; bindings like struct names (you can use them as constructors, but not
;; as match patterns).

(define-syntax (lazy-require-syntax1/transformer stx)
  (syntax-case stx ()
    [(_ modpath (import ...) orig-stx)
     (with-syntax ([modpath* (module-path-add-submod-up #'modpath)]
                   [((exp-name bind-name) ...)
                    (process-imports #'orig-stx (syntax->list #'(import ...)))]
                   [aux-mod1 (gen-aux-mod-name '*)]
                   [aux-mod2 (gen-aux-mod-name '*)])
       (with-syntax ([(get-tx ...) (generate-temporaries #'(exp-name ...))])
         #'(begin
             (module aux-mod1 racket/base
               (require (only-in (for-template modpath*) exp-name ...))
               (define (get-tx) (syntax-local-value (quote-syntax exp-name))) ...
               (provide get-tx ...))
             (module aux-mod2 racket/base
               (require (only-in racket/lazy-require lazy-require))
               (lazy-require [(submod ".." aux-mod1) (get-tx ...)])
               (provide get-tx ...))
             (require (for-syntax (only-in (submod "." aux-mod2) [get-tx get-tx] ...)))
             (define-syntax (bind-name stx) ((get-tx) stx)) ...)))]))
|#

;; Implementation 2: lazy rename-transformers

;; This version is more flexible, since rename-transformers support
;; ordinary variables and syntax bindings other than or more than macros
;; (eg struct information).

(begin-for-syntax
  (struct lazy-rename-transformer (get-id)
    #:property prop:rename-transformer
    (lambda (self)
      (syntax-property ((lazy-rename-transformer-get-id self))
                       'not-free-identifier=? #t))))

(define-syntax (lazy-require-syntax1/rename stx)
  (syntax-case stx ()
    [(_ modpath (import ...) orig-stx)
     (with-syntax ([modpath* (module-path-add-submod-up #'modpath)]
                   [((exp-name bind-name) ...)
                    (process-imports #'orig-stx (syntax->list #'(import ...)))]
                   [aux-mod1 (gen-aux-mod-name '*)]
                   [aux-mod2 (gen-aux-mod-name '*)])
       (with-syntax ([(get-id ...) (generate-temporaries #'(exp-name ...))]
                     [(bind-aux ...) (generate-temporaries #'(bind-name ...))])
         #'(begin
             (module aux-mod1 racket/base
               (#%require (for-template (only modpath* exp-name ...)))
               (define (get-id) (quote-syntax exp-name)) ...
               (provide get-id ...))
             (module aux-mod2 racket/base
               (require (only-in racket/lazy-require lazy-require))
               (lazy-require [(submod ".." aux-mod1) (get-id ...)])
               (provide get-id ...))
             (require (for-syntax (only-in (submod "." aux-mod2) [get-id get-id] ...)))
             ;; Use extra indirection (bind-name -> bind-aux) so that (provide bind-name)
             ;; doesn't force (get-id) trying to decide whether to bypass renamer.
             (define-syntax bind-aux (lazy-rename-transformer get-id)) ...
             (define-syntax bind-name
               (make-rename-transformer
                (syntax-property (quote-syntax bind-aux) 'not-free-identifier=? #t)))
             ...)))]))
