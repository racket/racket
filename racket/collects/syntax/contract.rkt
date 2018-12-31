#lang racket/base
(require racket/contract/base
         (for-template racket/base
                       syntax/location)
         syntax/srcloc
         syntax/modcollapse
         racket/syntax)

(provide/contract
 [wrap-expr/c
  (->* (syntax? syntax?)
       (#:arg? any/c
        #:positive (or/c syntax? string? module-path-index?
                         'from-macro 'use-site 'unknown)
        #:negative (or/c syntax? string? module-path-index?
                         'from-macro 'use-site 'unknown)
        #:name (or/c identifier? symbol? string? #f)
        #:macro (or/c identifier? symbol? string? #f)
        #:context (or/c syntax? #f))
       syntax?)])

(module runtime racket/base
  (require (for-syntax racket/base
                       syntax/free-vars)
           racket/contract/base
           racket/contract/combinator
           (only-in racket/contract/private/base
                    make-apply-contract))
  (provide expr/contract
           relative-source)

  (define (macro-expr/c arg? expr-name ctc0)
    (define ctc (coerce-contract 'wrap-expr/c ctc0))
    (define proj (get/build-late-neg-projection ctc))
    (make-contract
     #:name (unquoted-printing-string
             (format "macro ~a contract~a~a"
                     (if arg? "argument" "result")
                     (if expr-name " on " "")
                     (if expr-name expr-name "")))
     #:first-order (contract-first-order ctc)
     #:late-neg-projection
     (λ (blame)
       (define blame* (blame-add-context blame (format "~s" (contract-name ctc)) #:swap? arg?))
       (proj (blame-swap blame)))
     #:list-contract? (list-contract? ctc)))

  (define (macro-dep-expr/c arg? expr-name)
    (make-contract
     #:name (unquoted-printing-string
             (format "macro ~a contract~a~a"
                     (if arg? "argument" "result")
                     (if expr-name " on " "")
                     (if expr-name expr-name "")))
     #:late-neg-projection
     (lambda (blame)
       (lambda (_f neg)
         ;; Note: specialized to _f = return-second-arg.
         (lambda (c v)
           (define (slow-path)
             (define ctc (coerce-contract 'wrap-expr/c c))
             (define proj (get/build-late-neg-projection ctc))
             (define blame*
               (blame-add-context blame (format "~s" (contract-name ctc)) #:swap? arg?))
             ((proj blame*) v neg))
           (cond [(flat-contract? c)
                  (let ([c (if (procedure? c) c (coerce-contract 'wrap-expr/c c))])
                    (if (c v) v (slow-path)))]
                 [else (slow-path)]))))))

  (define (return-second-arg c v) v)

  (begin-for-syntax
    (define (okay-to-lift? ee)
      (and (identifier? ee) (not (local-free-vars? ee))))
    (define (self-module-path-index? mpi)
      (define-values (rel base) (module-path-index-split mpi))
      (and (eq? rel #f) (eq? (module-path-index-submodule mpi) #f)))
    (define (local-free-vars? ee)
      (for/or ([fv (in-list (free-vars ee #:module-bound? #t))])
        (define b (identifier-binding fv))
        (cond [(list? b) (self-module-path-index? (car b))]
              [else #t]))))

  (define-syntax (expr/contract stx)
    (cond
      [(eq? (syntax-local-context) 'expression)
       (syntax-case stx ()
         [(_ val-expr ctc-expr arg? expr-name [mac-arg ...])
          (let ([ctc-ee (local-expand #'ctc-expr 'expression null)])
            (cond [(okay-to-lift? ctc-ee)
                   #`(#,(syntax-local-lift-expression
                         #`(make-apply-contract
                            (macro-expr/c arg? expr-name #,ctc-ee)
                            mac-arg ...))
                      val-expr)]
                  [else
                   #`(#,(syntax-local-lift-expression
                         #`((make-apply-contract
                             (macro-dep-expr/c arg? expr-name)
                             mac-arg ...)
                            return-second-arg))
                      #,ctc-ee
                      val-expr)]))])]
      [else #`(#%expression #,stx)]))

  (define (relative-source base-mpi rel-mod-path)
    (define r
      (resolved-module-path-name
       (module-path-index-resolve
        (module-path-index-join rel-mod-path base-mpi))))
    (cond [(pair? r)
           (cons 'submod r)]
          [(symbol? r)
           (list 'quote r)]
          [else r])))
(require (for-template (submod "." runtime)))

(define (wrap-expr/c ctc-expr expr
                     #:arg? [arg? #t]
                     #:positive [pos-source 'from-macro]
                     #:negative [neg-source 'use-site]
                     #:name [expr-name #f]
                     #:macro [macro-name #f]
                     #:context [ctx (current-syntax-context)])
  (let* ([pos-source-expr
          (get-source-expr pos-source
                           (if (identifier? macro-name) macro-name ctx))]
         [neg-source-expr
          (get-source-expr neg-source
                           (if (identifier? macro-name) macro-name ctx))]
         [expr-name
          (if (identifier? expr-name)
              (syntax-e expr-name)
              expr-name)]
         [macro-name
          (cond [(identifier? macro-name) (syntax-e macro-name)]
                [(or (string? macro-name) (symbol? macro-name)) macro-name]
                [(syntax? ctx)
                 (syntax-case ctx ()
                   [(x . _) (identifier? #'x) (syntax-e #'x)]
                   [x (identifier? #'x) (syntax-e #'x)]
                   [_ '?])]
                [else '?])])
    #`(expr/contract #,expr #,ctc-expr #,arg? '#,expr-name
                     [#,pos-source-expr
                      #,neg-source-expr
                      '#,macro-name
                      (quote-syntax #,expr)
                      #f])))

(define (get-source-expr source ctx)
  (cond [(eq? source 'use-site)
         #'(quote-module-path)]
        [(eq? source 'unknown)
         #'(quote "unknown")]
        [(eq? source 'from-macro)
         (get-source-expr (extract-source ctx) #f)]
        [(string? source) #`(quote #,source)]
        [(syntax? source) #`(quote #,(source-location->string source))]
        [(module-path-index? source)
         ;; FIXME: This assumes that if source is relative, it is relative to
         ;; the current self-index (the module currently being compiled). That
         ;; should usually be the case, but it's not necessarily true.
         (define collapsed (collapse-module-path-index source))
         (cond [(eq? collapsed #f)
                #'(quote-module-path)]
               [(relative-module-path? collapsed)
                #`(relative-source (variable-reference->module-path-index
                                    (#%variable-reference))
                                   '#,collapsed)]
               [else #`(quote #,collapsed)])]))

(define (relative-module-path? mp)
  (or (string? mp) (path? mp)
      (and (pair? mp) (eq? (car mp) 'submod)
           (let ([base (cadr mp)]) (or (string? base) (path? base))))))

;; extract-source : (U Syntax #f) -> (U ModulePathIndex 'use-site 'unknown)
(define (extract-source stx)
  (let ([id (syntax-case stx ()
              [(x . _) (identifier? #'x) #'x]
              [x (identifier? #'x) #'x]
              [_ #f])])
    (if id
        (let ([b (identifier-binding id)])
          (cond [(list? b) (car b)] ;; module-path-index
                [else 'use-site]))
        'unknown)))
