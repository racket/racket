#lang racket/base
(require racket/contract/base
         syntax/srcloc
         syntax/modcollapse
         racket/syntax
         syntax/location)

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
        #:context (or/c syntax? #f)
        #:phase exact-integer?)
       syntax?)])

(module runtime racket/base
  (require (for-syntax racket/base
                       syntax/free-vars)
           racket/contract/base
           racket/contract/combinator
           (only-in racket/contract/private/base
                    make-apply-contract)
           syntax/location)
  (provide (all-from-out racket/base
                         syntax/location)
           expr/contract
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
       (proj blame*))
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

  (define (relative-source base-mpi rel-mod-paths)
    (define r
      (resolved-module-path-name
       (module-path-index-resolve
        (let loop ([rel-mod-paths rel-mod-paths])
          (module-path-index-join (car rel-mod-paths)
                                  (if (null? (cdr rel-mod-paths))
                                      base-mpi
                                      (loop (cdr rel-mod-paths))))))))
    (cond [(pair? r)
           (cons 'submod r)]
          [(symbol? r)
           (list 'quote r)]
          [else r])))

;; Allow phase shift of 0 or 1 without needing to lift requires
(require (for-template (submod "." runtime))
         ;; for phase +1 uses, only need to instantiate, since we’ll shift
         (only-in (submod "." runtime)))

(define (wrap-expr/c ctc-expr expr
                     #:arg? [arg? #t]
                     #:positive [pos-source 'from-macro]
                     #:negative [neg-source 'use-site]
                     #:name [expr-name #f]
                     #:macro [macro-name #f]
                     #:context [ctx (current-syntax-context)]
                     #:phase [phase (syntax-local-phase-level)])
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
                [else '?])]
         [introduce (make-syntax-introducer)]
         [phase-shift (- phase (syntax-local-phase-level))]
         [shift+introduce (lambda (stx) (introduce (syntax-shift-phase-level stx phase-shift)))]
         [unshift+introduce (lambda (stx) (introduce (syntax-shift-phase-level stx (- phase-shift))))]
         [expr+ctc (shift+introduce
                    #`(expr/contract #,(unshift+introduce expr) #,(unshift+introduce ctc-expr)
                                     '#,(and arg? #t) '#,expr-name
                                     [#,pos-source-expr
                                      #,neg-source-expr
                                      '#,macro-name
                                      (quote-syntax #,expr)
                                      #f]))])
    (cond
      ;; no need to lift for common phases, since we explicitly require them in this module
      [(memq phase-shift '(0 1))
       expr+ctc]
      [else
       (unless (syntax-transforming?)
         (raise-arguments-error 'wrap-expr/c "not currently expanding"))
       (define phased-require-spec
         (introduce (datum->syntax #'here `(for-meta ,phase-shift ,(quote-module-path runtime)))))
       (syntax-local-introduce (syntax-local-lift-require (syntax-local-introduce phased-require-spec)
                                                          (syntax-local-introduce expr+ctc)))])))

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
                ;; Instead of using `collapsed`, use the original steps in `source`,
                ;; because an executable created by `raco exe` may need the individual
                ;; steps to find the right module (i.e., it resolves through relative
                ;; references).
                #`(relative-source (variable-reference->module-path-index
                                    (#%variable-reference))
                                   '#,(let loop ([mp source])
                                        (cond
                                          [(or (not mp) (self-mpi? mp)) '()]
                                          [else
                                           (define-values (name base) (module-path-index-split mp))
                                           (cons name (loop base))])))]
               [else #`(quote #,collapsed)])]))

(define (relative-module-path? mp)
  (or (string? mp) (path? mp)
      (and (pair? mp) (eq? (car mp) 'submod)
           (let ([base (cadr mp)]) (or (string? base) (path? base))))))

(define (self-mpi? mpi)
  (define-values (base name) (module-path-index-split mpi))
  (and (not base) (not name)))

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
