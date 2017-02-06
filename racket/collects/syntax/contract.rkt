#lang racket/base
(require racket/contract/base
         (for-template racket/base
                       racket/contract/base
                       syntax/location)
         syntax/srcloc
         syntax/modcollapse
         racket/syntax)

(provide/contract
 [wrap-expr/c
  (->* (syntax? syntax?)
       (#:positive (or/c syntax? string? module-path-index?
                         'from-macro 'use-site 'unknown)
        #:negative (or/c syntax? string? module-path-index?
                         'from-macro 'use-site 'unknown)
        #:name (or/c identifier? symbol? string? #f)
        #:macro (or/c identifier? symbol? string? #f)
        #:context (or/c syntax? #f))
       syntax?)])

(module macro-arg/c racket/base
  (require racket/contract/base
           racket/contract/combinator)

  (provide macro-arg/c)

  (define (macro-arg/c macro-name ctc)
    (let ([ctc-project (get/build-late-neg-projection (coerce-contract 'wrap-expr/c ctc))])
      ((cond [(flat-contract? ctc) make-flat-contract]
             [(chaperone-contract? ctc) make-chaperone-contract]
             [else make-contract])
       #:name (contract-name ctc)
       #:first-order (contract-first-order ctc)
       #:late-neg-projection
       (λ (blame)
         (let ([blame* (if macro-name (blame-add-context blame #f #:important macro-name) blame)])
           (ctc-project (blame-swap blame*))))
       #:list-contract? (list-contract? ctc)))))

(require (for-template 'macro-arg/c))

(define (wrap-expr/c ctc-expr expr
                     #:positive [pos-source 'use-site]
                     #:negative [neg-source 'from-macro]
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
                   [_ #f])]
                [else #f])])
    (base-wrap-expr/c expr #`(macro-arg/c '#,macro-name #,ctc-expr)
                      #:positive pos-source-expr
                      #:negative neg-source-expr
                      #:expr-name (cond [(and macro-name expr-name)
                                         (format "~a of ~a" expr-name macro-name)]
                                        [(or macro-name expr-name)
                                         => (λ (name) (format "~a" name))]
                                        [else #f])
                      #:source #`(quote-syntax #,expr))))

(define (base-wrap-expr/c expr ctc-expr
                          #:positive positive
                          #:negative negative
                          #:expr-name expr-name
                          #:source source)
  (let ([expr-name (or expr-name #'#f)]
        [source (or source #'#f)])
    (quasisyntax/loc expr
      (contract #,ctc-expr
                #,expr
                #,negative
                #,positive
                #,expr-name
                #,source))))

(define (get-source-expr source ctx)
  (cond [(eq? source 'use-site)
         #'(quote-module-path)]
        [(eq? source 'unknown)
         #'(quote "unknown")]
        [(eq? source 'from-macro)
         (if (syntax? ctx)
             (get-source-expr (extract-source ctx) #f)
             (get-source-expr 'unknown #f))]
        [(string? source) #`(quote #,source)]
        [(syntax? source) #`(quote #,(source-location->string source))]
        [(module-path-index? source)
         ;; FIXME: extend collapse-module-path-index to accept #f, return rel mod path
         (let* ([here (current-load-relative-directory)]
                [collapsed
                 (collapse-module-path-index source (or here (build-path 'same)))])
           (cond [(and (path? collapsed) here)
                  #`(quote #,collapsed)]
                 [(path? collapsed)
                  (let-values ([(rel base) (module-path-index-split source)])
                    #`(quote #,rel))]
                 [else
                  #`(quote #,(format "~s" collapsed))]))]))

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
