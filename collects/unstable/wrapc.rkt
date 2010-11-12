#lang racket/base
(require racket/contract/base
         (for-template racket/base
                       racket/contract/base
                       unstable/location)
         unstable/srcloc
         unstable/syntax)

(provide/contract
 [wrap-expr/c
  (->* (syntax? syntax?)
       (#:positive (or/c syntax? string? module-path-index?
                         'from-macro 'use-site 'unknown)
        #:negative (or/c syntax? string? module-path-index?
                         'from-macro 'same-as-use-site 'unknown)
        #:name (or/c identifier? symbol? string? #f)
        #:macro (or/c identifier? symbol? string? #f)
        #:context (or/c syntax? #f))
       syntax?)])

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
         [macro-name
          (cond [(identifier? macro-name) (syntax-e macro-name)]
                [(or (string? macro-name) (symbol? macro-name)) macro-name]
                [(syntax? ctx)
                 (syntax-case ctx ()
                   [(x . _) (identifier? #'x) (syntax-e #'x)]
                   [x (identifier? #'#'x)]
                   [_ #f])]
                [else #f])])
    (base-wrap-expr/c expr ctc-expr
                      #:positive #'(quote-module-path)
                      #:negative neg-source-expr
                      #:expr-name (cond [(and expr-name macro-name)
                                          (format "~a of ~a" expr-name macro-name)]
                                         [expr-name expr-name]
                                         [else #f])
                      #:source #`(quote-syntax #,expr))))

(define (base-wrap-expr/c expr ctc-expr
                          #:positive positive
                          #:negative negative
                          #:expr-name [expr-name #f]
                          #:source [source #f])
  (let ([expr-name (or expr-name #'#f)]
        [source (or source #'#f)])
    #`(contract #,ctc-expr
                #,expr
                #,positive
                #,negative
                #,expr-name
                #,source)))

(define (get-source-expr source ctx)
  (cond [(eq? source 'use-site)
         #'(quote-module-path)]
        [(eq? source 'unknown)
         #'(quote "unknown")]
        [(eq? source 'from-macro)
         (if (syntax? ctx)
             (get-source-expr (extract-source ctx) #f)
             (get-source-expr 'unknown #f))]
        [else
         (let ([source-string
                (cond [(string? source) source]
                      [(syntax? source) (source-location->string source)]
                      [(module-path-index? source)
                       ;; FIXME: share with unstable/location ??
                       (let ([name (resolved-module-path-name
                                    (module-path-index-resolve source))])
                         (cond [(path? name) (format "(file ~s)" (path->string name))]
                               [(symbol? name) (format "(quote ~s)" name)]))])])
           #`(quote #,source-string))]))

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
