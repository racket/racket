#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     racket/syntax
                     "private/rep-data.rkt"
                     "private/rep.rkt"
                     "private/kws.rkt")
         "../parse.rkt"
         syntax/parse/private/residual
         "private/runtime.rkt"
         "private/runtime-progress.rkt"
         "private/runtime-report.rkt"
         "private/kws.rkt")

;; No lazy loading for this module's dependencies.

(provide syntax-class-parse
         syntax-class-attributes
         syntax-class-arity
         syntax-class-keywords

         debug-rhs
         debug-pattern
         debug-parse)

(define-syntax (syntax-class-parse stx)
  (syntax-case stx ()
    [(_ s x arg ...)
     (parameterize ((current-syntax-context stx))
       (let* ([argu (parse-argu (syntax->list #'(arg ...)) #:context stx)]
              [stxclass
               (get-stxclass/check-arity #'s stx
                                         (length (arguments-pargs argu))
                                         (arguments-kws argu))]
              [attrs (stxclass-attrs stxclass)])
         (with-syntax ([parser (stxclass-parser stxclass)]
                       [argu argu]
                       [(name ...) (map attr-name attrs)]
                       [(depth ...) (map attr-depth attrs)])
           #'(let ([fh (lambda (fs) fs)])
               (app-argu parser x x (ps-empty x x) #f fh fh #f
                         (lambda (fh . attr-values)
                           (map vector '(name ...) '(depth ...) attr-values))
                         argu)))))]))

(define-syntaxes (syntax-class-attributes
                  syntax-class-arity
                  syntax-class-keywords)
  (let ()
    (define ((mk handler) stx)
      (syntax-case stx ()
        [(_ s)
         (parameterize ((current-syntax-context stx))
           (handler (get-stxclass #'s)))]))
    (values (mk (lambda (s)
                  (let ([attrs (stxclass-attrs s)])
                    (with-syntax ([(a ...) (map attr-name attrs)]
                                  [(d ...) (map attr-depth attrs)])
                      #'(quote ((a d) ...))))))
            (mk (lambda (s)
                  (let ([a (stxclass-arity s)])
                    #`(to-procedure-arity '#,(arity-minpos a) '#,(arity-maxpos a)))))
            (mk (lambda (s)
                  (let ([a (stxclass-arity s)])
                    #`(values '#,(arity-minkws a) '#,(arity-maxkws a))))))))

(define-syntax (debug-rhs stx)
  (syntax-case stx ()
    [(debug-rhs rhs)
     (let ([rhs (parse-rhs #'rhs #f #f #:context stx)])
       #`(quote #,rhs))]))

(define-syntax (debug-pattern stx)
  (syntax-case stx ()
    [(debug-pattern p . rest)
     (let-values ([(rest pattern defs)
                   (parse-pattern+sides #'p #'rest
                                        #:splicing? #f
                                        #:decls (new-declenv null)
                                        #:context stx)])
       (unless (stx-null? rest)
         (raise-syntax-error #f "unexpected terms" stx rest))
       #`(quote ((definitions . #,defs)
                 (pattern #,pattern))))]))

(define-syntax-rule (debug-parse x p ...)
  (let/ec escape
    (parameterize ((current-failure-handler
                    (lambda (_ fs)
                      (escape
                       `(parse-failure
                         #:raw-failures
                         ,(failureset->sexpr fs)
                         #:maximal-failures
                         ,(let ([selected (map (lambda (fs)
                                                 (cons 'equivalence-class
                                                       (map failure->sexpr fs)))
                                               (maximal-failures fs))])
                            (if (= (length selected) 1)
                                (car selected)
                                (cons 'union selected))))))))
      (syntax-parse x [p 'success] ...))))
