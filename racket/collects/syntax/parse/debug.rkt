#lang racket/base
(require (for-syntax racket/base
                     syntax/stx
                     racket/syntax
                     "private/rep-data.rkt"
                     "private/rep.rkt"
                     "private/kws.rkt")
         racket/list
         racket/pretty
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
         debug-parse
         debug-syntax-parse!)

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
                      (define-values (raw-fs-sexpr maximal-fs-sexpr) (fs->sexprs fs))
                      (escape
                       `(parse-failure
                         #:raw-failures
                         ,raw-fs-sexpr
                         #:maximal-failures
                         ,maximal-fs-sexpr)))))
      (syntax-parse x [p 'success] ...))))

(define (fs->sexprs fs)
  (let* ([raw-fs (map invert-failure (reverse (flatten fs)))]
         [selected-groups (maximal-failures raw-fs)])
    (values (failureset->sexpr raw-fs)
            (let ([selected (map (lambda (fs)
                                   (cons 'progress-class
                                         (map failure->sexpr fs)))
                                 selected-groups)])
              (if (= (length selected) 1)
                  (car selected)
                  (cons 'union selected))))))

(define (debug-syntax-parse!)
  (define old-failure-handler (current-failure-handler))
  (current-failure-handler
   (lambda (ctx fs)
     (define-values (raw-fs-sexpr maximal-fs-sexpr) (fs->sexprs fs))
     (eprintf "*** syntax-parse debug info ***\n")
     (eprintf "Raw failures:\n")
     (pretty-write raw-fs-sexpr (current-error-port))
     (eprintf "Maximal failures:\n")
     (pretty-write maximal-fs-sexpr (current-error-port))
     (old-failure-handler ctx fs))))
