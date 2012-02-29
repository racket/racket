#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     "../private/kws.rkt"
                     "../private/rep-data.rkt"
                     "../private/rep.rkt")
         "../private/runtime.rkt")
(provide define-syntax-class/specialize)

(define-syntax (define-syntax-class/specialize stx)
  (syntax-case stx ()
    [(dscs header sc-expr)
     (let-values ([(name formals arity)
                   (let ([p (check-stxclass-header #'header stx)])
                     (values (car p) (cadr p) (caddr p)))]
                  [(target-scname argu)
                   (let ([p (check-stxclass-application #'sc-expr stx)])
                     (values (car p) (cdr p)))])
       (let* ([pos-count (length (arguments-pargs argu))]
              [kws (arguments-kws argu)]
              [target (get-stxclass/check-arity target-scname target-scname pos-count kws)])
         (with-syntax ([name name]
                       [formals formals]
                       [parser (generate-temporary (format-symbol "parser-~a" #'name))]
                       [splicing? (stxclass-splicing? target)]
                       [arity arity]
                       [attrs (stxclass-attrs target)]
                       [options (stxclass-options target)]
                       [target-parser (stxclass-parser target)]
                       [argu argu])
           #`(begin (define-syntax name
                      (stxclass 'name 'arity 'attrs
                                (quote-syntax parser)
                                'splicing?
                                options
                                #f))
                    (define-values (parser)
                      (lambda (x cx pr es fh0 cp0 rl success . formals)
                        (app-argu target-parser x cx pr es fh0 cp0 rl success argu)))))))]))
