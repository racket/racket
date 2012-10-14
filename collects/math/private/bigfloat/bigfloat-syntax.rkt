#lang racket/base

(require (only-in "mpfr.rkt" consts 0ary-funs)
         "bigfloat-constants.rkt"
         racket/promise
         (for-syntax racket/base racket/syntax syntax/strip-context))

(define-syntax (req/prov-constants stx)
  (syntax-case stx ()
    [(_ module collection force)
     (with-syntax ([require-it-name  (datum->syntax stx (gensym 'require-it))])
       (syntax/loc stx
         (begin
           (define-syntax (require-it-name stx1)
             (syntax-case stx1 ()
               [(require-it-name)
                (with-syntax* ([(name (... ...))  (replace-context #'require-it-name collection)]
                               [(stx-name (... ...))  (map (λ (name) (format-id name "stx:~a" name))
                                                           (syntax->list #'(name (... ...))))])
                  #'(begin (require (only-in module name (... ...)))
                           (define-syntax (stx-name stx)
                             (syntax-case stx ()
                               [(_ . args)  (syntax/loc stx ((force name) . args))]
                               [_  (syntax/loc stx (force name))]))
                           (... ...)
                           (provide (rename-out [stx-name name] (... ...)))))]))
           (require-it-name))))]))

(define-syntax-rule (apply0 x) (x))

(req/prov-constants "bigfloat-constants.rkt" consts force)
(req/prov-constants "bigfloat-constants.rkt" 0ary-funs apply0)
