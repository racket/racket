#lang typed/racket/base

(require (for-syntax racket/base racket/syntax syntax/strip-context))

(provide req/prov-uniform-collection)

(define-syntax (req/prov-uniform-collection stx)
  (syntax-case stx ()
    [(_ module collection type)
     (with-syntax ([require-it-name  (datum->syntax stx (gensym 'require-it))])
       (syntax/loc stx
         (begin
           (define-syntax (require-it-name stx1)
             (syntax-case stx1 ()
               [(require-it-name)
                (with-syntax ([(obj (... ...))  (replace-context #'require-it-name collection)])
                  #'(begin (require/typed module [obj  type] (... ...))
                           (provide obj (... ...))))]))
           (require-it-name))))]))
