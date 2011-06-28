#lang racket/base
(require (for-syntax syntax/parse
                     racket/list
                     racket/base)
         racket/contract
         datalog/stx
         datalog/runtime)

(define-for-syntax (partition-requires es)
  (define-values (rs stmts)
    (partition 
     (λ (e-stx)
       (syntax-parse
        e-stx
        #:literals (require)
        [(require . r)
         #t]
        [_
         #f]))
     (syntax->list es)))
  (list rs stmts))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ . es)
     (with-syntax ([theory (datum->syntax #'es 'theory)]
                   [((requires ...)
                     (stmt ...))
                    (partition-requires #'es)])
       (syntax/loc stx
         (#%module-begin 
          requires ...
          (define theory (make-theory))
          (datalog! theory stmt ...)
          (provide/contract
           [theory theory/c]))))]))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . stmt)
     (with-syntax ([theory (datum->syntax #'stmt 'theory)])
       (syntax/loc stx
         (datalog! theory stmt)))]))

(provide (rename-out [top-interaction #%top-interaction]
                     [module-begin #%module-begin])
         (except-out (all-from-out racket/base)
                     #%top-interaction
                     #%module-begin)
         ! ~ ? :-)
