#lang racket/base
(require (for-syntax syntax/parse
                     racket/list
                     racket/base)
         datalog/stx
         datalog/runtime)

(define lang-theory (make-theory))

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
     (with-syntax ([((requires ...)
                     (stmt ...))
                    (partition-requires #'es)])
       (syntax/loc stx
         (#%module-begin 
          requires ...
          (datalog! lang-theory stmt ...))))]))

(define-syntax top-interaction
  (syntax-rules ()
    [(_ . stmt)
     (datalog! lang-theory stmt)]))

(provide (rename-out [top-interaction #%top-interaction]
                     [module-begin #%module-begin])
         (except-out (all-from-out racket/base)
                     #%top-interaction
                     #%module-begin)
         ! ~ ? :-)