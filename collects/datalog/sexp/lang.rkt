#lang racket
(require (for-syntax syntax/parse)
         datalog/stx
         datalog/runtime)

(define lang-theory (make-theory))

(define-syntax module-begin
  (syntax-rules ()
    [(_ stmt ...)
     (#%module-begin 
      (datalog! lang-theory stmt ...))]))

(define-syntax top-interaction
  (syntax-rules ()
    [(_ . stmt)
     (datalog! lang-theory stmt)]))

(provide (rename-out [top-interaction #%top-interaction]
                     [module-begin #%module-begin])
         ! ~ ? :-)