#lang racket/base
(require (for-syntax racket/base
                     "../private/compiler.rkt")
         "../pretty.rkt")

(define (print-result value)
  (if (void? value)
      (void)
      (displayln (format-literals value))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(module-begin)
     #'(#%module-begin)]
    [(module-begin ast ...)
     (with-syntax ([(begin rs ...) (compile-module (syntax->datum #'(ast ...)))])
       #'(#%module-begin
          (begin (print-result rs) ...)))]))

(define-syntax (top-interaction stx)
  (syntax-case stx ()
    [(_ . stmt)
     (quasisyntax/loc stx
       (print-result 
        #,(compile-stmt (syntax->datum #'stmt))))]))

(provide module-begin
         top-interaction)