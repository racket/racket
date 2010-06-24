#lang racket/base
(require (for-syntax racket/base
                     "../private/compiler.rkt")
         "../pretty.rkt"
         "../private/pprint.rkt")

(define (print-result value)
  (if (void? value)
      (void)
      (pretty-print (format-literals value))))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(module-begin)
     #'(#%module-begin)]
    [(module-begin ast ...)
     (with-syntax ([(begin rs ...) (compile-module (syntax->datum #'(ast ...)))])
       #'(#%module-begin
          (begin (print-result rs) ...)))]))

#;(compile-module (syntax->datum #'(ast ...)))
#;(compile-stmt (syntax->datum #'ast))

(define-syntax (top-interaction stx)
  (printf "~S\n" stx)
  #'(void))

(provide module-begin
         top-interaction)