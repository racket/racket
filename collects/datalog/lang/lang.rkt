#lang scheme/base
(require (for-syntax scheme/base
                     "../private/compiler.ss")
         "../pretty.ss"
         (only-in (planet dherman/pprint:4)
                  pretty-print))

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

(define-syntax (script-begin stx)
  (syntax-case stx ()
    [(script-begin ast ...)
     (compile-module (syntax->datum #'(ast ...)))]))

(define-syntax (interaction-begin stx)
  (syntax-case stx ()
    [(interaction-begin ast)
     (compile-stmt (syntax->datum #'ast))]))

(provide module-begin
         script-begin
         interaction-begin)