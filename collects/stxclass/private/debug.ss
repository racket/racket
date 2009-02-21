
#lang scheme/base
(require (for-syntax scheme/base)
         (for-syntax "codegen.ss"))

(provide announce-parse-failures)

(define-syntax (announce-parse-failures stx)
  (syntax-case stx ()
    [(_ b)
     (begin (announce-failures? (and (syntax-e #'b) #t))
            #'(void))]
    [(_)
     #'(announce-failures #t)]))

