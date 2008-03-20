#lang scheme/base

(require scheme/require-syntax
         (for-syntax scheme/base)
         (for-syntax "private/short-syntax-helpers.ss"))

(provide (rename-out [plan planet]))

;; SPEC     ::= OWNER "/" PACKAGE VERSPEC PATHSPEC
;; VERSPEC  ::= "" | "/" MAJ MINSPEC
;; MINSPEC  ::= "" | "/" PMINSPEC 
;; PMINSPEC ::= MIN | ">=" MIN | "<=" MIN | MIN "-" MIN

(define-require-syntax plan
  (λ (stx)
    (syntax-case stx ()
      [(_ spec-sym)
       (symbol? (syntax->datum #'spec-sym))
       (let ([str (symbol->string (syntax->datum #'spec-sym))])
         (define (yell msg) (λ (str) (raise-syntax-error #f (format msg str) #'spec-sym))) 
         (try-parsing str
          ([owner   (get-next-slash #:on-error (yell "Illegal syntax; expected an owner, received ~e"))]
           [package (get-next-slash #:on-error (yell "Illegal syntax; expected a package, received ~e"))])
          (λ (final-path)
            (let-values ([(pkg maj min) (parse-package package stx)])
              (quasisyntax/loc stx 
                (planet #,final-path (#,owner 
                                      #,pkg
                                      #,@(if maj (list maj) '())
                                      #,@(if min (list min) '()))))))))]
      [(_ . any)
       #`(planet . any)])))
          
              
              
              
         
         
         
         
       