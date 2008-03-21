#lang scheme/base
#|
provides a shorthand syntax for planet files analagous to the (require net/url) syntax
for libraries.

Grammar:

SPEC        ::= OWNER "/" PKGNAME MAJVERSPEC "/" PATH
MAJVERSPEC  ::= "" | ":" [0-9]+ MINVERSPEC
MINVERSPEC  ::= "" | ":" PMINVERSPEC
PMINVERSPEC ::= [0-9]+ | "<=" [0-9]+ | ">=" [0-9]+ | "=[0-9]+" | [0-9]+ "-" [0-9]+
OWNER       ::= [string without /]
PKGNAME     ::= [string without /, :]
PATH        ::= string 

Examples:

(require (planet planet/test-connection.plt/test-connection.ss))
(require (planet planet/test-connection/test-connection.ss))
(require (planet planet/test-connection:1/test-connection.ss))
(require (planet planet/test-connection:1:0/test-connection.ss))
(require (planet planet/test-connection:1:=0/test-connection.ss))
(require (planet planet/test-connection:1:0-10/test-connection.ss))
(require (planet planet/test-connection:1:>=0/test-connection.ss))
(require (planet planet/test-connection:1:<=0/test-connection.ss))

|#

(require scheme/require-syntax
         (for-syntax scheme/base)
         (for-syntax "private/short-syntax-helpers.ss"))

(provide (rename-out [plan planet]))



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
