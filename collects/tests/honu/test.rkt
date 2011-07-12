#lang racket/base

(require 
  "test1.rkt"
  (prefix-in macro_ "macro2.rkt")
  (rename-in "literals.rkt"
             [honu-= =]
             [semicolon |;|])
  (rename-in (only-in "honu-typed-scheme.rkt" honu-var)
             [honu-var var])
  (for-syntax racket/base
              "test1.rkt"
              "macro2.rkt"
              syntax/stx
              racket/port
              syntax/parse
              (prefix-in parse: "parse2.rkt"))
  racket/port)

(define-syntax (fake-module-begin stx)
  (syntax-case stx ()
    [(_ stuff)
     (let ()
       (define output (parse:parse (stx-cdr #'stuff)))
       (printf "Output: ~a\n" (syntax->datum output))
       output)]))

#;
(fake-module-begin #hx(macro_macro foo (){ x:number }{
                            withSyntax [z 5]{
                               syntax(print(x); print(z););
                            }
                         }
                         foo 5))

(fake-module-begin #hx(var x = 2;
                       print(x)))
