#lang racket/base

(require 
  (prefix-in macro_ honu/core/private/macro2)
  (rename-in honu/core/private/literals
             [honu-= =]
             [semicolon |;|])
  (rename-in (only-in honu/core/private/honu-typed-scheme honu-var)
             [honu-var var])
  (for-syntax racket/base
              honu/core/private/macro2
              syntax/stx
              racket/port
              syntax/parse
              (prefix-in parse: honu/core/private/parse2))
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
