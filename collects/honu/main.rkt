#lang scheme/base

(require scheme/class)

(require "private/honu-typed-scheme.ss"
         ;; "private/honu.ss"
         "private/parse.ss"
         "private/literals.ss"
         "private/syntax.ss"
         "private/macro.ss")

(define test-x-class
  (class object%
    (init-field tuna)
    (super-new)))

(define x (new test-x-class [tuna 5]))

(provide (rename-out (#%dynamic-honu-module-begin #%module-begin)
                     (honu-top #%top)
                     (semicolon \;
                                )
                     (honu-+ +)
                     (honu-* *)
                     (honu-/ /)
                     (honu-- -)
                     (honu-? ?)
                     (honu-: :)
                     (honu-comma |,|)
                     (honu-. |.|)
                     )
         #%datum
         #%braces
         #%parens
         x
         true
         false
         display
         display2
         newline
         else
         foobar2000
         (rename-out
           (honu-if if)
           (honu-provide provide)
           (honu-macro-item macroItem)
           (honu-macro macro)
           (honu-syntax syntax)
           ))

#;
(provide int real bool obj 
         function var const
         string
         -> >->
         \;
         ? :
         && \|\|
         /
         < > <= >=
         !=
         cons list
         true false
         display write newline
         #%datum
         #%top
         #%parens #%brackets #%braces #%angles
         #%prefix #%postfix
         ;; define-honu-syntax
         ...
         (for-syntax ...)

         (rename-out (set! =)
                     (honu-return return)
                     (honu-if if)
                     (honu-macro macro)
                     (honu-time time)
                     (honu-class class)
                     (honu+ +)
                     (honu- -)
                     (honu* *) 
                     (do do)
                     (honu-end end)
                     (modulo %)
                     (equal? ==)
                     (string->number stringToNumber)
                     (number->string numberToString)
                     (car first) 
                     (cdr rest)
                     (null empty)
                     (null? isEmpty)
                     (pair? isCons)
                     (#%dynamic-honu-module-begin #%module-begin)
                     (honu-#%app #%app)
                     (honu-top #%top-interaction)
                     (honu-provide provide)
                     (honu-require require)))
