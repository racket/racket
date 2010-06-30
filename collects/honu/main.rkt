#lang scheme/base

(require (for-syntax scheme/base))
(require scheme/class)

(require "private/honu-typed-scheme.ss"
         ;; "private/honu.ss"
         "private/parse.ss"
         (for-syntax "private/literals.ss")
         (for-syntax "private/honu-typed-scheme.ss")
         (for-syntax "private/parse.ss")
         syntax/parse
         (for-syntax syntax/parse)
         "private/literals.ss"
         "private/syntax.ss"
         "private/more.ss"
         (for-template scheme/base)
         (for-template "private/literals.rkt")
         (for-syntax "private/more.ss")
         (for-syntax "private/syntax.ss")
         (for-syntax "private/macro.ss")
         "private/macro.ss")

(define test-x-class
  (class object%
    (init-field tuna)
    (super-new)))

(define x (new test-x-class [tuna 5]))

(define (sql1 . x) #f)
(define (sql2) #f)
(define (sql3) #f)
(define (sql4) #f)
(define (sql5) #f)

(provide (rename-out (#%dynamic-honu-module-begin #%module-begin)
                     ;; (honu-top #%top)
                     (semicolon \;
                                )
                     (honu-+ +)
                     (honu-* *)
                     (+ scheme:+)
                     (honu-/ /)
                     (honu-- -)
                     (honu-? ?)
                     (honu-: :)
                     (honu-comma |,|)
                     (honu-. |.|)
                     )

         #%top

         ;; sql nonsense
         (rename-out 
           (sql1 SQL_create_insert)
           (sql2 foo)
           (sql3 cheese)
           (sql4 monkeys)
           (sql5 horse))
         ;; end sql

         #%datum
         #%top-interaction
         (for-syntax #%datum
                     display
                     with-syntax
                     quote
                     #%app
                     #%parens #%brackets #%braces
                     ...
                     map
                     syntax->list
                     identifier
                     expression
                     statement
                     (rename-out (semicolon \;
                                            )
                                 (ellipses-comma ec)
                                 (ellipses-repeat repeat)
                                 #;
                                 (honu-identifier identifier)
                                 (expression-comma expression_comma)
                                 (honu-macro macro)
                                 (parse-an-expr parse)
                                 (... scheme:...)
                                 (honu-body:class body)
                                 (honu-syntax syntax)
                                 (honu-expression-syntax expressionSyntax)
                                 (honu-+ +)
                                 (honu-scheme scheme2)
                                 (scheme-syntax scheme:syntax)
                                 (scheme-syntax schemeSyntax)
                                 ))
         #%braces #%parens #%brackets
         x
         true
         false
         display
         display2
         newline
         with-syntax
         honu-unparsed-begin
         (for-template with-syntax)
         ;; stuff i done want
         define
         let
         ;; end stuff
         else
         #%app
         (for-template #%app)
         quote
         ...
         foobar2000
         expression
         str
         define-struct
         #;
         (for-template #%parens #%brackets #%braces)
         ;; (for-meta 2 (rename-out (honu-syntax syntax)))
         (rename-out
           (syntax real-syntax)
           (honu-if if)
           (honu-provide provide)
           (honu-macro-item macroItem)
           (honu-macro macro)
           (honu-identifier identifier)
           (honu-require require)
           (honu-for-syntax forSyntax)
           (honu-syntax syntax)
           (honu-pattern pattern)
           (honu-keywords keywords)
           #;
           (honu-scheme scheme2)
           (scheme-syntax scheme:syntax)
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
