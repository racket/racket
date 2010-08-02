#lang racket/base

(require (for-syntax racket/base))
(require (for-meta 2 racket/base))
(require racket/class)

(require "private/honu-typed-scheme.rkt"
         ;; "private/honu.ss"
         "private/parse.ss"
         (for-syntax "private/literals.rkt")
         (for-syntax "private/honu-typed-scheme.rkt")
         (for-syntax "private/parse.rkt")
         (for-syntax "private/canonical.rkt")
         syntax/parse
         (for-syntax syntax/parse)
         "private/literals.rkt"
         "private/syntax.rkt"
         "private/more.rkt"
         (for-template racket/base)
         (for-template "private/literals.rkt")
         (for-syntax "private/more.rkt")
         (for-syntax "private/syntax.rkt")
         (for-syntax "private/macro.rkt")
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

(define-for-syntax (syntax-to-string stx)
  (format "original '~a' - ~a" (syntax->datum stx) (to-honu-string stx)))

(define-syntax (honu-struct stx)
  (syntax-parse stx
    [(_ name (my-field ...))
     (with-syntax ([new-name (gensym (syntax->datum #'name))])
       #'(begin
           (define new-name
             (class object%
                    (init-field my-field ...)
                    (super-new)))
           (define name (lambda args (apply make-object new-name args)))))]))

(provide (rename-out (#%dynamic-honu-module-begin #%module-begin)
                     ;; (honu-top #%top)
                     (semicolon \;
                                )
                     (honu-+ +)
                     (honu-* *)
                     (+ scheme:+)
                     (honu-/ /)
                     (honu-- -)
                     (honu-< <)
                     (honu-> >)
                     (honu->= >=)
                     (honu-<= <=)
                     (honu-== ==)
                     (honu-= =)
                     (honu-literal literals)
                     (honu-!= !=)
                     (honu-? ?)
                     (honu-: :)
                     (honu-and and)
                     (honu-comma |,|)
                     (honu-. |.|)
                     (expression-comma expression_comma)
                     )

         #;
         (rename-out [honu-print print])

         (for-syntax (rename-out [syntax-to-string syntax_to_string]))

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
         (for-template #%datum)
         datum->syntax
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
                     ;identifier
                     expression
                     statement
                     (rename-out (semicolon \;
                                            )
                                 (ellipses-comma ec)
                                 (ellipses-repeat repeat)
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
         sqrt
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
         lambda
         #%app
         (for-template #%app)
         quote
         ...
         foobar2000
         expression
         str
         ;; define-struct
         #;
         (for-template #%parens #%brackets #%braces)
         in-range
         honu-struct
         ;; (for-meta 2 (rename-out (honu-syntax syntax)))
         (rename-out
           (struct scheme-struct)
           (syntax real-syntax)
           (for scheme-for)
           (honu-if if)
           (honu-provide provide)
           (honu-macro-item macroItem)
           (honu-macro macro)
           (honu-identifier identifier)
           (honu-identifier identifier123)
           (honu-require require)
           (honu-for-syntax forSyntax)
           (honu-for-template forTemplate)
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
