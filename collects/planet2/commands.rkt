#lang racket/base
(require racket/cmdline
         planet/private/command
         raco/command-name
         racket/function
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(begin-for-syntax
  (define symbol->keyword
    (compose string->keyword symbol->string))

  (define-syntax-class kind
    #:attributes (default fun (arg-val 1))
    [pattern #:bool
             #:attr (arg-val 1) empty
             #:attr default #'#f
             #:attr fun #'(λ () #t)]
    [pattern (#:sym default:expr)
             #:attr (arg-val 1) (list #'string)
             #:attr fun #'string->symbol]
    [pattern (#:str default:expr)
             #:attr (arg-val 1) (list #'string)
             #:attr fun #'identity])

  (define-syntax-class option
    #:attributes (command-line variable (param 1) (call 1))
    [pattern (k:kind arg:id (alias:str ...) doc:expr)
             #:do
             [(define arg-kw (symbol->keyword (syntax->datum #'arg)))
              (define arg-str (format "--~a" (syntax->datum #'arg)))
              (define arg-var (generate-temporary #'arg))]
             #:attr variable
             (quasisyntax/loc #'arg
               (define #,arg-var k.default))
             #:attr (param 1)
             (syntax-e
              (quasisyntax/loc #'arg
                [#,arg-kw [arg k.default]]))
             #:attr (call 1)
             (syntax-e
              (quasisyntax/loc #'arg
                [#,arg-kw #,arg-var]))
             #:attr command-line
             (quasisyntax/loc #'arg
               [(alias ... #,arg-str)
                k.arg-val ...
                doc
                (set! #,arg-var (k.fun k.arg-val ...))])])

  (define-syntax-class command
    #:attributes (name function variables command-line)
    [pattern (name:id doc:expr o:option ... #:args args body:expr ...)
             #:do
             [(define name-str (symbol->string (syntax->datum #'name)))]
             #:attr function
             (syntax/loc #'name
               (define (name o.param ... ... . args)
                 body ...))
             #:attr variables
             (syntax/loc #'name
               (begin o.variable ...))
             #:attr command-line
             (quasisyntax/loc #'name
               [#,name-str
                doc doc
                #:once-each
                o.command-line ...
                #:args args
                (args-app args (name o.call ... ...))])]))

(define-syntax (args-app stx)
  (syntax-parse stx
    [(_ () (call ...))
     (syntax/loc stx
       (call ...))]
    [(_ rest:id (call ...))
     (syntax/loc stx
       (apply call ... rest))]
    [(_ (fst . snd) (call ...))
     (syntax/loc stx
       (args-app snd (call ... fst)))]))

(define-syntax (commands stx)
  (syntax-parse stx
    [(_ main-doc:expr c:command ...)
     (syntax/loc stx
       (begin
         c.function ...
         (provide c.name ...)
         (module+ main
           c.variables ...
           (svn-style-command-line
            #:program (short-program+command-name)
            #:argv (current-command-line-arguments)
            main-doc
            c.command-line ...))))]))

(provide commands)
