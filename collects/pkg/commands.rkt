#lang racket/base
(require racket/cmdline
         planet/private/command
         raco/command-name
         racket/function
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse))

(define ((string->option what valid-options) str)
  (define s (string->symbol str))
  (unless (memq s valid-options)
    (raise-user-error (string->symbol 
                       (format "~a ~a" 
                               (short-program+command-name)
                               (current-svn-style-command)))
                      "invalid <~a>: ~a\n  valid <~a>s are:~a"
                      what
                      str
                      what
                      (apply string-append
                             (for/list ([s (in-list valid-options)])
                               (format " ~a" s)))))
  s)

(begin-for-syntax
  (define symbol->keyword
    (compose string->keyword symbol->string))

  (define-syntax-class kind
    #:attributes (default fun (arg-val 1))
    [pattern #:bool
             #:attr (arg-val 1) empty
             #:attr default #'#f
             #:attr fun #'(λ () #t)]
    [pattern (#:sym name:id [opt:id ...] default:expr)
             #:attr (arg-val 1) (list #'name)
             #:attr fun #'(string->option 'name '(opt ...))]
    [pattern (#:str name:id default:expr)
             #:attr (arg-val 1) (list #'name)
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

  (define-syntax-class group-kind
    [pattern #:once-any]
    [pattern #:once-each]
    [pattern #:multi])

  (define-splicing-syntax-class option-group
    #:attributes ((command-line 1) variable (param 1) (call 1))
    [pattern (~seq k:group-kind o:option ...)
             #:attr (command-line 1)
             (syntax->list #'(k o.command-line ...))
             #:attr variable
             #'(begin o.variable ...)
             #:attr (param 1)
             (syntax->list #'(o.param ... ...))
             #:attr (call 1)
             (syntax->list #'(o.call ... ...))])

  (define-syntax-class command
    #:attributes (name function variables command-line)
    [pattern (name:id doc:expr og:option-group ... #:args args body:expr ...)
             #:do
             [(define name-str (symbol->string (syntax->datum #'name)))]
             #:attr function
             (syntax/loc #'name
               (define (name og.param ... ... . args)
                 body ...))
             #:attr variables
             (syntax/loc #'name
               (begin og.variable ...))
             #:attr command-line
             (quasisyntax/loc #'name
               [#,name-str
                doc doc
                og.command-line ... ...
                #:args args
                (args-app args (name og.call ... ...))])]))

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
