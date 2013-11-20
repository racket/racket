#lang racket/base
(require racket/cmdline
         planet/private/command
         raco/command-name
         racket/function
         (for-syntax racket/base
                     racket/list
                     racket/syntax
                     syntax/parse
                     syntax/stx))

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

(define ((string->num what) str)
  (define n (string->number str))
  (unless (exact-nonnegative-integer? n)
    (raise-user-error (string->symbol 
                       (format "~a ~a" 
                               (short-program+command-name)
                               (current-svn-style-command)))
                      "invalid <~a> number: ~a"
                      what
                      str))
  n)

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
             #:attr fun #'identity]
    [pattern (#:num name:id default:expr)
             #:attr (arg-val 1) (list #'name)
             #:attr fun #'(string->num 'name)])

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

  (define-splicing-syntax-class usage-help
    #:attributes ((command-line 1))
    [pattern (~seq #:usage-help s:str ...)
             #:attr (command-line 1)
             (syntax->list #'(#:usage-help s ...))])

  (define-splicing-syntax-class arguments
    #:attributes (accum args (body 1) help-strs)
    [pattern (~seq #:args args 
                   body:expr ...)
             #:with accum #'ignored
             #:with help-strs (with-syntax ([strs 
                                             (map symbol->string
                                                  (map syntax->datum
                                                       (let loop ([args #'args])
                                                         (cond 
                                                          [(stx-null? args) null]
                                                          [(stx-pair? args)
                                                           (cons (stx-car args)
                                                                 (loop (stx-cdr args)))]
                                                          [else
                                                           (list args)]))))])
                                #`(list . strs))]
    [pattern (~seq #:handlers
                   (lambda (accum . args) body:expr ...) 
                   help-strs:expr)])

  (define-syntax-class command
    #:attributes (name function variables command-line)
    [pattern (name:id doc:expr uh:usage-help ... og:option-group ... arg:arguments)
             #:do
             [(define name-str (symbol->string (syntax->datum #'name)))]
             #:attr function
             (syntax/loc #'name
               (define (name og.param ... ... . arg.args)
                 arg.body ...))
             #:attr variables
             (syntax/loc #'name
               (begin og.variable ...))
             #:attr command-line
             (quasisyntax/loc #'name
               [#,name-str
                doc doc
                uh.command-line ... ...
                og.command-line ... ...
                #:handlers
                (lambda (accum . arg.args)
                  (args-app arg.args (name og.call ... ...)))
                arg.help-strs])]))

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
    [(_ main-doc:expr export-format:str c:command ...)
     (with-syntax ([(export-names ...)
                    (map (λ (x) 
                           #`[#,x
                              #,(string->symbol (format (syntax-e #'export-format)
                                                        (syntax-e x)))])
                         (syntax->list #'(c.name ...)))])
     (syntax/loc stx
       (begin
         c.function ...
         (provide (rename-out export-names ...))
         (module+ main
           c.variables ...
           (svn-style-command-line
            #:program (short-program+command-name)
            #:argv (current-command-line-arguments)
            main-doc
            c.command-line ...)))))]))

(provide commands)
