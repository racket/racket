#lang racket/base

(require (for-syntax "transformer.rkt"
                     syntax/define
                     syntax/parse
                     syntax/stx
                     racket/syntax
                     "literals.rkt"
                     "parse2.rkt"
                     "debug.rkt"
                     racket/base)
         "literals.rkt"
         #;
         (for-syntax "honu-typed-scheme.rkt")
         syntax/parse)

(provide define-honu-syntax)
(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-transformer rhs))))))

(define-for-syntax (convert-pattern original-pattern)
  (define-splicing-syntax-class pattern-type
    #:literal-sets (cruft)
    [pattern (~seq name colon class)
             #:with result #'(~var name class #:attr-name-separator "_")]
    [pattern x #:with result #'x])
  (syntax-parse original-pattern
    [(thing:pattern-type ...)
     #'(thing.result ...)]))

(define-for-syntax (find-pattern-variables original-pattern)
  (define-splicing-syntax-class pattern-type
    #:literal-sets (cruft)
    [pattern (~seq name colon class)
             #:with result (with-syntax ([name.result (format-id #'name "~a_result" #'name)])
                             #'(name name.result))]
    [pattern x #:with result #f])
  (syntax-parse original-pattern
    [(thing:pattern-type ...)
     (filter (lambda (x) x) (syntax->list #'(thing.result ...)))]))

(provide honu-macro)
(define-honu-syntax honu-macro 
  (lambda (code context)
    (debug "Macroize ~a\n" code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name literals (#%braces pattern ...) (#%braces action ...) . rest)
       (debug "Pattern is ~a\n" #'(pattern ...))
       (values
         (with-syntax ([(syntax-parse-pattern ...)
                        (convert-pattern #'(pattern ...))]
                       [((pattern-variable.name pattern-variable.result) ...)
                        (find-pattern-variables #'(pattern ...))]
                       [(code ...) (parse-all #'(action ...))])
           #'(%racket (define-honu-syntax name
                        (lambda (stx context-name)
                          (syntax-parse stx
                            [(_ syntax-parse-pattern ... . more)
                             (values
                               ;; if the pattern is x:expression then x_result will
                               ;; hold the parsed version of x, so we rebind x to
                               ;; x_result so you can use just x in the template
                               ;; instead of x_result. x_result is still there, too
                               (with-syntax ([pattern-variable.name #'pattern-variable.result]
                                             ...)
                                 (code ...)) #'more #t)])))))
         #'rest
         #t)])))

(provide (rename-out [honu-with-syntax withSyntax]))
(define-honu-syntax honu-with-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ [#%brackets name:id data]
          (#%braces code ...))
       #'(%racket (with-syntax ([name data]) code ...))])))

#;
(define-syntax (parse-stuff stx)
  (syntax-parse stx
    [(_ stuff ...)
     (parse-all #'(stuff ...))]))

(provide honu-syntax)
;; Do any honu-specific expansion here
(define-honu-syntax honu-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens stuff ...) . rest)
       (values
         #'(%racket #'(stuff ...))
         #; #'(%racket-expression (parse-stuff stuff ...))
         #'rest
         #f)])))
