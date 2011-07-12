#lang racket/base

(require (for-syntax "transformer.rkt"
                     syntax/define
                     syntax/parse
                     "literals.rkt"
                     "parse2.rkt"
                     "debug.rkt"
                     racket/base)
         syntax/parse)

(provide define-honu-syntax)
(define-syntax (define-honu-syntax stx)
  (let-values ([(id rhs) (normalize-definition stx #'lambda #f)])
    (with-syntax ([id id]
                  [rhs rhs])
      (syntax/loc stx
                 (define-syntax id (make-honu-transformer rhs))))))

(define-for-syntax (convert-pattern pattern)
  (syntax-parse pattern
    [(name semicolon class)
     #'(~var name class)]))

(provide macro)
(define-honu-syntax macro 
  (lambda (code context)
    (debug "Macroize ~a\n" code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name literals (#%braces pattern ...) (#%braces action ...) . rest)
       (debug "Pattern is ~a\n" #'(pattern ...))
       (values
         (with-syntax ([syntax-parse-pattern
                         (convert-pattern #'(pattern ...))])
           #'(define-honu-syntax name
               (lambda (stx context-name)
                 (syntax-parse stx
                   [(_ syntax-parse-pattern . more)
                    (values #'(let-syntax ([do-parse (lambda (stx)
                                                       (parse stx))])
                                (do-parse action ...))
                            #'more)]))))
         #'rest)])))

(provide (rename-out [honu-with-syntax withSyntax]))
(define-honu-syntax honu-with-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ [#%brackets name:id data]
          (#%braces code ...))
       #'(with-syntax ([name data]) code ...)])))
