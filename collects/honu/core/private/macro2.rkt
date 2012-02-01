#lang racket/base

(require (for-syntax "transformer.rkt"
                     syntax/parse
                     syntax/stx
                     racket/syntax
                     "literals.rkt"
                     "parse2.rkt"
                     "debug.rkt"
                     racket/base)
         (for-meta 2 syntax/parse
                     racket/base
                     "parse2.rkt"
                     "compile.rkt")
         "literals.rkt"
         "syntax.rkt"
         #;
         (for-syntax "honu-typed-scheme.rkt")
         syntax/parse)

(define-for-syntax (convert-pattern original-pattern)
  (define-splicing-syntax-class pattern-type
    #:literal-sets (cruft)
    [pattern (~seq name colon class)
             #:with (result ...) #'((~var name class #:attr-name-separator "_"))]
    [pattern (x:pattern-type ...) #:with (result ...) #'((x.result ... ...))]
    [pattern x #:with (result ...) #'(x)])
  (syntax-parse original-pattern
    [(thing:pattern-type ...)
     #'(thing.result ... ...)]))

(define-for-syntax (find-pattern-variables original-pattern)
  (define-splicing-syntax-class pattern-type
    #:literal-sets (cruft)
    [pattern (~seq name colon class)
             ;; we know the output of syntactic classes will end with _result
             #:with (result ...) (with-syntax ([name.result (format-id #'name "~a_result" #'name)])
                             #'((name name.result)))]
    [pattern (x:pattern-type ...) #:with (result ...) #'(x.result ... ...)]
    [pattern x #:with (result ...) #'()])
  (syntax-parse original-pattern
    [(thing:pattern-type ...)
     (filter (lambda (x) (syntax-e x)) (syntax->list #'(thing.result ... ...)))]))

(begin-for-syntax
(define-syntax (parse-stuff stx)
  (syntax-parse stx
    [(_ stuff ...)
     (honu->racket (parse-all #'(stuff ...)))])))

(provide honu-macro)
(define-honu-syntax honu-macro 
  (lambda (code context)
    (debug "Macroize ~a\n" code)
    (syntax-parse code #:literal-sets (cruft)
      [(_ name (#%parens literal ...) (#%braces pattern ...) (#%braces action ...) . rest)
       (debug "Pattern is ~a\n" #'(pattern ...))
       (debug 2 "Pattern variables ~a\n" (find-pattern-variables #'(pattern ...)))
       (values
         (with-syntax ([(syntax-parse-pattern ...)
                        (convert-pattern #'(pattern ...))]
                       [((pattern-variable.name pattern-variable.result) ...)
                        (find-pattern-variables #'(pattern ...))])
           #'(%racket (define-honu-syntax name
                        (lambda (stx context-name)
                          (define-literal-set local-literals (literal ...))
                          (syntax-parse stx
                            #:literal-sets ([cruft #:at name]
                                            [local-literals #:at name])
                            [(_ syntax-parse-pattern ... . more)
                             (values
                               ;; if the pattern is x:expression then x_result will
                               ;; hold the parsed version of x, so we rebind x to
                               ;; x_result so you can use just x in the template
                               ;; instead of x_result. x_result is still there, too
                               (with-syntax ([pattern-variable.name #'pattern-variable.result]
                                             ...)
                                 (parse-stuff action ...)
                                 #;
                                 (let-syntax ([parse-more (lambda (stx)
                                                            (parse-all #'(action ...)))])
                                   (parse-more)))
                               #'more #t)])))))
         #'rest
         #t)])))

(provide honu-syntax)
;; Do any honu-specific expansion here
(define-honu-syntax honu-syntax
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens stuff ...) . rest)
       (define context (stx-car #'(stuff ...)))
       (values
         (with-syntax ([stuff* (datum->syntax context
                                              (syntax->list #'(stuff ...))
                                              context context)])
           #'(%racket #'stuff*))
         #; #'(%racket-expression (parse-stuff stuff ...))
         #'rest
         #f)])))

;; combine syntax objects
;; #'(a b) + #'(c d) = #'(a b c d)
(provide mergeSyntax)
(define (mergeSyntax syntax1 syntax2)
  (with-syntax ([(syntax1* ...) syntax1]
                [(syntax2* ...) syntax2])
    #'(syntax1* ... syntax2* ...)))
