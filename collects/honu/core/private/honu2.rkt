#lang racket/base

(require "macro2.rkt"
         "operator.rkt"
         "struct.rkt"
         "honu-typed-scheme.rkt"
         racket/class
         racket/require
         (only-in "literals.rkt"
                  honu-then
                  honu-in
                  honu-prefix
                  semicolon)
         (for-syntax syntax/parse
                     "literals.rkt"
                     "parse2.rkt"
                     racket/base))

(provide (all-from-out "struct.rkt"))

(provide honu-function)
(define-honu-syntax honu-function
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens arg:identifier ...)
          (#%braces code ...)
          . rest)
       (values
         #'(lambda (arg ...)
             (let-syntax ([do-parse (lambda (stx)
                                      (parse-all #'(code ...)))])
               (do-parse)))
         #'rest
         #f)])))

(provide honu-var)
(define-honu-syntax honu-var
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-=)
      [(_ name:id honu-= . rest)
       ;; parse one expression
       (define-values (parsed unparsed)
                      (parse #'rest))
       (values
         (with-syntax ([parsed parsed])
           #'(define name parsed))
         (with-syntax ([unparsed unparsed])
         #'unparsed)
         #t)])))

(provide honu-for)
(define-honu-syntax honu-for
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (honu-= honu-in)
      [(_ iterator:id honu-= start:honu-expression honu-to end:honu-expression
          honu-do body:honu-expression . rest)
       (values
         #'(for ([iterator (in-range start.result end.result)])
             body.result)
         #'rest
         #t)]
      [(_ iterator:id honu-in stuff:honu-expression
          honu-do body:honu-expression . rest)
       (values #'(for ([iterator stuff.result])
                   body.result)
               #'rest
               #t)])))

(provide honu-if)
(define-honu-syntax honu-if
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
                       #:literals (else honu-then)
      [(_ condition:honu-expression honu-then true:honu-expression else false:honu-expression . rest)
       (values
         #'(if condition.result true.result false.result)
         #'rest
         #f)])))

(provide honu-val)
(define-honu-syntax honu-val
  (lambda (code context)
    (syntax-parse code
      [(_ rest ...)
       (define-values (parsed unparsed)
                      (parse #'(rest ...)))
       (values parsed unparsed #t)])))

(provide honu-quote)
(define-honu-syntax honu-quote
  (lambda (code context)
    (syntax-parse code
      [(_ expression rest ...)
       (values #'(quote expression) #'(rest ...) #f)])))

(provide honu-quasiquote)
(define-honu-syntax honu-quasiquote
  (lambda (code context)
    (syntax-parse code
      [(_ expression rest ...)
       (values #'(quasiquote expression)
               #'(rest ...)
               #f)])))

(define-syntax-rule (define-binary-operator name precedence associativity operator)
                    (begin
                      (provide name)
                      (define-honu-operator/syntax name precedence associativity
                                                   ;; binary
                                                   (lambda (left right)
                                                     (with-syntax ([left left]
                                                                   [right right])
                                                       #'(operator left right)))
                                                   ;; unary
                                                   (lambda (argument)
                                                     (with-syntax ([argument argument])
                                                       #'(operator argument))))))

(define-syntax-rule (define-unary-operator name precedence associativity operator)
                    (begin
                      (provide name)
                      (define-honu-operator/syntax name precedence associativity
                                                   #f
                                                   ;; unary
                                                   (lambda (argument)
                                                     (with-syntax ([argument argument])
                                                       #'(operator argument))))))

(provide honu-dot)
(define-honu-operator/syntax honu-dot 10000 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(let ([left* left])
          (cond
            [(honu-struct? left*) (let ([use (honu-struct-get left*)])
                                    (use left* 'right))]
            [(object? left*) (lambda args
                               (send/apply left* right args))]
            ;; possibly handle other types of data
            [else (error 'dot "don't know how to deal with ~a (~a)" 'left left*)])))))

(provide honu-flow)
(define-honu-operator/syntax honu-flow 0.001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(right left))))

(provide honu-assignment)
(define-honu-operator/syntax honu-assignment 0.0001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(set! left right))))

(define-binary-operator honu-+ 1 'left +)
(define-binary-operator honu-- 1 'left -)
(define-binary-operator honu-* 2 'left *)
(define-binary-operator honu-/ 2 'left /)
(define-binary-operator honu-^ 2 'right expt)
(define-binary-operator honu-< 0.9 'left <)
(define-binary-operator honu-<= 0.9 'left <=)
(define-binary-operator honu-> 0.9 'left >)
(define-binary-operator honu->= 0.9 'left >=)
(define-binary-operator honu-= 0.9 'left =)
(define-binary-operator honu-and 0.5 'left and)
(define-binary-operator honu-or 0.5 'left or)
(define-binary-operator honu-cons 0.1 'right cons)
(define-binary-operator honu-map 0.09 'left map)

(define-unary-operator honu-not 0.7 'left not)

(provide honu-top-interaction)
(define-syntax (honu-top-interaction stx)
  (syntax-case stx ()
    [(_ rest ...)
     #'(#%top-interaction . (honu-unparsed-begin rest ...))]))

(begin-for-syntax
  (define-splicing-syntax-class require-form
                                #:literals (honu-prefix)
                                #:literal-sets (cruft)
    [pattern (~seq honu-prefix prefix module)
             #:with result #'(prefix-in prefix module)]
    [pattern x:str #:with result #'x]
    [pattern x:id #:with result #'x
             #:when (not ((literal-set->predicate cruft) #'x))]))

(define-for-syntax (racket-names->honu name)
  (regexp-replace* #rx"-" "_"))

(provide honu-require)
(define-honu-syntax honu-require
  (lambda (code context)
    (syntax-parse code
      [(_ form:require-form ... . rest)
       (values
         #'(require (filtered-in (lambda (name)
                                   (regexp-replace* #rx"-"
                                                    (regexp-replace* #rx"->" name "_to_")
                                                    "_"))
                                 (combine-in form.result ...)))

         #'rest
         #f)])))

(provide honu-with-input-from-file)
(define-honu-syntax honu-with-input-from-file
  (lambda (code context)
    (syntax-parse code #:literal-sets (cruft)
      [(_ (#%parens name:id) something:honu-expression . rest)
       (define with #'(with-input-from-file name (lambda () something.result)))
       (values
         with
         #'rest
         #f)])))
