#lang racket/base

(require "macro2.rkt"
         "operator.rkt"
         "struct.rkt"
         (only-in "literals.rkt"
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
      [(_ iterator:id honu-= start:honu-expression honu-to end:honu-expression
          honu-do body:honu-expression . rest)
       (values
         #'(for ([iterator (in-range start.result end.result)])
             body.result)
         #'rest
         #t)])))

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
                                                   (lambda (left right)
                                                     (with-syntax ([left left]
                                                                   [right right])
                                                       #'(operator left right))))))

(provide honu-dot)
(define-honu-operator/syntax honu-dot 10000 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(let ([left* left])
          (cond
            [(honu-struct? left*) (let ([use (honu-struct-get left*)])
                                    (use left* 'right))]
            ;; possibly handle other types of data
            [else (error 'dot "don't know how to deal with ~a" 'left)])))))

(provide honu-flow)
(define-honu-operator/syntax honu-flow 0.001 'left
  (lambda (left right)
    (with-syntax ([left left]
                  [right right])
      #'(right left))))

(define-binary-operator honu-+ 1 'left +)
(define-binary-operator honu-- 1 'left -)
(define-binary-operator honu-* 2 'left *)
(define-binary-operator honu-/ 2 'left /)
(define-binary-operator honu-^ 2 'right expt)
(define-binary-operator honu-and 0.5 'left and)
(define-binary-operator honu-or 0.5 'left or)
(define-binary-operator honu-cons 0.1 'right cons)
