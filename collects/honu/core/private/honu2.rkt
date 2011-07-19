#lang racket/base

(require "macro2.rkt"
         "operator.rkt"
         (only-in "literals.rkt"
                  semicolon)
         (for-syntax syntax/parse
                     "literals.rkt"
                     "parse2.rkt"
                     racket/base))


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

(define-syntax-rule (define-binary-operator name precedence operator)
                    (begin
                      (provide name)
                      (define-honu-operator/syntax name precedence
                                                   (lambda (left right)
                                                     (with-syntax ([left left]
                                                                   [right right])
                                                       #'(operator left right))))))

(provide honu-+)
(define-honu-operator/syntax honu-+ 1
                             (lambda (left right)
                               (with-syntax ([left left]
                                             [right right])
                                 #'(+ left right))))

(provide honu--)
(define-honu-operator/syntax honu-- 1
                             (lambda (left right)
                               (with-syntax ([left left]
                                             [right right])
                                 #'(- left right))))

(define-binary-operator honu-* 2 *)
(define-binary-operator honu-/ 2 /)
(define-binary-operator honu-^ 2 expt)
