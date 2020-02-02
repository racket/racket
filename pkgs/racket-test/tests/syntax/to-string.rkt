#lang racket
(require syntax/to-string
         rackunit)

(check-equal? (syntax->string #'((a . 
                                      b))) "(a . \n     b)")
(check-equal? (syntax->string #'((a b c d))) "(a b c d)")
(check-equal? (syntax->string #'(a 'b #(a b c) c)) "a 'b #(a b c) c")
(check-equal? (syntax->string #'((a b  _   d))) "(a b  _   d)")
