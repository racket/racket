#lang racket
(require syntax/to-string
         rackunit)

(check-equal? (syntax->string #'((a . 
                                      b))) "(a . \n     b)")
(check-equal? (syntax->string #'(( a b  c d))) "( a b  c d)")
(check-equal? (syntax->string #'(a 'b #(a b c) c)) "a 'b #(a b c) c")
(check-equal? (syntax->string #'((a b  _   d))) "(a b  _   d)")

(check-equal? (syntax->string #'((  a  b  ))) "(  a  b  )")
; we can't find where . is, default to earliest position
(check-equal? (syntax->string #'((  a  b  .  c  ))) "(  a  b .   c  )") 

;; quote tests
(check-equal? (syntax->string #'('a)) "'a")
(check-equal? (syntax->string #'('  a)) "'  a")
(check-equal? (syntax->string #'((quote a))) "(quote a)")
(check-equal? (syntax->string #'((  quote  a  ))) "(  quote  a  )")
(check-equal? (syntax->string #'((quote a b))) "(quote a b)")
(check-equal? (syntax->string #'((  quote  a  b  ))) "(  quote  a  b  )")
(check-equal? (syntax->string #'((quote . a))) "(quote . a)")
(check-equal? (syntax->string #'((  quote  .  a  ))) "(  quote .   a  )")
(check-equal? (syntax->string #'((quote a b . c))) "(quote a b . c)")
(check-equal? (syntax->string #'((  quote  a  b  .  c  ))) "(  quote  a  b .   c  )")
