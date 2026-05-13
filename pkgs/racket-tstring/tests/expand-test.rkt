#lang racket/base

(require
 rackunit
 racket/string
 "../main.rkt"
) ; end require

(define name "Alice")

(define simple-template
  (tpl "hello {name}")
) ; end define simple-template

(check-true (template? simple-template))
(check-equal? (template-strings simple-template) (list "hello " ""))
(check-equal? (map interpolation-value (template-interpolations simple-template))
              (list "Alice")
) ; end check-equal?
(check-equal? (map interpolation-kind (template-interpolations simple-template))
              (list 'identifier)
) ; end check-equal?
(check-equal? (render-template simple-template) "hello Alice")
(check-equal? (fpl "hello {name}") "hello Alice")
(check-equal? (fpl "static") "static")

(check-equal? (fpl "{(string-upcase name)}") "ALICE")

(define evaluation-order '())

(check-equal?
 (fpl "{(begin (set! evaluation-order (append evaluation-order '(a))) \"A\")}{(begin (set! evaluation-order (append evaluation-order '(b))) \"B\")}")
 "AB"
) ; end check-equal?
(check-equal? evaluation-order '(a b))

(define a 1)
(define b 2)

(define multi-template
  (tpl "{a} + {b} = {(+ a b)}")
) ; end define multi-template

(check-equal? (render-template multi-template) "1 + 2 = 3")
(check-equal? (map interpolation-kind (template-interpolations multi-template))
              (list 'identifier 'identifier 'expression)
) ; end check-equal?

(check-exn
 exn:fail?
 (lambda ()
   (template->sql (tpl "WHERE id = {(+ 1 2)}"))
 ) ; end lambda
) ; end check-exn

(check-equal? (fpl "{{name}}") "{name}")
(check-equal? (fpl "outer {f\"inner {name}\"}") "outer inner Alice")

(define nested-template
  (tpl "outer {t\"inner {name}\"}")
) ; end define nested-template

(check-true (template? (interpolation-value (car (template-interpolations nested-template)))))
(check-equal? (render-template (interpolation-value (car (template-interpolations nested-template))))
              "inner Alice"
) ; end check-equal?

(check-exn
 exn:fail:syntax?
 (lambda ()
   (eval '(let ()
            (require "../main.rkt")
            (tpl 1)
          ) ; end let
   ) ; end eval
 ) ; end lambda
) ; end check-exn

(check-exn
 exn:fail:syntax?
 (lambda ()
   (eval '(let ()
            (require "../main.rkt")
            (tpl "hello {}")
          ) ; end let
   ) ; end eval
 ) ; end lambda
) ; end check-exn empty template interpolation

(check-exn
 exn:fail:syntax?
 (lambda ()
   (eval '(let ()
            (require "../main.rkt")
            (tpl "hello {")
          ) ; end let
   ) ; end eval
 ) ; end lambda
) ; end check-exn unclosed template interpolation

(check-exn
 exn:fail:syntax?
 (lambda ()
   (eval '(let ()
            (require "../main.rkt")
            (tpl "hello }")
          ) ; end let
   ) ; end eval
 ) ; end lambda
) ; end check-exn unmatched right brace

(check-exn
 exn:fail:syntax?
 (lambda ()
   (eval '(let ()
            (require "../main.rkt")
            (tpl "hello {(+ 1 2}")
          ) ; end let
   ) ; end eval
 ) ; end lambda
) ; end check-exn malformed expression
