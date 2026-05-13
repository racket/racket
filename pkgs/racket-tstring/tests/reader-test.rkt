#lang racket/base

(require
 rackunit
 "../main.rkt"
 "reader-basic.rkt"
 "reader-nesting.rkt"
) ; end require

(check-equal? rendered "hello Alice")
(check-true (template? template-value))
(check-equal? (render-template template-value) "hello Alice")
(check-equal? ordinary-string "f\"not a template\"")
(check-equal? nested-rendered "outer inner 1")
(check-true (template? nested-template))
(check-true (template? (interpolation-value (car (template-interpolations nested-template)))))
(check-equal? expression-nested-rendered "a1b")
(check-equal? string-brace-rendered "}x")
