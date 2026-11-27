#lang racket/base

(require
 rackunit
 "../main.rkt"
 "reader-basic.rkt"
 "reader-nesting.rkt"
) ; end require

(check-equal? rendered "hello Alice")
(check-true (template? template-value))
(check-equal? (interpolation-value (car (template-interpolations template-value))) "Alice")
(check-equal? (interpolation-format-spec (car (template-interpolations template-value))) #f)
(check-equal? (interpolation-conversion (car (template-interpolations template-value))) "")
(check-equal? ordinary-string "f\"not a template\"")
(check-equal? nested-rendered "outer inner 1")
(check-true (template? nested-template))
(check-true (template? (interpolation-value (car (template-interpolations nested-template)))))
(check-equal? expression-nested-rendered "a1b")
(check-equal? string-brace-rendered "}x")
