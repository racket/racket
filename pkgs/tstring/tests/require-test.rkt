#lang racket/base

(require
 rackunit
 "../main.rkt"
) ; end require

(define tpl-value
  (template (list "hello " "")
            (list (interpolation "Alice" #'name #f ""))
  ) ; end template
) ; end define tpl-value

(check-true (template? tpl-value))
(check-equal? (render-template tpl-value) "hello Alice")
(check-equal? (fpl "{(+ 1 2)}") "3")
