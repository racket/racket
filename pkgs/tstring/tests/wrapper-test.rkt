#lang racket/base

(require
 rackunit
 "../main.rkt"
 "wrapper-racket-base.rkt"
) ; end require

(check-equal? rendered "hello Alice")
(check-true (template? template-value))
(check-equal? (interpolation-value (car (template-interpolations template-value))) "Alice")
(check-equal? (interpolation-format-spec (car (template-interpolations template-value))) #f)
(check-equal? (interpolation-conversion (car (template-interpolations template-value))) "")
(check-equal? nested-rendered "outer inner Alice")
