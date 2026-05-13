#lang racket/base

(require
 rackunit
 "../main.rkt"
 "wrapper-racket-base.rkt"
) ; end require

(check-equal? rendered "hello Alice")
(check-true (template? template-value))
(check-equal? (render-template template-value) "hello Alice")
(check-equal? nested-rendered "outer inner Alice")
