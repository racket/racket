#lang racket/load

;; Test for PR 11172

(module a typed/racket (: n Positive-Integer) (define n 10) (provide: [n Integer]))
(module b typed/racket (define n 10) (provide: [n Integer]))
(module c typed/racket (provide: [n Integer]) (define n 10))
(module d typed/racket (provide: [n Integer]) (: n Positive-Integer) (define n 10))

(require 'a)
n

