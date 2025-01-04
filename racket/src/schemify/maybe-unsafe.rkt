#lang racket/base
(require "match.rkt"
         "wrap.rkt")

(provide maybe-unsafe)

(define (maybe-unsafe orig-s body)
  (match body
    [`((begin-unsafe . ,_)) body]
    [`,_
     (if (wrap-property orig-s 'body-as-unsafe)
         `((begin-unsafe . ,body))
         body)]))
