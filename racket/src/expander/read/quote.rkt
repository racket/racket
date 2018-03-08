#lang racket/base
(require "error.rkt"
         "wrap.rkt")

(provide read-quote)

(define (read-quote read-one sym desc c in config)
  (define wrapped-sym (wrap sym in config c))
  (define-values (end-line end-col end-pos) (port-next-location in))
  (define e (read-one #f in config))
  (when (eof-object? e)
    (reader-error in config #:due-to e #:end-pos end-pos
                  "expected an element for ~a, found end-of-file"
                  desc))
  (wrap (list wrapped-sym e) in config #f))
