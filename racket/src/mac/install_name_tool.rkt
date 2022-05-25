#lang racket/base
(require compiler/private/macfw)

(define args (current-command-line-arguments))
(unless (equal? (vector-length args) 4)
  (error 'install_name_tool "expected 4 arguments"))
(unless (equal? (vector-ref args 0) "-change")
  (error 'install_name_tool "expected `-change`"))

(define from (vector-ref args 1))
(define to (vector-ref args 2))
(define dest (vector-ref args 3))

(update-framework-path to #:as-given? #t
                       dest
                       #f)
