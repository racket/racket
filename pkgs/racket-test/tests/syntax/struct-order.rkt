#lang racket/base

(require rackunit
         racket/runtime-path)

(define-runtime-path this-file "struct-order.rkt")

(module use-before-struct racket/base
  (s)
  (struct s () #:transparent))

(check-exn
 (lambda (exn)
   (regexp-match? #rx"^s: undefined;" (exn-message exn)))
 (lambda ()
   (dynamic-require `(submod ,this-file use-before-struct) #f)))
