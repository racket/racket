#lang racket/base
(require
  rackunit
  compiler/cm)

(check-exn
  (λ (exn) (and (exn:fail? exn) (regexp-match #rx"does not exist" (exn-message exn))))
  (λ () (managed-compile-zo "a-directory-that-doesnt-exist/file.rkt")))
