#lang racket/base

(require racket/generic unstable/macro-testing)

(module+ test
  (require rackunit)

  (check-exn #rx"not a name for a generics group"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods 3))))
  (check-exn #rx"not a name for a generics group"
             (lambda () (convert-compile-time-error
                         (struct foo () #:methods bad))))
  )
