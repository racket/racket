#lang racket/base
(require rackunit syntax/readerr)

(check-exn
 (λ (x) 
   (and (exn:fail:read:eof? x)
        (regexp-match #rx"^y[01: ]* x" (exn-message x))))
 (λ () (raise-read-eof-error
        "x" "y" 1 1 1 1)))
