#lang racket/base
(require ffi/unsafe/custodian)

(define c (make-custodian))

(define done? #f)

(define val (cons 1 2))

(define (reg)
  (register-custodian-shutdown val
                               (lambda (x)
                                 (when done? (error "duplicate!"))
                                 (set! done? (equal? x '(1 . 2))))
                               c
                               #:at-exit? #t))

(unregister-custodian-shutdown val (reg))
(void (reg))

(custodian-shutdown-all c)

(unless done?
  (error "shutdown didn't work"))
