#lang racket/base
(define-struct (exn:fail:redex exn:fail) ())
(define (redex-error name fmt . args)
  (define suffix (apply format fmt args))
  (define message
    (if name
        (format "~a: ~a" name suffix)
        suffix))
  (raise (make-exn:fail:redex message (current-continuation-marks))))
(provide redex-error
         exn:fail:redex?
         (struct-out exn:fail:redex))
