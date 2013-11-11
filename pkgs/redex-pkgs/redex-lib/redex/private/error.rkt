#lang racket/base

(provide redex-error
         exn:fail:redex?
         (struct-out exn:fail:redex)
         unsupported
         unimplemented)

(define-struct (exn:fail:redex exn:fail) ())
(define (redex-error name fmt . args)
  (define suffix (apply format fmt args))
  (define message
    (if name
        (format "~a: ~a" name suffix)
        suffix))
  (raise (make-exn:fail:redex message (current-continuation-marks))))

(define (unsupported pat)
  (redex-error 'generate-term "#:i-th does not support ~s patterns" pat))

(define (unimplemented pat)
  (redex-error 'generate-term "#:i-th does not yet support ~s patterns" pat))
