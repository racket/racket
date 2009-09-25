#lang scheme/base
(define-struct (exn:fail:redex exn:fail) ())
(define (redex-error name fmt . args)
  (let ([str (format "~a: ~a" name (apply format fmt args))])
    (raise (make-exn:fail:redex str (current-continuation-marks)))))
(provide redex-error
         exn:fail:redex?
         (struct-out exn:fail:redex))
