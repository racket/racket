#lang racket/base

(provide internal-error)

(define (internal-error s)
  (raise (exn:fail (string-append "internal error: " s)
                   (current-continuation-marks))))
