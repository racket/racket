#lang racket/base

(provide internal-error)

(define (internal-error msg)
  (raise (exn:fail (string-append "internal error: " msg)
                   (current-continuation-marks))))
