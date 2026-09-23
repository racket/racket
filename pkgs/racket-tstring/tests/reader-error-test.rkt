#lang racket/base

(require
 rackunit
 "../lang/reader.rkt"
) ; end require

(define (read-tstring-source source)
  (read-syntax 'reader-error-test (open-input-string source))
) ; end define read-tstring-source

(check-exn
 exn:fail:read?
 (lambda ()
   (read-tstring-source "f\"hello")
 ) ; end lambda
) ; end check-exn unclosed template string

(check-exn
 exn:fail:read?
 (lambda ()
   (read-tstring-source "f\"hello {name")
 ) ; end lambda
) ; end check-exn unclosed reader interpolation
