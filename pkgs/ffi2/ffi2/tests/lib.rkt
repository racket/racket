#lang racket/base
(require ffi2
         rackunit)

(check-exn exn:fail:contract?
           (lambda ()
             (ffi2-lib-ref #f "hello")))

(check-equal? (ffi2-lib-ref #f "hello" #:fail (lambda () 'ok))
              'ok)
