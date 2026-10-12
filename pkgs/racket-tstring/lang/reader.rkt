#lang racket/base

(require
 racket/port
 racket/runtime-path
 syntax/readerr
 "../private/source-transform.rkt"
) ; end require

(provide
 read
 read-syntax
 get-info
) ; end provide

(define-runtime-path main-rkt "../main.rkt")

(define base-read-syntax
  (dynamic-require 'racket/base 'read-syntax)
) ; end define base-read-syntax

(define (read in)
  (syntax->datum (read-syntax #f in))
) ; end define read

(define (read-syntax path in)
  (define source (port->string in))
  (define transformed-body
    (with-handlers ((exn:fail?
                     (lambda (exn)
                       (raise-read-error (exn-message exn)
                                         path
                                         #f
                                         #f
                                         #f
                                         #f
                       ) ; end raise-read-error
                     ) ; end lambda
                    ) ; end exn:fail?
                   ) ; end handlers
      (transform-template-prefixes source)
    ) ; end with-handlers
  ) ; end define transformed-body
  (define transformed-source
    (string-append "#lang racket/base\n"
                   (format "(require (file ~s))\n" (path->string main-rkt))
                   transformed-body
    ) ; end string-append
  ) ; end define transformed-source
  (define transformed-in (open-input-string transformed-source))
  (port-count-lines! transformed-in)
  (base-read-syntax path transformed-in)
) ; end define read-syntax

(define (get-info key default default-filter)
  (default-filter key default)
) ; end define get-info
