#lang racket/base
(require "error.rkt"
         "wrap.rkt"
         "config.rkt"
         "parameter.rkt")

(provide read-box)

(define (read-box read-one dispatch-c in config)
  (unless (check-parameter read-accept-box config)
    (reader-error in config
                  "`~a&` forms not enabled"
                  dispatch-c))
  (define-values (open-end-line open-end-col open-end-pos) (port-next-location in))
  (define e (read-one #f in (next-readtable config)))
  (when (eof-object? e)
    (reader-error in config #:due-to e #:end-pos open-end-pos
                  "expected an element for `~a&` box, found end-of-file"
                  dispatch-c))
  (wrap (if (read-config-for-syntax? config)
            (box-immutable e)
            (box e))
        in config #f))
