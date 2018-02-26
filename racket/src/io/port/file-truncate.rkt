#lang racket/base
(require "../common/check.rkt"
         "port.rkt"
         "output-port.rkt"
         "file-stream.rkt")

(provide prop:file-truncate
         file-truncate)

(define-values (prop:file-truncate file-truncate? file-truncate-ref)
  (make-struct-type-property 'file-truncate))

(define (file-truncate p pos)
  (unless (and (output-port? p)
               (file-stream-port? p))
    (raise-argument-error 'file-truncate "(and/c output-port? file-stream-port?)" p))
  (check 'file-truncate exact-nonnegative-integer? pos)
  (let ([p (->core-output-port p)])
    (define data (core-port-data p))
    ((file-truncate-ref data) data pos)))
