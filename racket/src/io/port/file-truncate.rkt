#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "check.rkt"
         "port.rkt"
         "output-port.rkt"
         "file-stream.rkt")

(provide prop:file-truncate
         file-truncate)

(define-values (prop:file-truncate file-truncate? file-truncate-ref)
  (make-struct-type-property 'file-truncate))

(define/who (file-truncate p pos)
  (unless (and (output-port? p)
               (file-stream-port? p))
    (raise-argument-error who "(and/c output-port? file-stream-port?)" p))
  (check 'file-truncate exact-nonnegative-integer? pos)
  (atomically
   (check-not-closed who p)
   (let ([p (->core-output-port p)])
     ((file-truncate-ref p) p pos))))
