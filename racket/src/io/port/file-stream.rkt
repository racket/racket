#lang racket/base
(require "port.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide prop:file-stream
         file-stream-ref
         file-stream-port?)

;; Property value should be a function that returns a file descriptor
(define-values (prop:file-stream file-stream? file-stream-ref)
  (make-struct-type-property 'file-stream))

(define (file-stream-port? p)
  (define core-port 
    (or (->core-input-port p #:default #f)
        (->core-output-port p #:default #f)))
  (and (file-stream-ref core-port #f) #t))

