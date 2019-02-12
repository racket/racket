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
  (define accessor (file-stream-ref
                    (cond
                      [(input-port? p) (->core-input-port p)]
                      [(output-port? p) (->core-output-port p)]
                      [else
                       (raise-argument-error 'file-stream-port?
                                             "port?"
                                             p)])
                    #f))
  (and accessor
       (accessor p #t)))
