#lang racket/base

(provide prop:data-place-message
         data->place-message)

(define-values (prop:data-place-message data-place-message? data-place-message-ref)
  (make-struct-type-property 'data-place-message))

(define (data->place-message data port)
  (if (data-place-message? data)
      ((data-place-message-ref data) port)
      #f))
