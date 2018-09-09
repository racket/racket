#lang racket/base

;; To make certain kinds of ports allowed as a place message, a
;; `prop:place-message` property has to chain through properties on
;; the `data` field and, for an fd port, the `extra-data` field

(provide prop:data-place-message
         data->place-message
         prop:fd-extra-data-place-message
         fd-extra-data->opener)

(define-values (prop:data-place-message data-place-message? data-place-message-ref)
  (make-struct-type-property 'data-place-message))

(define (data->place-message data port)
  (if (data-place-message? data)
      ((data-place-message-ref data) port)
      #f))

(define-values (prop:fd-extra-data-place-message fd-extra-data-place-message? fd-extra-data-place-message-ref)
  (make-struct-type-property 'fd-extra-data-place-message))

(define (fd-extra-data->opener extra-data port)
  (if (fd-extra-data-place-message? extra-data)
      ((fd-extra-data-place-message-ref extra-data) port)
      #f))
