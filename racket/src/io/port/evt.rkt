#lang racket/base
(require "../host/thread.rkt")

;; To make a port act like an event, the `prop:secondary-evt` property
;; must be mapped to `port->evt` --- both for the `core-port`
;; structure type and implied by `prop:input-port` and
;; `prop:output-port`. As the name suggests, `prop:secondary-evt` is
;; used only when a structure doesn't have `prop:evt`, so `prop:input-port`
;; and `prop:output-port` can be mixed with `prop:evt`.

;; A structure with `prop:secondary-evt` mapped to `port->evt` should
;; also have `prop:input-port-evt` or `prop:input-port-evt`. Those
;; properties provide an indirection to avoid a dependency cycle between
;; this module and the implement of input and output ports.

(provide port->evt
         prop:input-port-evt input-port-evt? input-port-evt-ref
         prop:output-port-evt output-port-evt? output-port-evt-ref)

(define-values (prop:input-port-evt input-port-evt? input-port-evt-ref)
  (make-struct-type-property 'input-port-evt))

(define-values (prop:output-port-evt output-port-evt? output-port-evt-ref)
  (make-struct-type-property 'output-port-evt))

(define (port->evt p)
  ;; A structure can be both an input port and an output
  ;; port, and the input nature takes precedence
  (cond
    [(input-port-evt? p)
     (wrap-evt ((input-port-evt-ref p) p)
               (lambda (v) p))]
    [else
     (wrap-evt ((output-port-evt-ref p) p)
               (lambda (v) p))]))
