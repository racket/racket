#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/rktio.rkt"
         "../port/port.rkt"
         "../port/close.rkt"
         "../port/input-port.rkt"
         "../port/output-port.rkt"
         "../port/fd-port.rkt"
         "../port/file-stream.rkt"
         "error.rkt")

(provide open-input-output-tcp
         tcp-port?
         tcp-abandon-port)

(class tcp-input-port #:extends fd-input-port
  #:field
  [abandon? #f]
  #:override
  [on-close
   (lambda ()
     (unless abandon?
       (rktio_socket_shutdown rktio fd RKTIO_SHUTDOWN_READ)))]
  [raise-read-error
   (lambda (n)
     (raise-network-error #f n "error reading from stream port"))]
  #:property
  [prop:file-stream #f]
  [prop:fd-place-message-opener (lambda (fd name)
                                  (make-tcp-input-port fd name))])

(define (make-tcp-input-port fd name
                             #:fd-refcount [fd-refcount (box 1)])
  (finish-fd-input-port
   (new tcp-input-port
        #:field
        [name name]
        [fd fd]
        [fd-refcount fd-refcount])))

(class tcp-output-port #:extends fd-output-port
  #:field
  [abandon? #f]
  #:override
  [on-close
   (lambda ()
     (unless abandon?
       (rktio_socket_shutdown rktio fd RKTIO_SHUTDOWN_WRITE)))]
  [raise-write-error
   (lambda (n)
     (raise-network-error #f n "error writing to stream port"))]
  #:property
  [prop:file-stream #f]
  [prop:fd-place-message-opener (lambda (fd name)
                                  (make-tcp-output-port fd name))])

(define (make-tcp-output-port fd name
                              #:fd-refcount [fd-refcount (box 1)])
  (finish-fd-output-port
   (new tcp-output-port
        #:field
        [name name]
        [fd fd]
        [fd-refcount fd-refcount]
        [buffer-mode 'block])))

;; ----------------------------------------

(define (open-input-output-tcp fd name #:close? [close? #t])
  (define refcount (box (if close? 2 3)))
  (values
   (make-tcp-input-port fd name
                        #:fd-refcount refcount)
   (make-tcp-output-port fd name
                         #:fd-refcount refcount)))

(define/who (tcp-port? p)
  (define cp (or (->core-input-port p #:default #f)
                 (->core-output-port p #:default #f)))
  (or (tcp-input-port? cp)
      (tcp-output-port? cp)))

(define/who (tcp-abandon-port p)
  (define cp (or (->core-input-port p #:default #f)
                 (->core-output-port p #:default #f)))
  (cond
    [(tcp-input-port? cp)
     (set-tcp-input-port-abandon?! cp #t)
     (close-port p)]
    [(tcp-output-port? cp)
     (set-tcp-output-port-abandon?! cp #t)
     (close-port p)]))
