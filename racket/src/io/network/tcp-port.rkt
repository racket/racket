#lang racket/base
(require "../common/check.rkt"
         "../host/rktio.rkt"
         "../port/port.rkt"
         "../port/input-port.rkt"
         "../port/output-port.rkt"
         "../port/fd-port.rkt")

(provide open-input-output-tcp
         tcp-port?
         tcp-abandon-port)

(struct tcp-data (abandon-in? abandon-out?)
  #:mutable
  #:authentic)

(define (open-input-output-tcp fd name #:close? [close? #t])
  (define refcount (box (if close? 2 3)))
  (define extra-data (tcp-data #f #f))
  (values
   (open-input-fd fd name
                  #:extra-data extra-data
                  #:on-close
                  ;; in atomic mode
                  (lambda ()
                    (unless (tcp-data-abandon-in? extra-data)
                      (rktio_socket_shutdown rktio fd RKTIO_SHUTDOWN_READ)))
                  #:fd-refcount refcount)
   (open-output-fd fd name
                   #:extra-data extra-data
                   #:on-close
                   ;; in atomic mode
                   (lambda ()
                     (unless (tcp-data-abandon-out? extra-data)
                       (rktio_socket_shutdown rktio fd RKTIO_SHUTDOWN_WRITE)))
                   #:fd-refcount refcount
                   #:buffer-mode 'block)))

(define (port-tcp-data p)
  (maybe-fd-data-extra 
   (cond
     [(input-port? p)
      (core-port-data
       (->core-input-port p))]
     [(output-port? p)
      (core-port-data
       (->core-output-port p))]
     [else #f])))

(define/who (tcp-port? p)
  (tcp-data? (port-tcp-data p)))

(define/who (tcp-abandon-port p)
  (define data (port-tcp-data p))
  (unless (tcp-data? data)
    (raise-argument-error who "tcp-port?" p))
  (if (input-port? p)
      (set-tcp-data-abandon-in?! data #t)
      (set-tcp-data-abandon-out?! data #t)))
