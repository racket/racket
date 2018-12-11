#lang racket/base
(require "../common/check.rkt"
         "../host/rktio.rkt"
         "../port/port.rkt"
         "../port/close.rkt"
         "../port/input-port.rkt"
         "../port/output-port.rkt"
         "../port/fd-port.rkt"
         "../port/place-message.rkt")

(provide open-input-output-tcp
         tcp-port?
         tcp-abandon-port)

(struct tcp-data (abandon-in? abandon-out?)
  #:mutable
  #:authentic
  #:property
  prop:fd-extra-data-place-message
  (lambda (port)
    (if (input-port? port)
        (lambda (fd name)
          (open-input-fd fd name
                         #:extra-data (tcp-data #f #t)
                         #:file-stream? #f
			 #:network-error? #t))
        (lambda (fd name)
          (open-output-fd fd name
                          #:extra-data (tcp-data #t #f)
                          #:file-stream? #f
			  #:network-error? #t)))))

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
                  #:fd-refcount refcount
                  #:file-stream? #f
		  #:network-error? #t)
   (open-output-fd fd name
                   #:extra-data extra-data
                   #:on-close
                   ;; in atomic mode
                   (lambda ()
                     (unless (tcp-data-abandon-out? extra-data)
                       (rktio_socket_shutdown rktio fd RKTIO_SHUTDOWN_WRITE)))
                   #:fd-refcount refcount
                   #:buffer-mode 'block
                   #:file-stream? #f
		   #:network-error? #t)))

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

(define/who (tcp-abandon-port given-p)
  (define p (cond
              [(input-port? given-p)
               (->core-input-port given-p)]
              [(output-port? given-p)
               (->core-output-port given-p)]
              [else #f]))
  (define data (port-tcp-data p))
  (unless (tcp-data? data)
    (raise-argument-error who "tcp-port?" p))
  (if (input-port? p)
      (begin
        (set-tcp-data-abandon-in?! data #t)
        (close-port p))
      (begin
        (set-tcp-data-abandon-out?! data #t)
        (close-port p))))
