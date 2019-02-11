#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt")

(provide port-closed?
         close-input-port
         close-output-port
         port-closed-evt

         close-port
         set-closed-state!)

(define (port-closed? p)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else
              (raise-argument-error 'close-input-port "port?" p)])])
    (core-port-closed? p)))

;; maybe in atomic mode via custodian shutdown:
(define (close-port p)
  (unless (core-port-closed? p)
    (atomically
     (send core-port p close)
     (set-closed-state! p))))

;; in atomic mode
(define (set-closed-state! p)
  (unless (core-port-closed? p)
    (set-core-port-closed?! p #t)
    (let ([s (core-port-closed-sema p)])
      (when s (semaphore-post s)))))

(define/who (close-input-port p)
  (check who input-port? p)
  (close-port (->core-input-port p)))

(define/who (close-output-port p)
  (check who output-port? p)
  (close-port (->core-output-port p)))

(define (port-closed-evt p)
  (let ([p (cond
             [(input-port? p) (->core-input-port p)]
             [(output-port? p) (->core-output-port p)]
             [else
              (raise-argument-error 'port-closed-evt "port?" p)])])
    (define sema
      (atomically
       (or (core-port-closed-sema p)
           (let ([s (make-semaphore)])
             (set-core-port-closed-sema! p s)
             (when (core-port-closed? p)
               (semaphore-post s))
             s))))
    (define self (wrap-evt (semaphore-peek-evt sema)
                           (lambda (v) self)))
    self))
