#lang racket/base
(require "../common/check.rkt"
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
    (closed-state-closed? (core-port-closed p))))

;; maybe in atomic mode via custodian shutdown:
(define (close-port p)
  (define closed (core-port-closed p))
  (unless (closed-state-closed? closed)
    (atomically
     ((core-port-close p))
     (set-closed-state! closed))))

;; in atomic mode
(define (set-closed-state! closed)
  (unless (closed-state-closed? closed)
    (set-closed-state-closed?! closed #t)
    (let ([s (closed-state-closed-sema closed)])
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
    (define closed (core-port-closed p))
    (define sema
      (atomically
       (or (closed-state-closed-sema closed)
           (let ([s (make-semaphore)])
             (set-closed-state-closed-sema! closed s)
             (when (closed-state-closed? closed)
               (semaphore-post s))
             s))))
    (define self (wrap-evt (semaphore-peek-evt sema)
                           (lambda (v) self)))
    self))
