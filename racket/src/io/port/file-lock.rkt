#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "file-stream.rkt"
         "check.rkt")

(provide port-try-file-lock?
         port-file-unlock)

(define/who (port-try-file-lock? p mode)
  (check who file-stream-port? p)
  (check who (lambda (m) (or (eq? m 'shared) (eq? m 'exclusive)))
         #:contract "(or/c 'shared 'exclusive)"
         mode)
  (define exclusive? (eq? mode 'exclusive))
  (when (and exclusive? (not (output-port? p)))
    (raise-arguments-error who "port for 'exclusive locking is not an output port"
                           "port" p))
  (when (and (not exclusive?) (not (input-port? p)))
    (raise-arguments-error who "port for 'shared locking is not an input port"
                           "port" p))
  (define cp (cond
               [(input-port? p) (->core-input-port p)]
               [else (->core-output-port p)]))
  (start-atomic)
  (check-not-closed who cp)
  (define fd ((file-stream-ref cp) cp))
  (define r (rktio_file_lock_try rktio fd exclusive?))
  (end-atomic)
  (when (rktio-error? r)
    (raise-rktio-error who
                       r
                       (string-append "error getting file "
                                      (if exclusive? "exclusive" "shared")
                                      " lock")))
  (eqv? r RKTIO_LOCK_ACQUIRED))

(define/who (port-file-unlock p)
  (check who file-stream-port? p)
  (define cp (cond
               [(input-port? p) (->core-input-port p)]
               [else (->core-output-port p)]))
  (start-atomic)
  (check-not-closed who cp)
  (define fd ((file-stream-ref cp) cp))
  (define r (rktio_file_unlock rktio fd))
  (end-atomic)
  (when (rktio-error? r)
    (raise-rktio-error who r "error unlocking file")))
