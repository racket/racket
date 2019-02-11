#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "port.rkt"
         "input-port.rkt"
         "count.rkt"
         "check.rkt")

(provide (rename-out [progress-evt?* progress-evt?])
         port-provides-progress-evts?
         port-progress-evt
         port-commit-peeked

         check-progress-evt
         unwrap-progress-evt)

(struct progress-evt (port evt)
  #:property prop:evt (lambda (pe)
                        (wrap-evt (progress-evt-evt pe)
                                  (lambda args pe))))

(define progress-evt?*
  (let ([progress-evt?
         (case-lambda
           [(v) (progress-evt? v)]
           [(v port)
            (and (progress-evt? v)
                 (eq? port (progress-evt-port v)))])])
    progress-evt?))

;; ----------------------------------------

(define/who (port-provides-progress-evts? in)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (and (method core-input-port in get-progress-evt) #t)))

(define/who (port-progress-evt orig-in)
  (check who input-port? orig-in)
  (let ([in (->core-input-port orig-in)])
    (define get-progress-evt (method core-input-port in get-progress-evt))
    (if get-progress-evt
        (progress-evt orig-in (get-progress-evt in))
        (raise-arguments-error 'port-progress-evt
                               "port does not provide progress evts"
                               "port" orig-in))))

(define/who (port-commit-peeked amt progress-evt evt [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who progress-evt? progress-evt)
  (check who sync-atomic-poll-evt?
         #:contract "(or/c channel-put-evt? channel? semaphore? semaphore-peek-evt? always-evt never-evt)"
         evt)
  (check who input-port? in)
  (check-progress-evt who progress-evt in)
  (let ([in (->core-input-port in)])
    (atomically
     ;; We specially skip a check on whether the port is closed,
     ;; since that's handled as the progress evt becoming ready
     (send core-input-port in commit
           amt (progress-evt-evt progress-evt) evt
           ;; in atomic mode (but maybe leaves atomic mode in between)
           (lambda (bstr)
             (port-count! in (bytes-length bstr) bstr 0))))))

(define (check-progress-evt who progress-evt in)
  (unless (progress-evt?* progress-evt in)
    (raise-arguments-error who "evt is not a progress evt for the given port"
                           "evt" progress-evt
                           "port" in)))

(define (unwrap-progress-evt progress-evt)
  (and progress-evt
       (progress-evt-evt progress-evt)))
