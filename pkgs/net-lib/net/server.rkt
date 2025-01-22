#lang racket/base

(require racket/contract/base
         racket/tcp)

(provide
 (contract-out
  [start-server (server-proc/c (-> void?))]
  [run-server (server-proc/c void?)]))

(define (server-proc/c res/c)
  (->* [evt? (-> input-port? output-port? any)]
       [#:max-concurrent   (or/c +inf.0 natural-number/c)
        #:timeout-evt-proc (-> thread? input-port? output-port? boolean? evt?)
        #:accept-proc      (-> any/c (values input-port? output-port?))
        #:close-proc       (-> any/c void?)]
       res/c))

(define (start-server listen-evt handle
                      #:max-concurrent   [max-concurrent +inf.0]
                      #:timeout-evt-proc [make-timeout-evt (λ (_thd _in _out _break-sent?) never-evt)]
                      #:accept-proc      [accept tcp-accept]
                      #:close-proc       [close tcp-close])
  (define can-break?
    (break-enabled))
  (define paramz
    (current-parameterization))
  (define server-thd
    (thread
     (lambda ()
       (dynamic-wind
         void
         (lambda ()
           (with-handlers ([exn:break? void])
             (let loop ([in-progress 0])
               (loop
                (with-handlers ([exn:fail:network?
                                 (λ (e)
                                   (begin0 in-progress
                                     ((error-display-handler)
                                      (format "Connection error: ~a" (exn-message e))
                                      e)))])
                  (sync/enable-break
                   (handle-evt
                    (thread-receive-evt)
                    (lambda (_)
                      (let drain-loop ([in-progress in-progress])
                        (if (thread-try-receive)
                            (drain-loop (sub1 in-progress))
                            in-progress))))
                   (handle-evt
                    (if (< in-progress max-concurrent) listen-evt never-evt)
                    (lambda (listener)
                      (define supervisor-cust (make-custodian))
                      (define client-cust (make-custodian supervisor-cust))
                      (parameterize ([current-custodian client-cust])
                        (parameterize-break #f
                          (define-values (in out)
                            (accept listener))
                          (define client-thd
                            (thread
                             (lambda ()
                               (call-with-parameterization
                                paramz
                                (lambda ()
                                  (break-enabled can-break?)
                                  ;; Create an intermediary custodian to prevent `handle` from
                                  ;; shutting down `client-cust`.
                                  (parameterize ([current-custodian (make-custodian client-cust)])
                                    (handle in out)))))))
                          (define _supervisor-thd
                            ;; Under supervisor-cust so we can shut down client-cust before we
                            ;; report that this connection is finished, which means supervisor-thd
                            ;; must continue running after client-cust is shut down.
                            (parameterize ([current-custodian supervisor-cust])
                              (thread
                               (lambda ()
                                 (sync client-thd (make-timeout-evt client-thd in out #f))
                                 (when (thread-running? client-thd)
                                   (break-thread client-thd)
                                   (sync client-thd (make-timeout-evt client-thd in out #t)))
                                 (custodian-shutdown-all client-cust)
                                 (thread-send server-thd 'done void)
                                 (custodian-shutdown-all supervisor-cust)))))
                          (add1 in-progress)))))))))))
         (lambda ()
           (close listen-evt))))))
  (lambda ()
    (break-thread server-thd)
    (thread-wait server-thd)))

(define run-server
  (let-values ([(required-kws optional-kws) (procedure-keywords start-server)])
    (procedure-reduce-keyword-arity-mask
     (make-keyword-procedure
      (lambda (kws kw-args . args)
        (parameterize-break #f
          (define stop (keyword-apply start-server kws kw-args args))
          (with-handlers ([exn:break? void])
            (sync/enable-break never-evt))
          (stop))))
     (procedure-arity-mask start-server)
     required-kws
     optional-kws
     'run-server)))
