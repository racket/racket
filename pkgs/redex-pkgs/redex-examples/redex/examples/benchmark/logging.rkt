#lang racket

(require racket/date)

(provide with-logging-to
         log-counterexample
         log-new-avg
         log-gen-timeout
         log-check-timeout
         log-start
         log-finished
         log-heartbeat)

(struct bmark-log-data (data))
(struct bmark-log-end ())
(struct gc-info (major? pre-amount pre-admin-amount code-amount
                        post-amount post-admin-amount
                        start-process-time end-process-time
                        start-time end-time)
  #:prefab)

(define (with-logging-to filename thunk)
  (define bmark-log-recv
    (make-log-receiver (current-logger) 'debug))
  (define handler (log-handler filename))
  (define body-thd
    (thread thunk))
  (let loop ()
    (sync
     (handle-evt body-thd
                 (λ (_)
                   (log-message (current-logger) 'info "STOP" (bmark-log-end))
                   (loop)))
     (handle-evt bmark-log-recv 
                 (λ (log-evt)
                   (define data (vector-ref log-evt 2))
                   (match data
                     [(? gc-info? d)
                      (handle-gc-log-evt d)
                      (loop)]
                     [(bmark-log-end)
                      (void)]
                     [else
                      (handler data)
                      (loop)]))))))

(define (log-handler filename)
  (λ (data)
    (match data
      [(bmark-log-data data)
       (call-with-output-file filename
         (λ (log-port)
           (write data log-port)
           (newline log-port))
         #:exists 'append)]
      [_ (void)])))

(define (handle-gc-log-evt gci)
  (when (gc-info-major? gci)
    (bmark-log 'gc-major
               `(#:amount ,(- (gc-info-pre-amount gci) (gc-info-post-amount gci))
                 #:time ,(- (gc-info-end-process-time gci) (gc-info-start-process-time gci))))))

(define (log-counterexample model gen cexp tries time)
  (bmark-log 'counterexample
             `(#:model ,(path-format model)
               #:type ,gen
               #:counterexample ,cexp
               #:iterations ,tries
               #:time ,time)))

(define (log-new-avg model gen avg dev)
  (bmark-log 'new-average
             `(#:model ,(path-format model)
               #:type ,gen
               #:average ,avg
               #:stddev ,dev)))

(define (log-gen-timeout model gen)
  (bmark-log 'timeout
             `(#:during 'generation
               #:model ,(path-format model)
               #:type ,gen)))

(define (log-check-timeout model gen term)
  (bmark-log 'timeout
             `(#:during 'check
               #:term ,term
               #:model ,(path-format model)
               #:type ,gen)))

(define (log-start model gen)
  (bmark-log 'start
             `(#:model ,(path-format model)
               #:type ,gen)))

(define (log-finished model gen time tries countxmps)
  (bmark-log 'finished
             `(#:model ,(path-format model)
               #:type ,gen
               #:time-ms ,time
               #:attempts ,tries
               #:num-counterexamples ,countxmps
               #:rate-terms/s ,(exact->inexact (/ tries (/ time 1000)))
               #:attempts/cexp ,(if (zero? countxmps)
                                                'N/A
                                                (exact->inexact (/ tries countxmps))))))

(define (log-heartbeat model gen)
  (bmark-log 'heartbeat
             `(#:model ,(path-format model)
               #:type ,gen)))

(define path-format (compose string->symbol path->string))

(define (bmark-log event data)
  (log-message (current-logger) 'info "BENCHMARK-LOGGING" 
               (bmark-log-data `(,event ,(timestamp) ,data))))
            

(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (string->symbol (date->string (current-date) #t))))
