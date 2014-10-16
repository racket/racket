#lang racket/base

(require racket/date
         racket/match
         racket/function)

(provide benchmark-logging-to
         log-counterexample
         log-new-avg
         log-gen-timeout
         log-check-timeout
         log-start
         log-finished
         log-heartbeat
         timestamp
         bmark-log
         bmark-log-directory
         read-logfile
         make-event-filter
         datum-selector)

(struct bmark-log-data (data))
(struct bmark-log-end ())
(struct gc-info (major? pre-amount pre-admin-amount code-amount
                        post-amount post-admin-amount
                        start-process-time end-process-time
                        start-time end-time)
  #:prefab)

(define bmark-log-directory (make-parameter #f))

(define (benchmark-logging-to filename thunk)
  (define bmark-log-recv
    (make-log-receiver (current-logger) 'debug))
  (define handler (log-handler filename))
  (define res-chan (make-channel))
  (define exn-chan (make-channel))
  (define body-thd
    (thread (λ () 
              (with-handlers ([exn:fail? (λ (exn) (channel-put exn-chan exn))])
                (channel-put res-chan (thunk))))))
  (define result (void))
  (let loop ()
    (sync
     (handle-evt exn-chan
                 (λ (exn) (raise exn)))
     (handle-evt res-chan
                 (λ (result-of-thunk)
                   (set! result result-of-thunk)
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
                      result]
                     [else
                      (handler data)
                      (loop)]))))))

(define (log-handler filename)
  (λ (data)
    (match data
      [(bmark-log-data data)
       (define fpath (if (bmark-log-directory)
                         (path->string
                          (build-path (bmark-log-directory) 
                                      (string->path filename)))
                         filename))
       (call-with-output-file fpath
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
             `(#:model ,model
               #:type ,gen
               #:counterexample ,cexp
               #:iterations ,tries
               #:time ,time)))

(define (log-new-avg model gen avg err)
  (bmark-log 'new-average
             `(#:model ,model
               #:type ,gen
               #:average ,avg
               #:stderr ,err)))

(define (log-gen-timeout model gen)
  (bmark-log 'timeout
             `(#:during 'generation
               #:model ,model
               #:type ,gen)))

(define (log-check-timeout model gen term)
  (bmark-log 'timeout
             `(#:during 'check
               #:term ,term
               #:model ,model
               #:type ,gen)))

(define (log-start model gen)
  (bmark-log 'start
             `(#:model ,model
               #:type ,gen)))

(define (log-finished model gen time tries countxmps)
  (bmark-log 'finished
             `(#:model ,model
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
             `(#:model ,model
               #:type ,gen)))

(define path-format (compose string->symbol path->string))

(define (bmark-log event data)
  (log-message (current-logger) 'info "BENCHMARK-LOGGING" 
               (bmark-log-data `(,event ,(timestamp) ,data))))
            

(define (timestamp)
  (parameterize ([date-display-format 'iso-8601])
    (string->symbol (date->string (current-date) #t))))

(define (read-logfile logfile)
  (call-with-input-file logfile
    (λ (in)
      (reverse
       (let loop ([log '()])
         (define next (read in))
         (if (eof-object? next)
             log
             (loop (cons next log))))))))

(define (make-event-filter event)
  (λ (ld)
    (match ld
      [(list (? ((curry eq?) event)) time data) #t]
      [_ #f])))

(define (datum-selector kw)
  (λ (ld)
    (match ld
      [(list event time (list head ... (? ((curry eq?) kw)) datum rest ...))
       datum])))