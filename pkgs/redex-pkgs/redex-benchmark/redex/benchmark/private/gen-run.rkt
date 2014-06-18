#lang racket/base

(require "logging.rkt"
         racket/match
         racket/contract
         math/statistics)

(provide
 (struct-out run-results))

(struct timeout ())
(struct reached-limit (tries) #:transparent)
(struct run-results (tries time cexps) #:transparent)

(provide/contract 
 [run-gen-and-check/mods
  (->* (module-path?
        module-path?
        natural-number/c)
       (#:name string?)
       run-results?)]
 [run-gen-and-check
  (->* ((-> (-> any/c))
        (-> any/c boolean?)
        natural-number/c)
       (#:name string?
        #:type symbol?)
       run-results?)])

(define (run-gen-and-check/mods gen-mod-path check-mod-path seconds #:name [name "unknown"])
  (define get-gen (dynamic-require gen-mod-path 'get-generator))
  (define type (dynamic-require gen-mod-path 'type))
  (define check (dynamic-require check-mod-path 'check))
  (run-generations get-gen check seconds #:name name #:type type))

(define (run-gen-and-check get-gen check seconds
                           #:name [name "unknown"]
                           #:type [type 'unknown])
  (log-start name type)
  (with-heartbeat #:name name #:type type
                  (λ () (run-generations get-gen check seconds
                                         #:name name
                                         #:type type))))

(define (run-generations get-gen check seconds
                  #:name [name "unknown"]
                  #:type [type 'unknown] 
                  #:first-only [first-only #f])
  (collect-garbage)
  (define s-time (current-process-milliseconds))
  (define time-limit (+ s-time (* 1000 seconds)))
  (define counterexamples 0)
  (define (tot-time) (- (current-process-milliseconds) s-time))
  (let trials-loop ([t 0]
                    [g (get-gen)]
                    [stats empty-statistics]
                    [terms 0])
    (define trial-start-time (current-process-milliseconds))
    (define (me-time) (- (current-process-milliseconds) trial-start-time))
    (match (one-counterexample trial-start-time time-limit 
                               g check name type)
      [(timeout)
       (trials-loop t g stats terms)]
      [(reached-limit tries)
       (exit-message name type (+ tries terms) (tot-time) counterexamples)
       (run-results (+ tries terms) (tot-time) counterexamples)]
      [(list tries term)
       (define new-stats (update-statistics stats (me-time)))
       (set! counterexamples (add1 counterexamples))
       (log-counterexample name type term tries (me-time))
       (if (and (not first-only)
                (or (continue? new-stats (add1 t) name type)
                    (t . < . 5)))
           (trials-loop (add1 t) (get-gen) new-stats (+ tries terms))
           (begin
             (exit-message name type (+ tries terms) (tot-time) counterexamples)
             (run-results (+ tries terms) (tot-time) counterexamples)))])))

(define TIMEOUT-DEFAULT (* 5 60 1000)) ;; 5 mins in ms

(define (one-counterexample s-time time-limit generator check fname type [timeout-time TIMEOUT-DEFAULT])
  (let/ec break
    (let loop ([tries 0])
      (when ((current-process-milliseconds) . > . time-limit)
        (break (reached-limit tries))) 
      (define term (with-timeout timeout-time generator
                                 (λ () 
                                   (log-gen-timeout fname type)
                                   (break (timeout)))))
      (define ok? (with-timeout timeout-time (λ () (check term))
                                (λ () 
                                  (log-check-timeout fname type term)
                                  (break (timeout)))
                                (λ (exn)
                                  (print-and-log (format "\nException when calling check with: ~a" term))
                                  (raise exn))))
      (cond
        [(not ok?)
         (list tries term)]
        [else
         (loop (add1 tries))]))))

(define (with-timeout time thunk fail-thunk [on-exn raise])
  (define res-chan (make-channel))
  (define exn-chan (make-channel))
  (define thd (thread (λ () 
                        (with-handlers ([exn:fail? (λ (exn) (channel-put exn-chan exn))])
                          (channel-put res-chan (thunk))))))
  (sync
   (handle-evt (alarm-evt (+ (current-inexact-milliseconds) time))
               (λ (_) 
                 (break-thread thd)
                 (fail-thunk)))
   (handle-evt exn-chan
               (λ (exn) (on-exn exn)))
   (handle-evt res-chan
               (λ (result-of-thunk) result-of-thunk))))

(define (print-and-log str)
  (displayln str)
  (bmark-log 'print str))

(define (with-heartbeat thunk 
                        #:time [time 10]
                        #:type [type #f]
                        #:name [name #f])
  (define res-chan (make-channel))
  (define exn-chan (make-channel))
  (define thd (thread (λ () 
                        (with-handlers ([exn:fail? (λ (exn) (channel-put exn-chan exn))])
                          (channel-put res-chan (thunk))))))
  (define heartbeat-thd
    (thread (λ () 
              (let loop () 
                (log-heartbeat name type) 
                (sleep time)
                (loop)))))
  (sync
   (handle-evt res-chan
               (λ (res) 
                 (kill-thread heartbeat-thd)
                 res))
   (handle-evt exn-chan
               (λ (e)
                 (raise e)))
   (handle-evt heartbeat-thd
               (λ (_) (error 'with-hearbeat "heartbeat thread ended")))))

(define (exit-message file type terms time countxmps)
  (log-finished file type time terms countxmps))


(define (continue? new-stats num-results name type)
  (define avg (statistics-mean new-stats))
  (define dev (/ (statistics-stddev new-stats #:bias #t) (sqrt num-results)))
  (log-new-avg name type (exact->inexact avg) dev)
  (or (= dev 0)
      ((/ dev avg) . > . 0.1)))
  