#lang racket/base 
(require rackunit 
         racket/future 
         future-visualizer/private/visualizer-data 
         (for-syntax racket/base 
                     future-visualizer/private/visualizer-data)
         (only-in future-visualizer/trace trace-futures)) 

#|

Invariants: 
-It is not possible for a future to have an empty timeline, even if it never 
 executes on anything other than the runtime thread.
-Cannot have two consecutive 'start-work events on the same future, same thread.
-For each future created there must exist exactly one 'create event. 
-The number of events matching synchronization-event? must always match the 
 number of runtime-synchronization-event? events.

|#

;Stable sort by timestamp, then check 
;index ordering
(define-syntax (check-ordering stx) 
  (syntax-case stx () 
    [(_ log) 
     (with-syntax ([line (syntax-line stx)]) 
       #'(let ([time-sorted (sort log 
                                  #:key (λ (e) (future-event-time (indexed-future-event-fevent e))) 
                                  <)])
           (let loop ([sorted time-sorted]) 
             (define cur (car sorted))
             (define rest (cdr sorted))
             (unless (null? rest)
               (define next (car rest)) 
               (define curtime (future-event-time (indexed-future-event-fevent cur)))
               (define curind (indexed-future-event-index cur)) 
               (define nextind (indexed-future-event-index next))
               (define nexttime (future-event-time (indexed-future-event-fevent cur))) 
               (check-true (< curind 
                              nextind) 
                           (format 
                            "Incorrect event ordering at line ~a: event with (index=~a, time=~a) 
                                occurs before event with (index=~a, time=~a)\n" 
                            line 
                            curind 
                            curtime 
                            nextind 
                            nexttime))
               (loop rest)))))]))


(define log1 (parameterize ([current-output-port (open-output-string)])
               (trace-futures 
                (let ([fs (for/list ([i (in-range 0 1000)]) 
                            (future (λ () 
                                      (printf "hello\n"))))]) 
                  (sleep 0.1)
                  (map touch fs)))))
(check-true (> (length log1) 2000)) 
;Event types 
(check-equal? (length (filter (λ (e) (and (synchronization-event? e) 
                                          (equal? (op-name e) 'printf))) 
                              log1)) 
                      1000)
(define syncs-len (length (filter synchronization-event? log1))) 
(check-true (>= syncs-len 1000)) 
(check-true (<= syncs-len 2000))
(check-ordering log1)
(define tr1 (build-trace log1)) 
;Keys should include all unique future id's, and one entry for #f 
;(events logged on runtime thread outside scope of any future)
(check-equal? (length (hash-keys (trace-future-timelines tr1))) 1001)


(define log3 (trace-futures 
              (parameterize ([current-command-line-arguments #("2000")]
                             [current-output-port (open-output-string)])
                (void (dynamic-require 'tests/racket/benchmarks/shootout/mandelbrot-futures #f))))) 
(check-true (> (length log3) 0))
(define tr3 (build-trace log3)) 
(check-equal? (length (hash-keys (trace-future-timelines tr3))) 2001)


(define log4 (trace-futures 
              (let ([f (future (λ () 
                                 (for/list ([i (in-range 0 10000)]) 
                                   (cons i (+ i 1)))))]) 
                (sleep 0.5)
                (touch f)))) 
(check-true (> (length log4) 0))
(check-true (list? (memf allocation-event? log4)) "No allocation events found in log4")
(define ae (findf allocation-event? log4)) 
(check-true (allocation-event? ae)) 
(check-true (runtime-synchronization-event? ae))