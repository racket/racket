#lang racket/base 
(require rackunit 
         racket/future 
         future-visualizer/private/visualizer-data 
         (for-syntax racket/base 
                     future-visualizer/private/visualizer-data)
         (only-in future-visualizer/trace trace-futures)
         "vtrace3.rkt") 

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
                                  #:key (λ (e) (event-or-gc-time (indexed-future-event-fevent e))) 
                                  <)])
           (for ([e (in-list time-sorted)]
                 [i (in-naturals)]) 
             (check-equal? (indexed-future-event-index e) 
                           i
                           (format 
                            "Incorrect event ordering at line ~a: event with (index=~a, time=~a) 
                                occurs at actual index ~a\n" 
                            line 
                            (indexed-future-event-index e) 
                            (event-or-gc-time (indexed-future-event-fevent e)) 
                            i)))))]))

;Each future timeline's events sorted in time order?
(define (check-future-timeline-ordering tr)
  (define ftls (hash-values (trace-future-timelines tr)))
  (for ([ftl (in-list ftls)])
    (let loop ([evts ftl]) 
      (cond 
        [(null? (cdr evts)) void]
        [else 
         (check-true (<= (event-start-time (car evts)) (event-start-time (car (cdr evts)))))
         (loop (cdr evts))]))))

(cond 
  [(futures-enabled?)
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
   ;Keys should include all unique future id's, and one entry for #f (no future context, on rt thread)
   ;(events logged on runtime thread outside scope of any future)
   (check-equal? (length (hash-keys (trace-future-timelines tr1))) 1001)
   
   (define log3 (trace-futures 
                 (parameterize ([current-command-line-arguments #("2000")]
                                [current-output-port (open-output-string)])
                   (void (dynamic-require 'tests/racket/benchmarks/shootout/mandelbrot-futures #f))))) 
   (check-true (> (length log3) 0))
   (check-true (list? (memf jitcompile-event? log3)) "No JIT compilation events found in mandelbrot")
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
   
   (check-true (proc-id-or-gc<? 'gc 0)) 
   (check-false (proc-id-or-gc<? 0 'gc)) 
   (check-false (proc-id-or-gc<? 'gc 'gc)) 
   (check-true (proc-id-or-gc<? 0 1)) 
   (check-false (proc-id-or-gc<? 1 0))
   
   (let* ([future-log (list (indexed-future-event 0 (future-event #f 0 'create 0 #f 0)) 
                            (indexed-future-event 1 (future-event 0 1 'start-work 1 #f #f)) 
                            (indexed-future-event 2 (future-event 0 1 'block 2 #f #f))
                            (indexed-future-event 3 (future-event 0 1 'end-work 3 #f #f))
                            (indexed-future-event 4 (future-event 0 0 'block 4 'printf #f))
                            (indexed-future-event 5 (future-event 0 0 'complete 3 #f #f)))]) 
     (define tr (build-trace future-log)) 
     (define blocks (filter worker-block-event? (trace-all-events tr)))
     (for ([b (in-list blocks)])
       (check-true (event? (event-block-handled-event b))) 
       (check-true (symbol? (event-prim-name b)))))
   
   (let ([tr (build-trace vtrace-3)]) 
     (check-future-timeline-ordering tr))]
  [else 
   (define l (trace-futures (let ([f (future (λ () (printf "hello\n")))]) 
                              (sleep 0.1) 
                              (touch f)))) 
   (check-equal? l '())])
