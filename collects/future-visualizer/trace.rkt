#lang racket/base
(require racket/contract 
         "private/visualizer-data.rkt")
(provide (struct-out future-event) 
         (struct-out indexed-future-event) 
         trace-futures 
         (contract-out
          [start-performance-tracking! (-> void?)]
          [timeline-events (-> (listof indexed-future-event?))]
          [trace-futures-thunk ((-> any/c) . -> . (listof indexed-future-event?))]))

(define-syntax-rule (trace-futures e ...) 
  (begin (start-performance-tracking!) 
         (begin (begin e ...) 
                (timeline-events))))

;;trace-futures-thunk : (-> any) -> (listof indexed-future-event)
(define (trace-futures-thunk thunk) 
  (start-performance-tracking!) 
  (begin
    (thunk) 
    (timeline-events)))