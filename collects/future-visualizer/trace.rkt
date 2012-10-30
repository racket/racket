#lang racket/base
(require racket/contract
         "private/visualizer-data.rkt")
(provide (struct-out future-event)
         (struct-out gc-info)
         (struct-out indexed-future-event)
         trace-futures
         (contract-out
          [start-future-tracing! (-> void?)]
          [stop-future-tracing! (-> void?)]
          [timeline-events (-> (listof indexed-future-event?))]
          [trace-futures-thunk ((-> any/c) . -> . (listof indexed-future-event?))]))

(define-syntax-rule (trace-futures e ...)
  (begin (start-future-tracing!)
         (begin (begin e ...)
                (stop-future-tracing!)
                (timeline-events))))

;;trace-futures-thunk : (-> any) -> (listof indexed-future-event)
(define (trace-futures-thunk thunk)
  (start-future-tracing!)
  (begin
    (thunk)
    (stop-future-tracing!)
    (timeline-events)))
