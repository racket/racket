#lang racket

(require "places.rkt")

(require racket/place data/queue racket/async-channel)
(provide generate-log start-workers run-in-other-place places verbose?)

(define places (make-parameter (and (place-enabled?) (min 8 (processor-count)))))

(define-values (enq-ch deq-ch) (place-channel))
(define (start-workers)
  (when (places)
    (for ([i (places)])
      (start-worker deq-ch i))))

(define (run-in-other-place p* [error? #f])
  (define-values (res-ch res-ch*) (place-channel))
  (place-channel-put enq-ch (vector p* res-ch* error?))
  (delay/thread
   (define res (place-channel-get res-ch))
   (when (s-exn? res)
     (raise (deserialize-exn res)))))


(define (generate-log name dir)
  (cond [(places) 
         (define-values (res-ch res-ch*) (place-channel))
         (place-channel-put enq-ch (vector 'log name dir res-ch*))
         (place-channel-get res-ch)]
        [else
         (generate-log/place name dir)]))
