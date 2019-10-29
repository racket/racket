#lang racket
(require racket/place)

;; Check the interaction of custodian memory limits with
;;  * allocating shared arrays, and 
;;  * putting messages into an channel with no receiver

(module+ test
  (module config info
    (define timeout 200)))

(define (check mode)
  (for ([i 20])
    (printf "iter ~a\n" i)
    (collect-garbage)
    (let ([c (make-custodian)])
      (custodian-limit-memory c (* 1024 1024 10) c)
      (parameterize ([current-custodian c])
        (thread-wait
         (thread
          (lambda ()
            (define-values (a b) (place-channel))
            (list
             (for/fold ([v 0]) ([i (in-range 999999999)])
               (case mode
                 [(bytes)
                  ;; Not really about places or channels, but worth checking, too
                  (cons (make-bytes 1024) v)]
                 [(shared-bytes)
                  (cons (make-shared-bytes 1024) v)]
                 [(messages)
                  (place-channel-put b (list 1 2 3 4 (make-vector 500)))]))
             (log-error "shouldn't get done")
             ;; Remember `a`, just in case the runtime system is smart
             ;; enough to discard messages that have no destination
             a))))))))

(check 'bytes)
(check 'shared-bytes)
(check 'messages)
