#lang racket
(require racket/place)

;; Check the interaction of custodian memory limits with
;;  * allocating shared arrays, and 
;;  * putting messages into an channel with no receiver

(define (check shared?)
  (for ([i 20])
    (printf "iter ~a\n" i)
    (let ([c (make-custodian)])
      (custodian-limit-memory c (* 1024 1024 10) c)
      (parameterize ([current-custodian c])
        (thread-wait
         (thread
          (lambda ()
            (define-values (a b) (place-channel))
            (for/fold ([v 0]) ([i (in-range 999999999)]) 
              (if shared?
                  (cons (make-shared-bytes 1024) v)
                  (place-channel-put b (list 1 2 3 4)))))))))))

(check #t)
(check #t)
