#lang racket/base
(require racket/place)

(provide place-worker
         main)

(define (place-worker ch)
  (random-seed (current-seconds))
  ;(define id (place-channel-get ch))
  (define id "HI")
  (for ([i (in-range (+ 5  (random 5)))])
    (displayln (list (current-seconds) id i))
    (flush-output)
    (place-channel-put ch (list (current-seconds) id i))
    (sleep 3)))

;(define-values (p1 p2) (place-channel))
;(place-worker p1)

(define  (main . argv)
  (define p (place ch
  (random-seed (current-seconds))
  ;(define id (place-channel-get ch))
  (define id "HI")
  (for ([i (in-range (+ 5  (random 5)))])
    (displayln (list (current-seconds) id i))
    (flush-output)
    ;(place-channel-put ch (list (current-seconds) id i))
    #;(sleep 3))))
  (sync (handle-evt (place-dead-evt p) (lambda (e) (printf "DEAD\n")))))
