#lang racket/base
(require racket/place)

;; This test (by Eric Dobson) effectively checks synchronization
;; of GC among places. The `holder` place waits until the main
;; place triggers a shared-space GC due to a growing message channel.
;; Since the `holder` place can reach the channel, the GC can go
;; bad if the `holder` place traverses the channel while the other
;; place is still installing new message-array (so that the channel
;; is marked already by the GC, but its content changes).

(define (holder)
  (place chan
    (let loop ([v #f])
      (define v2 (sync chan))
      (place-channel-put chan v)
      (loop v2))))

(module+ main
  (define other (holder))
  (define exec (make-will-executor))
  (define exec-thread
    (thread
     (lambda ()
       (let loop ()
         (will-execute exec)
         (loop)))))
  (define in
    (let-values ([(in out) (place-channel)])
      (place-channel-put other out)
      in))
  (collect-garbage)
  ;; Let will execute
  (sleep 1)
  (/ (current-memory-use) 1000000.0)
  (for ((i 1000000))
    (place-channel-put in i)))

(module+ test
  (require (submod ".." main)))
