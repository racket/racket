#lang racket/base
(require racket/place)

;; Make sure multiple places can compete for a lock that is
;; implemented by a place channel. This test turns out to also make
;; sure that the link to the read end of a message queue is not too
;; weak when a suspend thread is waiting on the channel.

(define (lock-via-channel lock-ch)
  (let ([saved-ch #f])
    (lambda (mode)
      (case mode
        [(lock)
         (define ch (sync lock-ch))
         (place-channel-put ch 'lock)
         (set! saved-ch ch)]
        [(unlock)
         (place-channel-put saved-ch 'done)
         (set! saved-ch #f)]))))

(define (go)
  (place pch
         #;(printf "start\n")
         (define lock-ch (place-channel-get pch))
         (define lock (lock-via-channel lock-ch))
         (for ([i (in-range 100)])
           #;(printf "~s\n" i)
           (lock 'lock)
           (lock 'unlock))
         #;(printf "done\n")))

(module+ main
  (define-values (lock-ch lock-ch-in) (place-channel))
  (thread (lambda ()
            (define-values (ch ch-in) (place-channel))
            (let loop ()
              (place-channel-put lock-ch-in ch)
              (unless (eq? (place-channel-get ch-in) 'lock)
                (error "bad lock"))
              (unless (eq? (place-channel-get ch-in) 'done)
                (error "bad unlock"))
              (loop))))

  (define ps
    (for/list ([i (in-range 4)])
      (define p (go))
      (place-channel-put p lock-ch)
      p))

  (map place-wait ps))
         
(module+ test (require (submod ".." main)))
