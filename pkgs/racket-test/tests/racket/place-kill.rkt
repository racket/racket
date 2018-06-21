#lang racket/base
(require racket/place)

;; Make sure `place-kill` doesn't return until the place has
;; completed.

(define (go)
  (place p
         (define bstr (place-channel-get p))
         (let loop ()
           (bytes-set! bstr 0 (bytes-ref bstr 1))
           (loop))))

(module+ main
  (define bstr (make-shared-bytes 2))
  (define p (go))
  (place-channel-put p bstr)
  (sleep 2)
  (place-kill p)
  (bytes-set! bstr 1 2)
  (sleep 0.1)
  (unless (equal? 0 (bytes-ref bstr 0))
    (error 'place-kill-test "place-kill returned before the place completed")))

;; ----------------------------------------

;; Also make sure a place doesn't keep running because the current
;; thread isn't the main thread when the place is killed.

(define (go2)
  (place p
         (define bstr (place-channel-get p))
         (for ([i 10])
           (thread (lambda () 
                     (let loop ()
                       (bytes-set! bstr 0 (bytes-ref bstr 1))
                       (loop)))))
         (sleep)
         (place-channel-put p 'ready)
         (sync (make-semaphore))))

(module+ main
  (define bstr2 (make-shared-bytes 2))
  (define p2 (go2))
  (place-channel-put p2 bstr2)
  (unless (eq? 'ready (place-channel-get p2)) (error "didn't get 'ready"))
  (place-kill p2)
  (bytes-set! bstr2 1 2)
  (sleep 0.1)
  (unless (equal? 0 (bytes-ref bstr2 0))
    (error 'place-kill-test "place-kill doesn't seem to have terminated threads")))
