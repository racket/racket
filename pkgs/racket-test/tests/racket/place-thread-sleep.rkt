#lang racket/base
(require racket/place)

;; This is a regression test for a scheduler problem that lost
;; track of the ability for a thread woken up after `alarm-evt`
;; to keep going.

(define (fib n)
  (cond
    [(n . < . 2) 1]
    [else (+ (fib (- n 1)) (fib (- n 2)))]))

(define (go)
  (place pch
         (define th (thread (lambda ()
                              (let loop ()
                                (loop)))))
         (sync (alarm-evt (+ 100 (current-inexact-monotonic-milliseconds)) #t))
         (thread-suspend th)
         (printf "fib...\n")
         (fib 30) ; takes long enough to exhaust quantum
         (printf "done\n")))

(module+ main
  (printf "waiting\n")
  (place-wait (go)))
