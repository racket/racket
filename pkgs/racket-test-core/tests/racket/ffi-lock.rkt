#lang racket
(require ffi/unsafe)

(define (go)
  (place pch (s)))

(define (s)
  (define sleep (get-ffi-obj 'sleep #f (_fun #:lock-name "sleep-lock" _int -> _void)))
  (printf "start\n")
  (sleep 1)
  (printf "done\n"))

(module+ test
  (when (memq (system-type) '(unix macosx))
    (place-wait (go))
    ;; Trigger a GC in space of place-shared objects:
    (for ([i 1000])
      (place-channel))
    (define now (current-seconds))
    (define l
      (for/list ([i 3])
        (go)))
    (map place-wait l)
    (unless ((- (current-seconds) now) . >= . 3)
      (error "didn't serialize"))))

