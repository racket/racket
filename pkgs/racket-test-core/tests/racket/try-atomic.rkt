#lang racket/base
(require ffi/unsafe/try-atomic)

(define ch (make-channel))
(define done? #f)

(define (check v expect)
  (unless (equal? v expect)
    (error 'check "failed: ~s vs. ~s" v expect)))

(check
 (call-as-nonatomic-retry-point
  (lambda ()
    (try-atomic
     (lambda ()
       (sync/timeout 0.25 ch)
      (set! done? #t))
     'no)))
 'no)

(check done? #t)
