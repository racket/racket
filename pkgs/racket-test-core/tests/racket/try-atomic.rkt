#lang racket/base
(require ffi/unsafe/try-atomic
         ffi/unsafe/atomic)

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

(set! done? #f)

(check
 (call-as-nonatomic-retry-point
  (lambda ()
    (try-atomic
     (lambda ()
       (call-with-continuation-barrier
        (lambda ()
          (sync/timeout 0.25 ch)))
       (set! done? #t))
     'barrier)))
 'barrier)

(check done? #t)

(let ()
  (define finished? #f)
  (define oops? #f)

  (call-as-nonatomic-retry-point
   (lambda ()
     (try-atomic
      (lambda ()
        (start-atomic)
        (define done (+ (current-milliseconds) 300))
        (let loop ()
          (unless ((current-milliseconds) . >= . done)
            (loop)))
        (set! finished? #t)
        (end-atomic))
      (void))
     (unless finished?
       (set! oops? #t))))

  (when oops?
    (error "nested atomic mode interrupted within try-atomic")))

(let ()
  (define finished? #f)
  (define stop? #f)
  (define oops? #f)

  (call-as-nonatomic-retry-point
   (lambda ()
     (try-atomic
      (lambda ()
        (define done (+ (current-milliseconds) 10000))
        (let loop ()
          (unless (or stop? ((current-milliseconds) . >= . done))
            (loop)))
        (set! finished? #t))
      (void))
     (set! stop? #t)
     (when finished?
       (set! oops? #t))))

  (when oops?
    (error "try-atomic never interrupted")))
