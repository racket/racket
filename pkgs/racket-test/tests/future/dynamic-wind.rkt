#lang racket/base
(require racket/future)

;; Make sure that suspending/resuming a future doesn't run its post/pre thunks

(define ns (make-vector 50 0))
(define bs (make-vector (vector-length ns) #f))

(define fs
  (for/list ([i (in-range (vector-length ns))])
    (future
     (lambda ()
       (dynamic-wind
        (lambda ()
          (vector-set! ns i (add1 (vector-ref ns i))))
        (lambda ()
          (memory-order-release)
          (vector-set! bs i #t)
          (current-continuation-marks))
        (lambda ()
          (vector-set! ns i (sub1 (vector-ref ns i)))))))))

(sleep 0.1)
(memory-order-acquire)

(for ([i (in-range (vector-length ns))])
  (when (and (vector-ref bs i)
             (zero? (vector-ref ns i)))
    (error "looks like a suspended future ran its post thunk")))

(for-each touch fs)

(for ([i (in-range (vector-length ns))])
  (unless (and (vector-ref bs i)
               (zero? (vector-ref ns i)))
    (error "future does not seem to have completed correctly")))
