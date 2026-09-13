#lang racket/base
(require ffi2
         racket/future
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

(define-ffi2-type to_double_t (int_t float_t . -> . double_t
                                     #:collect-safe))

(define-test-procedure double_built (to_double_t . -> . double_t))
(define-test-procedure double_built_collect_safe (to_double_t . -> . double_t
                                                              #:collect-safe)
  #:c-id double_built)
(define-test-procedure sleep_seconds (int_t . -> . void_t #:collect-safe))

(define N 1024)

(for ([i (in-range N)])
  (check-equal? (double_built (lambda (i f) (+ i f))) 220.0)
  (check-equal? (double_built_collect_safe (lambda (i f) (+ i f))) 220.0))

(when (futures-enabled?)
  (define sleep-time 100)
  (define start-time (current-seconds))
  (thread #:pool 'own
          (lambda ()
            (sleep_seconds sleep-time)))
  (sleep 1.0)
  (collect-garbage)
  (unless ((- (current-seconds) start-time) . < . sleep-time)
    (error "GC blocked too long")))

(clean-ffi2-lib)
