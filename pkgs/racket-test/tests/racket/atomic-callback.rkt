#lang racket/base
(require ffi/unsafe
         ffi/unsafe/atomic)

(define (take-a-while)
  ;; intended to run long enough to hit a thread-swap interrupt
  ;; and also to create work that will cause the scheduler
  ;; to go into atomic mode (to clean up unreachable custodians) after
  ;; the callback returns
  (for/fold ([v #f]) ([i (in-range 10000)])
    (define c (make-custodian (make-custodian)))
    (make-custodian-box c 'content)))

(define take-a-while-ptr (cast take-a-while (_fun #:atomic? #t -> _void) _pointer))
(define take-a-while-as-callback (cast take-a-while-ptr _pointer (_fun #:atomic? #t -> _void)))

(for ([i 100])
  (take-a-while-as-callback)
  ;; these two steps try to provoke the scheduler into going
  ;; into a redudant atomic mode, which is ok unless there's
  ;; an action still associated to exiting atomic mode
  (collect-garbage)
  (sleep))
