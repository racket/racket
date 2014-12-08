#lang racket/base
(require racket/future)

(let ()
  (struct a (x))
  (define an-a (a 10))

  (define iter 10000)

  (define (f x)
    (set! iter (sub1 iter))
    (unless (zero? iter)
      ;; This call to `x` is JITted optimistically
      ;; as a struct-getter call, so non-getter `x`
      ;; fill fall back to a slow path:
      (x an-a)))
  (set! f f)

  (void (f a-x))

  (define f1 (future (lambda ()
                       (f f))))
  (for/fold ([x #f]) ([i (in-range 1000)])
    (cons 1 2)))
