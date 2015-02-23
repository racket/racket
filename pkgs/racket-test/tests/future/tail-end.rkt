#lang racket/base
(require racket/future)

;; This example ends up with a delayed tail call

(let ()
  (struct a (x))
  (define an-a (a 10))

  (define ((f arg) x)
    (x arg))
  (set! f f)

  (void ((f an-a) a-x))

  (define f1 (future (lambda ()
                       ((f f) (f f)))))

  (touch f1))
