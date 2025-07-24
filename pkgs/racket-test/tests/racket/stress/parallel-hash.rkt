#lang racket/base
(require ffi/unsafe/atomic)

;; make sure an `equal?`-based hash table can be used in uninterruptable mode,
;; as long as it is use *only* in uninterruptable mode

(for ([i (in-range 10)])
  (define ht (make-hash))
  (define N 10000)

  (define l (make-uninterruptible-lock))

  (define t (thread #:pool 'own (lambda ()
                                  (for ([i (in-range N)])
                                    (uninterruptible-lock-acquire l)
                                    (hash-set! ht i i)
                                    (uninterruptible-lock-release l)))))
  (for ([i (in-range N)])
    (uninterruptible-lock-acquire l)
    (hash-set! ht i i)
    (uninterruptible-lock-release l))

  (thread-wait t))
