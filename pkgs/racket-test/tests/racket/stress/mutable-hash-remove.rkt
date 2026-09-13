#lang racket/base

;; Check that adding and removing items from a mutable
;; hash table does make iteraton progressively slower.
;; There's a fine-grained timing test here, but when
;; things go especially wrong, a timeout is also possible.

(define ht (make-hasheq))

(define N 10000)
(define M 1000)

(define bad-threshhold 50)

(define-values (slowest slower-count)
  (for/fold ([slowest 0] [slower-count 0]) ([i (in-range M)])
    (printf "~s: ~a\n" i slower-count)
    (define start-gc (current-gc-milliseconds))
    (define start (current-process-milliseconds))
    (for ([i (in-range N)])
      (hash-set! ht i i))
    (for/sum ([k (in-hash-keys ht)])
      1)
    (for ([i (in-range N)])
      (hash-remove! ht i))
    (define dur (- (- (current-process-milliseconds) start)
                   (- (current-gc-milliseconds) start-gc)))
    (if (dur . > . slowest)
        (values dur (add1 slower-count))
        (values slowest slower-count))))

slowest slower-count

(define str (getenv "PLT_RUN_UNRELIABLE_TESTS"))
(when (and str (regexp-match? #rx"(^|,)timing(,|$)" str))
  (when (slower-count . > . bad-threshhold)
    (error "seems to have gotten slower often")))
