#lang racket/base

;; Check `(thread-wait (current-thread))` as a special case

(let loop ([max-use 0] [same-count 0] [tries 0])
  (collect-garbage)
  (define use (current-memory-use))
  (printf "~s ~s\n" use (<= use max-use))
  (define new-same-count (if (<= use max-use)
                             (+ same-count 1)
                             0))
  (unless (= new-same-count 3)
    (when (= tries 30)
      (error "didn't reach memory-use fixpoint"))
    (for ([i 3000])
      (thread (lambda () (thread-wait (current-thread))))
      (sync (system-idle-evt)))
    (loop (max use max-use) new-same-count (+ tries 1))))
