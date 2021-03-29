#lang racket/base
(require racket/future)

;; Make sure fsemaphores can work like locks, as
;; long as every future that might hold a lock is
;; demanded by a thread (so it must continue to
;; run if it takes a lock)

(define (box-cas!* b old new)
  (memory-order-release) ; makes spurious-failure check meaningful (I think)
  (or (box-cas! b old new)
      ;; Try again if failure looks spurious:
      (and (eq? (unbox b) old)
           (box-cas!* b old new))))

(for ([N (in-range 4 20)])
  (define f (make-fsemaphore 1))
  (define working (box 'ok))
  (define fts
    (for/list ([i (in-range N)])
      (define ft
        (future
         (lambda ()
           (for ([i (in-range 5000)])
             (fsemaphore-wait f)
             (unless (box-cas!* working 'ok 'not-ok)
               (printf "FAIL\n")
               (exit 1))
             (unless (box-cas!* working 'not-ok 'ok)
               (printf "FAIL\n")
               (exit 1))
             (fsemaphore-post f)))))
      (thread (lambda () (touch ft)))
      ft))
  (for-each touch fts))
