#lang racket/base

;; Make sure that `custodian-managed-list` doesn't fail if entries
;; are GCed during the traversal of internal tables

(define c (current-custodian))
(define c2 (make-custodian c))

(parameterize ([current-custodian c2])
  (for ([k (in-range 200)])
    (printf "~a\n" k)
    (for ([i (in-range 1000)])
      (thread (lambda () (semaphore-wait (make-semaphore)))))
    (sync (system-idle-evt))
    (time
     (for ([j (in-range 1000)])
       (custodian-managed-list c2 c)))
    (define len (length (custodian-managed-list c2 c)))
    (printf "=> ~a\n" len)
    (when (len . > . 1000)
      (collect-garbage))))
