#lang racket/base
(require "../common/internal-error.rkt")

;; Simple lock for sandman

(provide make-lock
         lock-acquire
         lock-release)

(define (make-lock)
  (box 0))

(define (lock-acquire box)
  (let loop ()
    (unless (and (= 0 (unbox box)) (box-cas! box 0 1))
      (loop))))

(define (lock-release box)
  (unless (box-cas! box 1 0)
    (internal-error "failed to release lock")))
