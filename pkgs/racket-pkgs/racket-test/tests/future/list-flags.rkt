#lang racket
(require racket/future)

;; This test tries to trigger race conditions between computing an eq
;; hash code in the runtime thread and setting "is a list" flags in a
;; future thread. It also effectively checks that `list?' is a
;; constant-time operation (i.e., that "is a list" flags are set
;; correctly), since it uses a long chain of pairs.

(define N 1000000)

(define v (make-vector N null))
(for ([i N]) 
  (vector-set! v i (cons i (vector-ref v (max 0 (sub1 i))))))

(define v2 (make-vector (vector-length v)))
(define f (future (lambda ()
                    (for/and ([a (in-vector v)]) 
                      (and (car a) (list? a))))))

(for ([i (vector-length v)])
  (let ([a (vector-ref v i)])
    (when (car a)
      (vector-set! v2 i (eq-hash-code a)))))

(unless (touch f) (error "future fail?"))

(for ([i (vector-length v)])
  (unless (eq? (vector-ref v2 i) (eq-hash-code (vector-ref v i)))
    (error "fail")))
