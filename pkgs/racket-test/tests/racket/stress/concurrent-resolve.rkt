#lang racket/base
(require racket/sandbox)

;; Test by William Bowman

;; This is a regression test for module caching, module instantiation,
;; and sandbox module initialization, where there are various
;; possibilities for race conditions if there's too much sharing
;; or insufficient locking.

;; Initial value greater than 1 allows concurrency
(define s (make-semaphore 2))

(sandbox-eval-limits #f)
(sandbox-memory-limit #f)

(define threads
  (for/list ([i (in-range 20)])
    (semaphore-wait s)
    (thread
     (lambda ()
       (with-handlers ([void (lambda (exn)
                               (eprintf "Error running ~a~n" (exn-message exn))
                               (semaphore-post s)
                               #;(exit 1))])

         (let ([evalor (make-evaluator
                        'scheme
                        '(define (fact n)
                           (if (zero? n)
                               1
                               (* n (fact (sub1 n))))))])
           (printf "Finished ~a~n" (evalor '(fact 5)))
           (semaphore-post s)))))))

(for-each thread-wait threads)
