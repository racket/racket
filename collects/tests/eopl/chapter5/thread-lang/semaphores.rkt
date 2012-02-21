#lang eopl

(require "store.rkt")                    ; for store ops
(require "data-structures.rkt")          ; for lock, a-lock
(require "scheduler.rkt")                ; for os calls
(require "queues.rkt")

(provide (all-defined-out))

;; implements binary semaphores (mutexes).

(define instrument-mutexes (make-parameter #f))

;; new-mutex () -> Mutex
;; Page: 188
(define new-mutex
  (lambda ()
    (a-mutex
     (newref #f)                     
     (newref '()))))                 

; wait queue, initially empty

;; wait-for-mutex : Mutex * Thread -> FinalAnswer
;; waits for mutex to be open, then closes it.
;; Page: 190
(define wait-for-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (cond
                 ((deref ref-to-closed?)                  
                  (setref! ref-to-wait-queue
                           (enqueue (deref ref-to-wait-queue) th))
                  (run-next-thread))
                 (else
                  (setref! ref-to-closed? #t)
                  (th)))))))

;; signal-mutex : Mutex * Thread -> FinalAnswer
;; Page 190
(define signal-mutex
  (lambda (m th)
    (cases mutex m
      (a-mutex (ref-to-closed? ref-to-wait-queue)
               (let ((closed? (deref ref-to-closed?))
                     (wait-queue (deref ref-to-wait-queue)))
                 (when closed?
                   (if (empty? wait-queue)
                       (setref! ref-to-closed? #f)
                       (dequeue wait-queue
                                (lambda (first-waiting-th other-waiting-ths)
                                  (place-on-ready-queue!
                                   first-waiting-th)
                                  (setref!
                                   ref-to-wait-queue
                                   other-waiting-ths)))))
                 (th))))))
