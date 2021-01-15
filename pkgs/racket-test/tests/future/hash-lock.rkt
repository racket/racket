#lang racket/base
(require racket/future)

;; Check that a future won't take a lock on an `equal?`-based hash
;; table and then leave the table stuck as the future is suspended.
;; Indirectly, that's checking that a future won't succeed waiting
;; on a semaphore, since a semaphore is used as a lock.

(struct a (x)
  #:property
  prop:equal+hash
  (list
   (lambda (a b eql?)
     (sync (system-idle-evt))
     (eql? (a-x a) (a-x b)))
   (lambda (a hc)
     (sync (system-idle-evt))
     (hc (a-x a)))
   (lambda (a hc)
     (hc (a-x a)))))

(define ht (make-hash (list
                       (cons (a 1) 'one)
                       (cons (a 2) 'two))))

(define f (future
           (lambda ()
             (hash-ref ht (a 1) #f))))
(sync (system-idle-evt))
(unless (eq? (hash-ref ht (a 1) #f) 'one)
  (error "hash failed"))
