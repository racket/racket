#lang racket/base

;; A Queue contains a linked list with mutable cdrs, hoding two pointers
;; to the head and the tail -- where items are pulled from the head and
;; pushed on the tail.  It is not thread safe: mutating a queue from
;; different threads can break it.
(struct queue (head tail) #:mutable)
;; (Note: uses #f for `head' to mark an empty queue, but in those cases
;; the tail will be set to #f too, to avoid holding on to values that
;; should be collected.)
(struct link (value [tail #:mutable]))

(define (make-queue) (queue #f #f))

(define (queue-empty? q) (not (queue-head q)))

(define (nonempty-queue? v) (and (queue? v) (queue-head v) #t))

(define (enqueue! q v)
  (unless (queue? q) (raise-type-error enqueue! "queue" 0 q))
  (let ([new (link v #f)])
    (if (queue-head q)
      (set-link-tail! (queue-tail q) new)
      (set-queue-head! q new))
    (set-queue-tail! q new)))

(define (dequeue! q)
  (unless (queue? q) (raise-type-error dequeue! "queue" 0 q))
  (let ([old (queue-head q)])
    (unless old (error 'dequeue! "empty queue"))
    (set-queue-head! q (link-tail old))
    (link-value old)))

;; --- contracts ---

(require racket/contract)

(define queue/c
  (flat-named-contract "queue" queue?))

(define nonempty-queue/c
  (flat-named-contract "nonempty-queue" nonempty-queue?))

;; Eli: Are these needed?  (vs just providing `queue?', `make-queue' and
;; `queue-empty?'.)
(provide/contract
 [queue/c flat-contract?]
 [nonempty-queue/c flat-contract?]
 [queue? (-> any/c boolean?)]
 [make-queue (-> queue/c)]
 [queue-empty? (-> queue/c boolean?)])

(provide enqueue! dequeue!)
