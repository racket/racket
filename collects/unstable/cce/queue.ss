#lang scheme

;; A Queue is a circularly linked list of queue structures.
;; The head of the circle is identified by the distinguished head value.
;; The top of the queue (front of the line) is to the right of the head.
;; The bottom of the queue (back of the line) is to the left of the head.
(define-struct queue (value left right) #:mutable)

(define head (gensym 'queue-head))

(define (empty-queue)
  (let* ([q (make-queue head #f #f)])
    (set-queue-left! q q)
    (set-queue-right! q q)
    q))

(define (queue-head? q)
  (eq? (queue-value q) head))

(define (head-queue? v)
  (and (queue? v) (queue-head? v)))

(define (queue-empty? q)
  (and (queue-head? q) (queue-head? (queue-right q))))

(define (nonempty-queue? v)
  (and (queue? v)
       (queue-head? v)
       (queue? (queue-right v))
       (not (queue-head? (queue-right v)))))

(define (enqueue! q v)
  (let* ([bot (queue-left q)]
         [new (make-queue v bot q)])
    (set-queue-left! q new)
    (set-queue-right! bot new)))

(define (dequeue! q)
  (let* ([old (queue-right q)]
         [top (queue-right old)])
    (set-queue-right! q top)
    (set-queue-left! top q)
    (queue-value old)))

(define queue/c
  (flat-named-contract "queue" head-queue?))

(define nonempty-queue/c
  (flat-named-contract "nonempty-queue" nonempty-queue?))

(provide/contract
 [queue/c flat-contract?]
 [nonempty-queue/c flat-contract?]
 [rename head-queue? queue? (-> any/c boolean?)]
 [rename empty-queue make-queue (-> queue/c)]
 [queue-empty? (-> queue/c boolean?)]
 [enqueue! (-> queue/c any/c void?)]
 [dequeue! (-> nonempty-queue/c any/c)])
