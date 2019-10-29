#lang racket/base

;; Mutable queue

(provide queue
         make-queue
         queue-empty?
         queue-remove!
         queue-fremove!
         queue-remove-all!
         queue-add!
         queue-add-front!
         queue-remove-node!
         queue-length
         queue-remove-end!)

(struct queue (start end) #:mutable)
(struct node (elem
              [prev #:mutable]
              [next #:mutable]))

(define (make-queue)
  (queue #f #f))

(define (queue-empty? q)
  (not (queue-start q)))

(define (queue-remove! q)
  (define qs (queue-start q))
  (cond
   [(not qs) #f]
   [else
    (define n (node-next qs))
    (set-queue-start! q n)
    (if n
        (set-node-prev! n #f)
        (set-queue-end! q #f))
    (node-elem qs)]))

(define (queue-fremove! q pred)
  (let loop ([qs (queue-start q)])
    (cond
     [qs
      (define w (node-elem qs))
      (cond
       [(pred w)
        (queue-remove-node! q qs)
        w]
       [else
        (loop (node-next qs))])]
     [else #f])))

(define (queue-remove-all! q proc)
  (let loop ([qs (queue-start q)])
    (when qs
      (proc (node-elem qs))
      (loop (node-next qs))))
  (set-queue-start! q #f)
  (set-queue-end! q #f))

(define (queue-add! q w)
  (define e (queue-end q))
  (define n (node w e #f))
  (cond
   [(not e)
    (set-queue-start! q n)]
   [else
    (set-node-next! e n)])
  (set-queue-end! q n)
  n)

(define (queue-add-front! q w)
  (define e (queue-start q))
  (define n (node w #f e))
  (cond
    [(not e)
     (set-queue-start! q n)
     (set-queue-end! q n)]
    [else
     (set-node-prev! e n)
     (set-queue-start! q n)])
  n)

(define (queue-remove-node! q n)
  (if (node-prev n)
      (set-node-next! (node-prev n) (node-next n))
      (set-queue-start! q (node-next n)))
  (if (node-next n)
      (set-node-prev! (node-next n) (node-prev n))
      (set-queue-end! q (node-prev n))))

(define (queue-remove-end! q)
  (define e (queue-end q))
  (cond
    [(not e) #f]
    [else
     (define n (node-prev e))
     (set-node-next! n #f)
     (set-queue-end! q n)
     (node-elem e)]))

(define (queue-length q)
  (let loop ([qs (queue-start q)] [len 0])
    (cond
      [qs (loop (node-next qs) (+ len 1))]
      [else len])))
