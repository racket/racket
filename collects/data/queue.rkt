#lang racket/base
(require racket/contract/base
         (for-syntax racket/base
                     unstable/wrapc))

;; A Queue contains a linked list with mutable cdrs, holding two pointers
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

(define (queue->list queue)
  (let loop ([link (queue-head queue)]
             [out '()])
    (if (not link)
      (reverse out)
      (loop (link-tail link) (cons (link-value link) out)))))

;; queue->vector could be implemented as (list->vector (queue->list q))
;; but this is somewhat slow. a direct translation between queue's and
;; vector's should be fast so the ideal situation is not to use a list
;; as an intermediate data structure.
;; maybe add the elements to a gvector and use gvector->vector?

;; could use (length (queue->list q)) here but that would double
;; the time it takes to get the length
;; probably if `queue->vector' gets implemented it would be better to
;; do (vector-length (queue->vector q))
(define (queue-length queue)
  (let loop ([link (queue-head queue)]
             [count 0])
    (if (not link)
      count
      (loop (link-tail link) (add1 count)))))

(define (in-queue queue)
  (in-list (queue->list queue)))

(define-sequence-syntax in-queue*
  (lambda () #'in-queue)
  (lambda (stx)
    (syntax-case stx ()
      ([(var) (in-queue* queue-expression)]
       (with-syntax ([queue-expression/c (wrap-expr/c #'queue? #'queue-expression
                                                      #:macro #'in-queue*)])
       #'[(var)
          (:do-in ([(queue) queue-expression/c])
                  (void) ;; handled by contract
                  ([link (queue-head queue)])
                  link
                  ([(var) (link-value link)])
                  #t
                  #t
                  ((link-tail link)))]))
       ([(var ...) (in-queue* queue-expression)]
        #f))))

;; --- contracts ---

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
 [queue-empty? (-> queue/c boolean?)]
 [queue-length (-> queue/c integer?)]
 [queue->list (-> queue/c (listof any/c))])

(provide enqueue! dequeue! (rename-out [in-queue* in-queue]))
