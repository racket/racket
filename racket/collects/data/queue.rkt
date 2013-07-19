#lang racket/base
(require racket/contract/base
         (for-syntax racket/base
                     unstable/wrapc))

;; A Queue contains a linked list with mutable cdrs, holding two pointers
;; to the head and the tail -- where items are pulled from the head and
;; pushed on the tail.  It is not thread safe: mutating a queue from
;; different threads can break it.
(struct queue (head tail length) #:mutable
  #:property prop:sequence (λ (q) (in-queue q)))
;; (Note: uses #f for `head' to mark an empty queue, but in those cases
;; the tail will be set to #f too, to avoid holding on to values that
;; should be collected.)
(struct link (value [tail #:mutable]))

(define (make-queue) (queue #f #f 0))

(define (queue-empty? q) (not (queue-head q)))

(define (non-empty-queue? v) (and (queue? v) (queue-head v) #t))

(define (enqueue! q v)
  (unless (queue? q) (raise-type-error enqueue! "queue" 0 q))
  (let ([new (link v #f)])
    (if (queue-head q)
      (set-link-tail! (queue-tail q) new)
      (set-queue-head! q new))
    (set-queue-tail! q new)
    (set-queue-length! q (+ (queue-length q) 1))))

(define (enqueue-front! q v)
  (unless (queue? q) (raise-type-error enqueue! "enqueue-front!" 0 q))
  (define fr (queue-head q))
  (cond
    [fr
     (set-queue-head! q (link v fr))]
    [else
     (define k (link v #f))
     (set-queue-head! q k)
     (set-queue-tail! q k)])
  (set-queue-length! q (+ (queue-length q) 1)))

(define (dequeue! q)
  (unless (queue? q) (raise-type-error dequeue! "queue" 0 q))
  (let ([old (queue-head q)])
    (unless old (raise-type-error 'dequeue! "non-empty queue" 0 q))
    (cond
      [(eq? old (queue-tail q))
       (set-queue-tail! q #f)
       (set-queue-head! q #f)]
      [else
       (set-queue-head! q (link-tail old))])
    (set-queue-length! q (- (queue-length q) 1))
    (link-value old)))

(define (queue->list q) (for/list ([e (in-queue q)]) e))

;; queue->vector could be implemented as (list->vector (queue->list q))
;; but this is somewhat slow. a direct translation between queue's and
;; vector's should be fast so the ideal situation is not to use a list
;; as an intermediate data structure.
;; maybe add the elements to a gvector and use gvector->vector?

(define (queue-filter! q pred?)
  (unless (queue-empty? q)
    (let loop ([prev #f]
               [curr (queue-head q)]
               [i 0])
      (cond
        [(not curr)
         (set-queue-tail! q prev)
         (set-queue-length! q i)]
        [else
         (define passed? (pred? (link-value curr)))
         (cond
           [passed?
            (loop curr (link-tail curr) (+ i 1))]
           [else
            (define tl (link-tail curr))
            (if prev
                (set-link-tail! prev tl)
                (set-queue-head! q tl))
            (loop prev tl i)])]))))

(define (in-queue q)
  (make-do-sequence
   (λ ()
     (values 
      link-value
      link-tail
      (queue-head q)
      link?
      #f #f))))

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
(define queue/c queue?)
(define nonempty-queue/c non-empty-queue?)

(provide/contract
 [queue/c flat-contract?]
 [nonempty-queue/c flat-contract?]
 [queue? (-> any/c boolean?)]
 [non-empty-queue? (-> any/c boolean?)]
 [make-queue (-> queue?)]
 [queue-empty? (-> queue? boolean?)]
 [queue-length (-> queue? exact-nonnegative-integer?)]
 [queue->list (-> queue? (listof any/c))]
 [queue-filter! (-> queue? (-> any/c any/c) void?)])

(provide enqueue! enqueue-front!
         dequeue! (rename-out [in-queue* in-queue]))
