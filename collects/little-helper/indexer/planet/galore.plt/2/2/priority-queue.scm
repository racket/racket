;;; priority-queue.scm  --  Jens Axel Søgaard
;;; PURPOSE

; This file implements priority queues on top of
; a heap library.

(module priority-queue mzscheme
  (require (prefix heap: "heap.scm")
           (lib "67.ss" "srfi"))
  
  ; a priority-queue is a heap of  (cons <priority> <element>)
  
  (define-struct priority-queue (heap))
  
  ; conveniences
  (define (heap pq) (priority-queue-heap pq))
  (define (pri p)   (car p))
  (define (elm p)   (cdr p))
  (define (make h)  (make-priority-queue h))
  
  ; sort after priority
  ; TODO: and then element?
  (define (compare p1 p2)
    (number-compare (pri p1) (pri p2)))
  
  ;;; OPERATIONS

  (define (elements pq)
    (map elm (heap:elements (heap pq))))

  (define (elements+priorities pq)
    (let ([eps (heap:elements (heap pq))])
      (values (map elm eps)
              (map pri eps))))
  
  (define (empty? pq)
    (heap:empty? (heap pq)))

  (define empty
    (case-lambda 
      [()    (empty (current-compare))]
      [(cmp) (make (heap:empty compare))]))
  
  (define (fold f b a)
    (heap:fold f b (heap a)))
  
  (define (find-min pq) 
    (elm (heap:find-min (heap pq))))
  
  (define (find-min-priority pq)
    (pri (heap:find-min (heap pq))))
  
  (define (insert x p pq)
    (make (heap:insert (cons p x) (heap pq))))

  (define (insert* xs ps pq)
    (make (heap:insert* (map cons ps xs) (heap pq))))
  
  (define (delete-min pq)
    (make (heap:delete-min (heap pq))))
  
  (define (size pq)
    (heap:size (heap pq)))
  
  (define (union pq1 pq2)
    (make (heap:union (heap pq1) (heap pq2))))
  
  
  (require "signatures/priority-queue-signature.scm")
  (provide-priority-queue)
  )
