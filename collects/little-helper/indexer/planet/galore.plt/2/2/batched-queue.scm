;;; batched-queue.ss  --  Jens Axel Soegaard  --  2003/2004

; NOTES
;   For applications which don't need persistence and
;   more than amortized time bounds is uneeded,
;   the batched queue/deque is the best choice.

;;; BATCHED QUEUE

; Reference: [Oka,43]

; Timings:   empty?         O(1) wc
;            insert         O(1) amortized
;            remove-first   O(1) amortized
;            first          O(1) wc

(module batched-queue mzscheme
  (require (only (lib "list.ss") foldl)
           (lib "42.ss" "srfi"))
  (define-struct queue (front rear))
  
  ; Invariants
  ;   1.  q empty <=> (null? (queue-front q))
  ;   2.  elements of q  =  (append (queue-front q) (reverse (queue-rear q)))
  
  (define empty (make-queue '() '()))
  
  (define (empty? q)
    (null? (queue-front q)))
  
  (define (insert-last x q)
    (let ([front (queue-front q)])
      (if (null? front)
          (make-queue (list x) '())
          (make-queue front (cons x (queue-rear q))))))
  
  (define insert insert-last)
  
  (define (insert* xs q)
    (foldl insert q xs))
  
  (define (remove-first q)
    (let ([front (queue-front q)])
      (if (null? front)
          (error 'remove-first "can't remove element from empty queue; given " q)
          (if (null? (cdr front))
              (make-queue (reverse (queue-rear q)) '())
              (make-queue (cdr front) (queue-rear q))))))
  
  (define (first+remove q)
    (let ([front (queue-front q)])
      (if (null? front)
          (error 'remove-first "can't remove element from empty queue; given " q)
          (values (car front)
                  (if (null? (cdr front))
                      (make-queue (reverse (queue-rear q)) '())
                      (make-queue (cdr front) (queue-rear q)))))))
  
  (define remove remove-first)
  
  (define (first q)
    (if (empty? q)
        (error 'first "There is no first element in an empty queue; given " q))
    (car (queue-front q)))
  
  (define (elements q)
    (append (queue-front q) 
            (reverse (queue-rear q))))
  
  (define (fold f init q)
    (foldl f 
           (foldl f init (queue-front q))
           (reverse (queue-rear q))))
  
  (define (size q)
    ; NOTE: T(size) = O(n)
    (+ (length (queue-front q))
       (length (queue-rear q))))
  
  ;; support for srfi-42
  ; TODO: Remove the generation of the extra list in :queue
  
  (define-syntax queue-ec
    (syntax-rules ()
      [(_ etc1 etc ...)
       (fold-ec empty etc1 etc ... insert)]))
  
  (define (:queue-dispatch args)
    (cond
      [(null? args)
       'queue]
      [(and (= (length args) 1)
            (queue? (car args)))
       (:generator-proc (:list (elements (car args))))]
      [else
       #f]))
  
  (define-syntax :queue
    (syntax-rules (index)
      ((:queue cc var (index i) arg1 arg ...)
       (:dispatched cc var (index i) :queue-dispatch arg1 arg ...) )
      ((:queue cc var arg1 arg ...)
       (:dispatched cc var :queue-dispatch arg1 arg ...) )))
  
  (:-dispatch-set! 
   (dispatch-union (:-dispatch-ref) :queue-dispatch))

  
  (require "signatures/queue-signature.scm")
  (provide-queue)

  )
