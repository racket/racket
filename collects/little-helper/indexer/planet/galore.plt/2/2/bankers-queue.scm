;;; bankers-queue.scm  --  Jens Axel Søgaard

; TODO: implement size

;;; BANKER'S QUEUES

; [Oka, p.64-67]
; The queues are an efficient persistent implementation of queues,
; every operation runs in O(1) amortized time.

(module bankers-queue mzscheme
  (require 
   (only (lib "list.ss") foldl)
   "signatures/queue-signature.scm")
  (provide-queue)
  
  (define-struct queue (front-length front rear-length rear))
  
  (define fl queue-front-length)
  (define f  queue-front)
  (define rl queue-rear-length)
  (define r  queue-rear)

  ; Lazy stream
  
  (define lempty (delay '()))
  
  (define (lcons x stream)
    (delay (cons x stream)))
  
  (define (fcar stream)
    (car (force stream)))
  
  (define (fcdr stream)
    (cdr (force stream)))
  
  (define (lappend pl1 pl2)
    (let ([l1 (force pl1)])
      (if (null? l1)
          pl2
          (let ([l2 (force pl2)])
            (if (null? l2)
                pl1
                (lcons (car l1)
                       (lappend (cdr l1) pl2)))))))
  
  (define (lreverse pl)
    (define (loop pl pr)
      (let ([l (force pl)])
        (if (null? l)
            pr
            (loop (cdr l) (lcons (car l) pr)))))
    (loop pl lempty))
  
  ; Queue operations
  
  (define (insert-last x q)
    (check (fl q) (f q) (add1 (rl q)) (lcons x (r q))))
  
  (define insert insert-last)
  
  (define (insert* xs q)
    (foldl insert q xs))
  
  (define (first q)
    (if (empty? q)
        (error "first: There is no first element in an empty queue; given " q))
    (fcar (f q)))
  
  (define (first+remove q)
    (if (empty? q)
        (error "first: There is no first element in an empty queue; given " q))
    (values (fcar (f q))
            (rest q)))
  
  (define (rest q)
    (check (sub1 (fl q)) (fcdr (f q)) (rl q) (r q)))
  
  (define (remove-first q)
    (if (empty? q)
        (error "remove-first: There is no first element in an empty queue; given " q))
    (rest q))
  
  (define remove remove-first)
  
  (define (check fl f rl r)
    (if (<= rl fl)
        (make-queue fl f rl r)
        (make-queue (+ fl rl) (lappend f (lreverse r)) 0 lempty)))
  
  (define (empty? q)
    (and (= 0 (fl q) (rl q))))
  
  (define empty (make-queue 0 (delay '()) 0 (delay '())))
  
  (define (fold f b q)
    (define (loop q a)
      (if (empty? q)
          a
          (loop (rest q) (f (first q) a))))
    (loop q b))
  
  (define (elements q)
    (fold cons '() q))

  (define (size q)
    (+ (queue-front-length q)
       (queue-rear-length q)))

  (define queue-ec error)  ; TODO !!! implement queue-ec
  (define :queue error)    ; TODO !!! implement :queue
  
  )