;;; batched-dequeue.scm  --  Jens Axel Søgaard

;;; DOUBLE ENDED BATCHED QUEUE

; Reference: Exercise 5.1 in [Oka]. (Hoogerwoord 92)

(module batched-deque mzscheme
  (require (only (lib "list.ss") foldl)
           (lib "42.ss" "srfi"))

  (define-struct deque  (front rear))
  
  ; Invariants
  ;   1.  q empty         <=>  (null? (deque-front q))
  ;   2.  (size q) >= 2   <=>  (and (not (null? (deque-front q)))
  ;                                 (not (null? (deque-rear q)))
  ;   3.  elements of q  =  (append (deque-front q) (reverse (deque-rear q)))
  
  (define empty (make-deque '() '()))
  
  (define (empty? q)
    (null? (deque-front q)))
  
  (define (split l)
    (define (take n f r)
      (if (= n 0)
          (list (reverse f) r)
          (take (sub1 n) (cons (car r) f) (cdr r))))
    (let ([n (length l)])
      (take (quotient n 2) '() l)))
  
  (define (check-invariant q)
    (let ([f (deque-front q)]
          [r (deque-rear q)])
      (cond
        [(and (or (null? f) 
                  (null? (cdr f))) 
              (null? r))            q]
        [(null? f)                  (let* ([h  (split r)]
                                           [fh (car h)]
                                           [sh (cadr h)])
                                      (make-deque (reverse sh)  fh))]
        ; TODO: This brach is not covered by the test suite !!!
        [(null? r)                  (let* ([h  (split f)]                    
                                           [fh (car h)]
                                           [sh (cadr h)])
                                      (make-deque fh  (reverse sh)))]
        [else q])))
  
  (define (insert-first x q)
    (check-invariant (make-deque (cons x (deque-front q)) 
                                 (deque-rear q))))
  
  (define (insert-last x q)
    (check-invariant (make-deque (deque-front q) 
                                 (cons x (deque-rear q)))))
  
  (define insert insert-last) 
  
  (define (insert* xs q)
    (foldl insert q xs))
  
  (define (remove-first q)
    (if (null? (deque-front q))
        (error "remove-first: can't remove element from empty deque; given " q)
        (check-invariant (make-deque (cdr (deque-front q)) (deque-rear q)))))
  
  (define (remove-last q)
    (if (null? (deque-rear q))
        (remove-first q) 
        (check-invariant (make-deque (deque-front q) (cdr (deque-rear q))))))   
  
  (define (last q)
    (if (empty? q)
        (error "last: There is no last element in an empty deque; given " q))
    (cond
      [(null? (deque-rear q))  (first q)]
      [else                    (car (deque-rear q))]))
  
  (define remove remove-first)
  
  (define (first q)
    (if (empty? q)
        (error "first: There is no first element in an empty deque; given " q))
    (car (deque-front q)))
  
  (define (first+remove q)
    (let ([front (deque-front q)])
      (if (null? front)
          (error 'remove-first "can't remove element from empty deque; given " q)
          (values (car front)
                  (check-invariant (make-deque (cdr (deque-front q)) (deque-rear q)))))))
  
  
  (define (elements q)
    (append (deque-front q) (reverse (deque-rear q))))
  
  (define (fold f init q)
    (foldl f 
           (foldl f init (deque-front q))
           (reverse (deque-rear q))))
  
  ; a deque is also a queue
  (define (queue? o)
    (deque? o))

  (define (size d)
    (+ (length (deque-front d))
       (length (deque-rear d))))
  
  ;; support for srfi-42
  ; TODO: remove intermediary list in :queue-dispatch
  
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

  (define-syntax deque-ec (syntax-rules () ((_ more ...)  (queue-ec more ...))))
  (define-syntax :deque (syntax-rules () ((_ more ...)  (:queue more ...))))

  

  (require "signatures/queue-signature.scm")
  (provide-deque)
  )
