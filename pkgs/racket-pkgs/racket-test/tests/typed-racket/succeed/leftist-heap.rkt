;;; leftist-heap.scm  --  Jens Axel Søgaard  --  28th dec 2005

;;; LEFTIST HEAP [Okasaki, p.17-20]

; A *Leftist heap* is a heap-ordered binary tree with the /leftist property/:
; The rank of any left child is at least as large as the rank of its right sibling.
; The *rank* of a node is the length of the its *right spine*, which is the
; rightmost path from the node to an empty node.

;;; Time             worst case
;  delete-min         O(log n)
;  empty              O(1)
;  empty?             O(1)
;  find-min           O(1)
;  insert             O(log n)
;  union              O(log n)

;;; changes
;; annotations
;; foldl from typed-list
;; eta-expand cons (line 193)
;; added error case for one-armed if
;; need rest args
;; didn't attempt generators

#lang typed-scheme
(define-type-alias number Number)
(define-type-alias boolean Boolean)
(define-type-alias symbol Symbol)
(define-type-alias top Any)
(define-type-alias list-of Listof)
(require
   (except-in srfi/67 current-compare =? <?)
   #;"typed-list.rkt"
   #;srfi/42
   #;(only mzlib/list foldl))

  #;(provide (all-defined))
  (provide comparator Heap elements empty fold heap-node? find-min empty? insert insert* delete-min size union)

  #;(define-type-alias top top)

  (define-type-alias comparator (top top -> number))

  ;; fixme - type aliases should work in require

  (require/typed srfi/67
    [current-compare (-> (top top -> number))]
    [=? ((top top -> number) top top -> boolean)]
    [<? ((top top -> number) top top -> boolean)])

  ;;; DATA DEFINITION

  ; A HEAP is either
  ;    (make-heap-empty cmp)
  ; or
  ;    (make-heap-node cmp rank elm left right)
  ; where
  ;   cmp  is a compare function,
  ;   rank is an integer, and
  ;   left and right are heaps.

  (define-typed-struct heap              ([compare : comparator]))
  (define-typed-struct (heap-empty heap) ())
  (define-typed-struct (a) (heap-node heap)
    ([rank : Real] [elm : a] [left : (Un (heap-node a) heap-empty)] [right : (Un (heap-node a) heap-empty)]))

  (define-type-alias (Heap a) (Un (heap-node a) heap-empty))

  ;;; CORE HEAP OPERATIONS

  ;; FIXME
  (: empty (All (a) (case-lambda (-> (Heap a)) (comparator -> (Heap a)))))
  (define empty
    (case-lambda
      [()    (make-heap-empty (current-compare))]
      [(#{cmp : comparator}) (make-heap-empty cmp)]))

  (define: empty? : (pred heap-empty) heap-empty?)

  (pdefine: (a) (rank [h : (Heap a)]) : Real
    (if (empty? h)
        0
        (heap-node-rank h)))

  (pdefine: (a) (make [x : a] [a : (Heap a)] [b : (Heap a)]) : (Heap a)
    (let ([ra (rank a)]
          [rb (rank b)]
          [cmp (heap-compare a)])
      (if (>= ra rb)
          (make-heap-node cmp (add1 rb) x a b)
          (make-heap-node cmp (add1 ra) x b a))))

  (pdefine: (a) (union [h1 : (Heap a)] [h2 : (Heap a)]) : (Heap a)
    (cond
      [(empty? h1) h2]
      [(empty? h2) h1]
      [else        (let* ([x (heap-node-elm h1)]
                          [y (heap-node-elm h2)])
                          (if<=? ((heap-compare h1) x y)
                                (make x (heap-node-left h1) (union (heap-node-right h1) h2))
                                (make y (heap-node-left h2) (union h1 (heap-node-right h2)))))]))

  (pdefine: (a) (insert [x : a] [h : (Heap a)]) : (Heap a)
    (let: ([cmp : comparator (heap-compare h)])
      (union (make-heap-node cmp 1 x (make-heap-empty cmp) (make-heap-empty cmp))
             h)))

  ;; No changes other than variable annotations
  (pdefine: (a) (delete [x : a] [h : (Heap a)]) : (Heap a)
    (define: (delete/sf [x : a] [h : (Heap a)] [s : ((Heap a) -> (Heap a))] [f : (-> (Heap a))]) : (Heap a)
      (cond
        [(empty? h)
         (s h)]
        [(=? (heap-compare h) x (heap-node-elm h))
         (s (union (heap-node-left h) (heap-node-right h)))]
        [(<? (heap-compare h) x (heap-node-elm h))
         (f)]
        [else
         (let ([cmp (heap-compare h)])
           (let ([y (heap-node-elm h)]
                 [l (heap-node-left h)]
                 [r (heap-node-right h)])
             (delete/sf x l
                        (lambda: ([h1 : (Heap a)]) (s (make y h1 r)))
                        (lambda ()   (delete/sf x r
                                                (lambda: ([h1 : (Heap a)]) (s (make y l h1)))
                                                (lambda () (f)))))))]))
    (delete/sf x h
               (lambda: ([h1 : (Heap a)]) h1)
               (lambda ()   h)))

  ;; annotated w/ no errors
  (pdefine: (a) (delete-all [x : a] [h : (Heap a)]) : (Heap a)
    (define: (delete-all/sf [x : a] [h : (Heap a)] [s : ((Heap a) -> (Heap a))] [f : (-> (Heap a))]) : (Heap a)
      (cond
        [(empty? h)
         (s h)]
        [(=? (heap-compare h) x (heap-node-elm h))
         (s (union (delete-all x (heap-node-left h))
                   (delete-all x (heap-node-right h))))]
        [(<? (heap-compare h) x (heap-node-elm h))
         (f)]
        [else
         (let ([cmp (heap-compare h)])
           (let ([y (heap-node-elm h)]
                 [l (heap-node-left h)]
                 [r (heap-node-right h)])
             (delete-all/sf x l
                        (lambda: ([l1 : (Heap a)]) (s (delete-all/sf x r
                                                                     (lambda: ([r1 : (Heap a)]) (make y l1 r1))
                                                                     (lambda ()   (make y l1 r)))))
                        (lambda ()   (delete-all/sf x r
                                                    (lambda: ([r1 : (Heap a)]) (s (make y l r1)))
                                                    (lambda ()   (f)))))))]))
    (delete-all/sf x h
                   (lambda: ([h1 : (Heap a)]) h1)
                   (lambda ()   h)))

  (pdefine: (a) (find-min [h : (heap-node a)]) : a
    (heap-node-elm h))

  (pdefine: (a) (delete-min [h : (heap-node a)]) : (Heap a)
    (union (heap-node-left h) (heap-node-right h)))

  (pdefine: (a) (get [x : a] [h : (Heap a)]) : (Un #f a)
    (let ([cmp (heap-compare h)])
      (define: (inner-get [h : (Heap a)] [s : (a -> a)] [f : (-> (Un #f a))]) : (Un #f a)
        (if (empty? h)
            (f)
            (if=? (cmp x (heap-node-elm h))
                  (s (heap-node-elm h))
                  (inner-get (heap-node-left h) s
                             (lambda () (inner-get (heap-node-right h) s
                                                   f))))))
      (inner-get h (lambda: ([x : a]) x) (lambda () #f))))

  ;;;
  ;;; EXTRA OPERATIONS
  ;;;

  (pdefine: (a) (delete* [xs : (list-of a)] [h : (Heap a)]) : (Heap a)
    (foldl {ann delete : (a (Heap a) -> (Heap a))} h xs))

  (pdefine: (a r) (fold [f : (a r -> r)] [b : r] [h : (Heap a)]) : r
    (if (empty? h)
        b
        (fold f
              (fold f
                    (f (heap-node-elm h) b)
                    (heap-node-left h))
              (heap-node-right h))))

  (pdefine: (a) (elements [h : (Heap a)]) : (list-of a)
    (fold (lambda: ([x : a] [l : (list-of a)]) (cons x l)) '() h))

  (pdefine: (a) (count [x : a] [h : (Heap a)]) : number
    (let ([cmp (heap-compare h)])
      (fold (lambda: ([y : a] [s : number])
              (if=? (cmp x y)
                    (add1 s)
                    s))
            0 h)))

  (pdefine: (a) (-heap . [xs : a *]) : (Heap a)
    (list->heap xs))


  (define: list->heap : (All (a) (case-lambda (comparator (list-of a) -> (Heap a)) ((list-of a) -> (Heap a))))
    ; time: O(n)
    (pcase-lambda: (a)
      [([l : (list-of a)])     (list->heap (current-compare) l)]
      [([cmp : comparator] [l : (list-of a)])
       (let* ([e  (#{empty @ a} cmp)]
              [hs (map (lambda: ([x : a]) (insert x e)) l)])
         ; (list heap) -> (list heap)
         ;  merge adjacent pairs of heaps
         (define: (merge-pairs [hs : (Listof (Heap a))]) : (list-of (Heap a))
           (cond
             [(or (null? hs)
                  (null? (cdr hs))) hs]
             [else
              (cons (union (car hs) (cadr hs))
                    (merge-pairs (cddr hs)))]))
         (if (null? hs)
             e
             (let: loop : (Heap a) ([hs : (list-of (Heap a)) hs])
               ; merge adjacent pairs of heaps until one is left
               (cond
                 [(null? hs)       (error 'never-happen)]
                 [(null? (cdr hs)) (car hs)]
                 [else             (loop (merge-pairs hs))]))))]))



  (pdefine: (a) (insert* [xs : (list-of a)] [h : (Heap a)]) : (Heap a)
    (union (list->heap (heap-compare h) xs) h))

  ; select : set -> element
  (pdefine: (a) (select [s : (Heap a)]) : a
    (if (empty? s)
        (error 'select "can't select an element from an empty heap")
        (find-min s)))

  (define: singleton : (All (a) (case-lambda (a -> (Heap a)) (comparator a -> (Heap a))))
    (pcase-lambda: (a)
      [([x : a])     (insert x (#{empty @ a}))]
      [([cmp : comparator] [x : a]) (insert x (make-heap-empty cmp))]))

  (pdefine: (a) (size [h : (Heap a)]) : Real
    ; NOTE: T(size)=O(n)
    (cond
      [(heap-empty? h) 0]
      [else            (+ (size (heap-node-left h))
                          1
                          (size (heap-node-right h)))]))

  #|
  ;;;
  ;;; support for srfi-42
  ;;;

  (define-syntax heap-ec
    (syntax-rules ()
      [(heap-ec cmp etc1 etc ...)
       (fold-ec (empty cmp) etc1 etc ... insert)]))

  (define-syntax :heap
    (syntax-rules (index)
      ((:heap cc var (index i) arg)
       (:parallel cc (:stack var arg) (:integers i)) )
      ((:heap cc var arg)
       (:do cc
            (let ())
            ((t arg))
            (not (empty? t))
            (let ((var (find-min t))))
            #t
            ((delete-min t)) ))))

  (define (:heap-dispatch args)
    (cond
      [(null? args)
       'heap]
      [(and  (heap? (car args)))
       (:generator-proc (:heap (car args)))]
      [else
       #f]))

  (:-dispatch-set!
   (dispatch-union (:-dispatch-ref) :heap-dispatch))

  |#

