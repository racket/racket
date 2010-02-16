#lang plai/collector
; This collector was written by Robby.
; It is advanced and particular enough, that I do not expect a student would be brazen enough to copy or plagiarize it, so it is public.

(print-only-errors #t)

#|

heap-layout:

 0 during gc   : left-side of queue
   non-gc time : free pointer
 1 during-gc   : right-side-of-queue
   non-gc time : out of memory pointer
 2 '()
 3 #f
 4 #t
 5 0
 6 1
 7 2
 8 3
 9 4
 10 .... (n-10/2): space 1.
 (n-10/2)+1 ... n: space 2.

|#

;; the bounds on the initial half of a heap.
(define (first-start) 10)
(define (second-start) 
  (unless (even? (- (heap-size) (first-start)))
    (error 'second-start "bad heap size ~s" (heap-size)))
  (+ (first-start) (/ (- (heap-size) (first-start)) 2)))

(define (init-allocator) 
  (unless (even? (heap-size))
    (error 'two-space.ss "must have an even sized heap"))
  (when (<= (heap-size) 10)
    (error 'two-space.ss "heap too small"))
  (for ([i (in-range 0 (heap-size))])
    (heap-set!
     i
     (cond
       [(= i 0) (first-start)]
       [(= i 1) (second-start)]
       [(immediate-loc? i) (immediate-loc->val i)]
       [(< i (second-start)) 'free]
       [else 'bad]))))

(define (immediate-loc? i) (< 1 i (first-start)))

(define (immediate-loc->val i)
  (case i
    [(2) #f]
    [(3) #t]
    [(4) '()]
    [else (- i 5)]))

(define (immediate-val? v)
  (or (and (exact-integer? v)
           (<= 0 v 4))
      (eq? v #t)
      (eq? v #f)
      (eq? v null)))

(define (immediate-val->loc v)
  (case v
    [(#f) 2]
    [(#t) 3]
    [(()) 4]
    [else (+ v 5)]))

(test (immediate-loc? 1) #f)
(test (immediate-loc? 2) #t)
(test (immediate-loc? 4) #t)
(test (immediate-loc? 10) #f)
(test (immediate-loc->val 5) 0)
(test (immediate-loc->val 4) '())
(test (immediate-val->loc (immediate-loc->val 2)) 2)
(test (immediate-val->loc (immediate-loc->val 3)) 3)
(test (immediate-val->loc (immediate-loc->val 4)) 4)
(test (immediate-val->loc (immediate-loc->val 5)) 5)
(test (immediate-val->loc (immediate-loc->val 6)) 6)
(test (immediate-val->loc (immediate-loc->val 7)) 7)
(test (immediate-val->loc (immediate-loc->val 8)) 8)
(test (immediate-val->loc (immediate-loc->val 9)) 9)

(define (mkheap alloc-ptr alloc-stop . args)
  (unless (and (number? alloc-ptr) (number? alloc-stop))
    (error 'mkheap "expected numbers for first two args, got ~e and ~e" alloc-ptr alloc-stop))
  (apply vector (append (list alloc-ptr alloc-stop  #f #t '() 0 1 2 3 4) args)))

(test (let ([v (make-vector 12 'x)])
        (with-heap v (init-allocator))
        v)
      (mkheap 10 11 'free 'bad))

(test (let ([v (make-vector 20 'x)])
        (with-heap v (init-allocator))
        v)
      (mkheap 10 15
              'free 'free 'free 'free 'free
              'bad  'bad  'bad  'bad  'bad))

(define (gc:deref loc) 
  (cond
    [(immediate-loc? loc)
     (immediate-loc->val loc)]
    [(equal? (heap-ref loc) 'flat)
     (heap-ref (+ loc 1))]
    [else
     (error 'gc:deref "attempted to deref a non flat value, loc ~s" loc)]))

(test (with-heap (mkheap 10 20 'flat 14)
                 (gc:deref 10))
      14)
(test (gc:deref 2) #f)
(test (gc:deref 3) #t)

(define (gc:first pr-ptr) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 1))
      (error 'first "non pair")))

(test (with-heap (mkheap 10 20 'pair 0 1)
                 (gc:first 10))
      0)

(define (gc:rest pr-ptr) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 2))
      (error 'first "non pair")))

(test (with-heap (mkheap 10 20 'pair 0 1)
                 (gc:rest 10))
      1)

(define (gc:flat? loc) 
  (cond
    [(< loc (first-start)) #t]
    [(equal? (heap-ref loc) 'flat) #t]
    [else #f]))

(test (with-heap (mkheap 10 20 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 12))
      #f)
(test (with-heap (mkheap 10 20 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 15))
      #t)
(test (with-heap (mkheap 10 20 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 5))
      #t)

(define (gc:cons? loc) 
  (cond
    [(< loc (first-start)) #f]
    [else
     (equal? (heap-ref loc) 'pair)]))

(test (with-heap (mkheap 10 20 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 12))
      #t)
(test (with-heap (mkheap 10 20 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 15))
      #f)
(test (with-heap (mkheap 10 20 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 5))
      #f)

(define (gc:set-first! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 1) new)
      (error 'set-first! "non pair")))

(test (let ([h (mkheap 10 20 'pair 2 2)])
        (with-heap h (gc:set-first! 10 3))
        h)
      (mkheap 10 20 'pair 3 2))

(define (gc:set-rest! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 2) new)
      (error 'set-first! "non pair")))

(test (let ([h (mkheap 10 20 'pair 2 2)])
        (with-heap h (gc:set-rest! 10 3))
        h)
      (mkheap 10 20 'pair 2 3))

(define (gc:alloc-flat fv)
  (cond
    [(immediate-val? fv)
     (immediate-val->loc fv)]
    [else
     (let ([ptr (alloc 2)])
       (cond
         [ptr
          (fill-in-flat ptr fv)]
         [else
          (init-gc)
          (when (procedure? fv)
            (move-roots (procedure-roots fv)))
          (collect-garbage)
          (let ([ptr (alloc 2)])
            (unless ptr
              (error 'two-space.ss "out of memory"))
            (fill-in-flat ptr fv))]))]))

(define (fill-in-flat ptr fv)
  (heap-set! ptr 'flat)
  (heap-set! (+ ptr 1) fv)
  ptr)
  
(define (gc:cons hd tl)
  (let ([ptr (alloc 3)])
    (cond
      [ptr
       (fill-in-cons ptr hd tl)]
      [else
       (init-gc)
       (let ([new-hd (move-loc hd)]
             [new-tl (move-loc tl)])
         (collect-garbage)
         (let ([ptr (alloc 3)])
           (unless ptr
             (error 'two-space.ss "out of memory"))
           (fill-in-cons ptr new-hd new-tl)))])))

(define (fill-in-cons ptr hd tl)
  (heap-set! ptr 'pair)
  (heap-set! (+ ptr 1) hd)
  (heap-set! (+ ptr 2) tl)
  ptr)

;; alloc : number -> boolean
;; returns #f if nothing can be allocated
(define (alloc n)
  (let ([next (heap-ref 0)])
    (cond
      [(<= (+ next n) (heap-ref 1))
       (heap-set! 0 (+ next n))
       next]
      [else
       #f])))

(test (let ([h (mkheap 16 16
                       'fwd  17    'flat 112   'pair 10    10
                       'free 'free 'free 'free 'free 'free 'free)])
        (with-heap h (alloc 3)))
      #f)

(test (let ([h (mkheap 10 16
                       'free 'free 'free 'free 'free 'free 'free
                       'free 'free 'free 'free 'free 'free 'free)])
        (with-heap h (alloc 3))
        h)
      (mkheap 13 16
              'free 'free 'free 'free 'free 'free 'free
              'free 'free 'free 'free 'free 'free 'free))

(define (init-gc)
  (cond
    [(< (heap-ref 0) (second-start))
     (heap-set! 0 (second-start))]
    [else
     (heap-set! 0 (first-start))])
  (heap-set! 1 (heap-ref 0)))

(test (let ([h (mkheap 16 16
                       'fwd  17    'flat 112   'pair 10    10
                       'free 'free 'free 'free 'free 'free 'free)])
        (with-heap h (init-gc))
        h)
      (mkheap 17 17
              'fwd  17    'flat 112   'pair 10    10
              'free 'free 'free 'free 'free 'free 'free))

(test (let ([h (mkheap 15 16
                       'fwd  17    'flat 112   'pair 10    10
                       'free 'free 'free 'free 'free 'free 'free)])
        (with-heap h (init-gc))
        h)
      (mkheap 17 17
              'fwd  17    'flat 112   'pair 10    10
              'free 'free 'free 'free 'free 'free 'free))

(define (finalize-gc)
  (heap-set! 0 (heap-ref 1))
  (cond
    [(< (heap-ref 0) (second-start))
     (heap-set! 1 (second-start))
     (for ([i (in-range (second-start) (heap-size))])
       (heap-set! i 'bad))]
    [else
     (heap-set! 1 (heap-size))
     (for ([i (in-range (first-start) (second-start))])
       (heap-set! i 'bad))])
  (for ([i (in-range (heap-ref 0) (heap-ref 1))])
    (heap-set! i 'free)))

(test (let ([h (mkheap 20 20
                       'flat 17 'flat 12    'free 'free 'free
                       'pair 12  10   'free 'free 'free 'free)])
        (with-heap h (finalize-gc))
        h)
      (mkheap 20 24
              'bad  'bad 'bad 'bad  'bad  'bad  'bad
              'pair 12   10   'free 'free 'free 'free))

(test (let ([h (mkheap 14 14
                       'flat 17 'flat 12    'free 'free 'free
                       'pair 12  10   'free 'free 'free 'free)])
        (with-heap h (finalize-gc))
        h)
      (mkheap 14 17
              'flat 17 'flat 12    'free 'free 'free
              'bad  'bad 'bad 'bad  'bad  'bad  'bad))

(define (collect-garbage)
  (move-roots (get-root-set))
  (copy-data)
  (finalize-gc))
  
;; move-roots : (listof roots) -> void
(define (move-roots roots)
  (cond
    [(null? roots) (void)]
    [else 
     (set-root! (car roots) (move-loc (read-root (car roots))))
     (move-roots (cdr roots))]))

;; move-loc : loc[from-space] -> loc[to-space]
(define (move-loc loc)
  (cond
    [(immediate-loc? loc)
     loc]
    [else
     (case (heap-ref loc)
       [(fwd) (heap-ref (+ loc 1))]
       [(pair) 
        (let ([dest (heap-ref 1)])
          (heap-set! dest 'pair)
          (heap-set! (+ dest 1) (heap-ref (+ loc 1)))
          (heap-set! (+ dest 2) (heap-ref (+ loc 2)))
          (heap-set! 1 (+ dest 3))
          (heap-set! loc 'fwd)
          (heap-set! (+ loc 1) dest)
          (heap-set! (+ loc 2) 'junk)
          dest)]
       [(flat) 
        (let ([dest (heap-ref 1)])
          (heap-set! dest 'flat)
          (heap-set! (+ dest 1) (heap-ref (+ loc 1)))
          (heap-set! 1 (+ dest 2))
          (heap-set! loc 'fwd)
          (heap-set! (+ loc 1) dest)
          dest)]
       [else
        (error 'move-loc "found a non-tag at location ~a" loc)])]))

(test (move-loc 4) 4)

(test (let ([v (mkheap 15 15 
                       'fwd   17 'free 'free 'free 
                       'free 'free 'free 'free 'free)])
        (list (with-heap v (move-loc 10))
              v))
      (list 17
            (mkheap 15 15 
                    'fwd   17   'free 'free 'free 
                    'free 'free 'free 'free 'free)))

(test (let ([v (mkheap 15 15 
                       'flat 13 'free 'free 'free 
                       'free 'free 'free 'free 'free)])
        (list (with-heap v (move-loc 10))
              v))
      (list 15
            (mkheap 15 17
                    'fwd  15 'free 'free 'free
                    'flat 13 'free 'free 'free)))

(test (let ([v (mkheap 15 15 
                       'pair 10 10 'free 'free 
                       'free 'free 'free 'free 'free)])
        (list (with-heap v (move-loc 10))
              v))
      (list 15
            (mkheap 15 18
                    'fwd  15 'junk 'free 'free
                    'pair 10 10 'free 'free)))

(define (copy-data)
  (let ([left (heap-ref 0)]
        [right (heap-ref 1)])
  (when (< left right)
    (case (heap-ref left)
      [(pair)
       (maybe-move/loc left 1)
       (maybe-move/loc left 2)
       (heap-set! 0 (+ left 3))]
      [(flat)
       (heap-set! 0 (+ left 2))]
      [(proc)
       (maybe-move/roots left (procedure-roots (heap-ref (+ left 1))))
       (heap-set! 0 (+ left 2))]
      [else
       (error 'copy-data "unknown tag ~s" (heap-ref left))])
    (copy-data))))

;; maybe-move/loc : loc[to-space] offset -> void
;; moves the pointer at record+offset if it is in a different
;; semispace from record.
(define (maybe-move/loc record delta)
  (let ([pointer (heap-ref (+ record delta))])
    (unless (different-halves? record pointer)
      (error 'maybe-move/loc "tried to move a pointer that was in the from space already ~s ~s" record pointer))
    ;; now we know pointer is in the from-space
    (heap-set! (+ record delta) (move-loc pointer))))

;; maybe-move/roots : loc[to-space] (listof root) -> void
(define (maybe-move/roots record roots)
  (cond
    [(null? roots) (void)]
    [else 
     (maybe-move/root record (car roots))
     (maybe-move/roots record (cdr roots))]))

;; maybe-move/root : loc[to-space] root -> void
;; moves the pointer in the root if it is in a different
;; semispace from record.
(define (maybe-move/root record root)
  (let ([pointer (read-root root)])
    (unless (different-halves? record pointer)
      (error 'maybe-move/root "tried to move a pointer that was in the from space already"))
    ;; now we know pointer is in the from-space
    (set-root! root (move-loc pointer))))
  
;; different-halves? : loc loc -> boolean
;; returns #t if n and m are in different halves of the heap.
(define (different-halves? n m)
  (cond
    [(or (immediate-loc? n) 
         (immediate-loc? m))
     #f]
    [else
     (not (equal? (< n (second-start))
                  (< m (second-start))))]))
     
(test (different-halves? 2 3) #f)

(test (with-heap (mkheap 10 15 
                         'free 'free 'free 'free 'free
                         'free 'free 'free 'free 'free)
                 (different-halves? 12 13))
      #f)
(test (with-heap (mkheap 10 15 
                         'free 'free 'free 'free 'free
                         'free 'free 'free 'free 'free)
                 (different-halves? 12 17))
      #t)
(test (with-heap (mkheap 10 15 
                         'free 'free 'free 'free 'free
                         'free 'free 'free 'free 'free)
                 (different-halves? 16 17))
      #f)
(test (with-heap (mkheap 10 15 
                         'free 'free 'free 'free 'free
                         'free 'free 'free 'free 'free)
                 (different-halves? 17 12))
      #t)

(test (with-heap (mkheap 17 20
                         'fwd  17 'junk 'free 'free 'free 'free
                         'pair 11  11   'free 'free 'free 'free)
                 (different-halves? 17 11))
      #t)

(test (let ([h (mkheap 17 22
                       'fwd  17    'free 'free 'free 'free 'free
                       'flat 11    'pair  17   10    'free 'free)])
        (with-heap h (maybe-move/loc 19 2))
        h)
      (mkheap 17 22
              'fwd  17    'free 'free 'free 'free 'free
              'flat 11    'pair  17   17    'free 'free))

(test (let ([h (mkheap 17 22
                       'flat 12    'free 'free 'free 'free 'free
                       'flat 11    'pair  17   10    'free 'free)])
        (with-heap h (maybe-move/loc 19 2))
        h)
      (mkheap 17 24
              'fwd  22    'free 'free 'free 'free 'free
              'flat 11    'pair  17    22    'flat '12))

(test (let ([h (mkheap 17 19
                       'free 'free 'free 'free 'free 'free 'free
                       'flat 11    'free 'free 'free 'free 'free)])
        (with-heap h (copy-data))
        h)
      (mkheap 19 19
              'free 'free 'free 'free 'free 'free 'free
              'flat 11    'free 'free 'free 'free 'free))

(test (let ([h (mkheap 17 20
                       'fwd  17 'junk 'free 'free 'free 'free
                       'pair 10  10   'free 'free 'free 'free)])
        (with-heap h (copy-data))
        h)
      (mkheap 20 20
              'fwd  17 'junk 'free 'free 'free 'free
              'pair 17  17   'free 'free 'free 'free))

(test (let ([h (mkheap 17 20
                       'fwd  17 'flat 112   'free 'free 'free
                       'pair 12  10   'free 'free 'free 'free)])
        (with-heap h (copy-data))
        h)
      (mkheap 22 22
              'fwd  17 'fwd  20    'free 'free 'free
              'pair 20  17   'flat 112 'free 'free))

(test (gc:alloc-flat 1)
      6)


(test (let ([h (mkheap 15 17
                       'flat 17 'pair 10    10    'free 'free
                       'pair 12  10   'free 'free 'free 'free)])
        (list (with-heap h (gc:alloc-flat 111))
              h))
      (list 15
            (mkheap 17 17
                    'flat  17 'pair 10    10   'flat 111
                    'pair 12  10   'free 'free 'free 'free)))

(test (let ([h (mkheap 14 17
                       'flat 17 'flat 12    'free 'free 'free
                       'pair 12  10   'free 'free 'free 'free)])
        (list (with-heap h (gc:cons 10 10))
              h))
      (list 14
            (mkheap 17 17
                    'flat 17 'flat 12    'pair 10    10
                    'pair 12  10   'free 'free 'free 'free)))
