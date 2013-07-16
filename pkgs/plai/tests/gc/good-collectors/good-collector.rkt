#lang plai/collector

#|

A collector for use in testing the random mutator generator.

|#

(print-only-errors #t)

(define (find-free-space start size)
  (cond
    [(= start (heap-size))
     #f]
    [(n-free-blocks? start size)
     start]
    [else
     (find-free-space (+ start 1) size)]))

(define (n-free-blocks? start size)
  (cond
    [(= size 0) #t]
    [(= start (heap-size)) #f]
    [else 
     (and (eq? 'free (heap-ref start))
          (n-free-blocks? (+ start 1) (- size 1)))]))

(test (with-heap #(free free free)
                 (n-free-blocks? 0 2))
      #t)
(test (with-heap #(free free free)
                 (n-free-blocks? 0 3))
      #t)
(test (with-heap #(free free free)
                 (n-free-blocks? 0 4))
      #f)
(test (with-heap #(free free free)
                 (n-free-blocks? 2 1))
      #t)
(test (with-heap #(free free free)
                 (n-free-blocks? 2 2))
      #f)

(test (with-heap #(free free free)
                 (find-free-space 0 1))
      0)
(test (with-heap #(pair free free)
                 (find-free-space 0 1))
      1)
(test (with-heap #(pair free free)
                 (find-free-space 0 2))
      1)
(test (with-heap #(pair free free)
                 (find-free-space 0 3))
      #f)

(define (init-allocator) 
  (for ([i (in-range 0 (heap-size))])
    (heap-set! i 'free)))

(test (let ([v (make-vector 12 'x)])
        (with-heap v (init-allocator))
        v)
      (make-vector 12 'free))

(define (gc:deref loc) 
  (cond
    [(equal? (heap-ref loc) 'flat)
     (heap-ref (+ loc 1))]
    [else
     (error 'gc:deref "attempted to deref a non flat value, loc ~s" loc)]))

(test (with-heap (vector 'free 'free 'free 'flat 14 'free 'free)
                 (gc:deref 3))
      14)

(define (gc:first pr-ptr) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 1))
      (error 'first "non pair")))

(test (with-heap (vector 'free 'flat 3 'pair 0 1)
                 (gc:first 3))
      0)

(define (gc:rest pr-ptr) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-ref (+ pr-ptr 2))
      (error 'first "non pair")))

(test (with-heap (vector 'free 'flat 3 'pair 0 1)
                 (gc:rest 3))
      1)

(define (gc:flat? loc) (equal? (heap-ref loc) 'flat))

(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 2))
      #f)
(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:flat? 5))
      #t)

(define (gc:cons? loc) (equal? (heap-ref loc) 'pair))

(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 2))
      #t)
(test (with-heap (vector 'free 'free 'pair 0 1 'flat 14)
                 (gc:cons? 5))
      #f)

(define (gc:set-first! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 1) new)
      (error 'set-first! "non pair")))

(define (gc:set-rest! pr-ptr new) 
  (if (equal? (heap-ref pr-ptr) 'pair)
      (heap-set! (+ pr-ptr 2) new)
      (error 'set-first! "non pair")))


(define (gc:alloc-flat fv) 
  (let ([ptr (alloc 2 (λ () 
                        (if (procedure? fv)
                            (append (procedure-roots fv)
                                    (get-root-set))
                            (get-root-set))))])
    (heap-set! ptr 'flat)
    (heap-set! (+ ptr 1) fv)
    ptr))

(define (gc:cons hd tl)
  (let ([ptr (alloc 3 (λ () (get-root-set hd tl)))])
    (heap-set! ptr 'pair)
    (heap-set! (+ ptr 1) hd)
    (heap-set! (+ ptr 2) tl)
    ptr))

(define (alloc n get-roots)
  (let ([next (find-free-space 0 n)])
    (cond
      [next
       next]
      [else
       (collect-garbage get-roots)
       (let ([next (find-free-space 0 n)])
         (unless next
           (error 'alloc "out of space"))
         next)])))

(define (collect-garbage get-roots)
  (let ([roots (map read-root (get-roots))])
    ;; (eprintf "roots: ~a\n" roots)
    (collect-garbage-help roots
                          (remove* roots (get-all-records 0)))))

(define (collect-garbage-help gray white)
  (cond
    [(null? gray) (free! white)]
    [else
     (case (heap-ref (car gray))
       [(flat) 
        (let ([proc (heap-ref (+ (car gray) 1))])
          (if (procedure? proc)
              (let ([new-locs (map read-root (procedure-roots proc))])
                ;; (eprintf "proc roots: ~a\n" new-locs)
                (collect-garbage-help 
                 (add-in new-locs (cdr gray) white)
                 (remove* new-locs white)))
              (collect-garbage-help (cdr gray) white)))]
       [(pair) 
        (let ([hd (heap-ref (+ (car gray) 1))]
              [tl (heap-ref (+ (car gray) 2))])
          (collect-garbage-help 
           (add-in (list hd tl) (cdr gray) white)
           (remove tl (remove hd white))))]
       [else
        (error 'collect-garbage "unknown tag ~s, loc ~s" (heap-ref (car gray)) (car gray))])]))

(define (free! whites)
  (cond
    [(null? whites) (void)]
    [else
     (let ([white (car whites)])
       (case (heap-ref white)
         [(pair) 
          (heap-set! white 'free)
          (heap-set! (+ white 1) 'free)
          (heap-set! (+ white 2) 'free)]
         [(flat)
          (heap-set! white 'free)
          (heap-set! (+ white 1) 'free)]
         [else 
          (error 'free! "unknown tag ~s\n" (heap-ref white))])
       (free! (cdr whites)))]))

(test (let ([v (vector #f #t '() 0 1 2 3 4 5 6 'pair 0 1 'flat 14 'pair 0 1 'flat 12)])
        (with-heap v (free! (list 10 18)))
        v)
      (vector #f #t '() 0 1 2 3 4 5 6 'free 'free 'free 'flat 14 'pair 0 1  'free 'free))
      
;; add-in : (listof location) (listof location) (listof location) -> (listof location)
;; computes a new set of gray addresses by addding all white elements of locs to gray
(define (add-in locs gray white)
  (cond
    [(null? locs) gray]
    [else
     (let* ([loc (car locs)]
            [white? (member loc white)])
       (add-in (cdr locs)
               (if white? (cons loc gray) gray)
               white))]))

(test (add-in '(13 14) '(100 102) '(13 14 104 105))
      '(14 13 100 102))

(test (add-in '(13 14) '(100 102) '(13 104 105))
      '(13 100 102))

(define (get-all-records i)
  (cond
    [(< i (heap-size))
     (case (heap-ref i)
       [(pair) (cons i (get-all-records (+ i 3)))]
       [(flat) (cons i (get-all-records (+ i 2)))]
       [(free) (get-all-records (+ i 1))]
       [else (get-all-records (+ i 1))])]
    [else null]))

(test (with-heap (vector #f #t '() 0 1 2 3 4 5 6 'pair 0 1 'flat 14 'pair 0 1 'flat 12)
                 (get-all-records 0))
      (list 10 13 15 18))

(test (with-heap (make-vector 10 'free) (gc:alloc-flat #f))
      0)

(test (with-heap (make-vector 10 'free) (gc:alloc-flat #t) (gc:alloc-flat #f))
      2)

(test (let ([v (vector 'flat 0 'flat 1)])
        (with-heap v (collect-garbage-help (list) 
                                           (get-all-records 0)))
        v)
      (vector 'free 'free 'free 'free))

(test (let ([v (vector 'flat 0 'flat 1)])
        (with-heap v (collect-garbage-help (list 0) 
                                           (remove 0 (get-all-records 0))))
        v)
      (vector 'flat 0 'free 'free))

(test (let ([v (vector 'flat 0 'flat 1)])
        (with-heap v (collect-garbage-help (list 2) 
                                           (remove 2 (get-all-records 0))))
        v)
      (vector  'free 'free 'flat 1))

(test (let ([v (vector 'flat 0 'flat 1 'pair 0 2)])
        (with-heap v (collect-garbage-help (list 4) 
                                           (remove 4 (get-all-records 0))))
        v)
      (vector 'flat 0 'flat 1 'pair 0 2))

(test (let ([v (vector 'flat 0 'flat 1 'pair 0 0)])
        (with-heap v (collect-garbage-help (list 4) 
                                           (remove 4 (get-all-records 0))))
        v)
      (vector 'flat 0 'free 'free 'pair 0 0))

(test (let ([v (vector 'flat 0 'flat 1 'pair 4 4)])
        (with-heap v (collect-garbage-help (list 4) 
                                           (remove 4 (get-all-records 0))))
        v)
      (vector 'free 'free 'free 'free 'pair 4 4))

(test (with-heap (make-vector 50)
                 (with-roots (list 1 2 3)
                             (get-root-set)))
      (list 1 2 3))
(test (with-heap (make-vector 50)
                 (with-roots (list 1 2 3)
                             (with-roots (list 4 5 6)
                                         (sort (get-root-set) <))))
      (list 1 2 3 4 5 6))
