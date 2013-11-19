#lang plai/gc2/collector
#|

This is just like trivial-collector.rkt, except
it moves all addresses forward by one on each
allocation

|#

(define heap-ptr 'uninitialized-heap-ptr)
(define starting-point 0)

(define (alloc n roots)
  (when (> (+ heap-ptr n) (heap-size))
    (error "out of memory"))
  
  ;; slide everything in the heap forward by one address
  (for ([i (in-range (- heap-ptr 1) (- starting-point 1) -1)])
    (heap-set! (+ i 1) (heap-ref i)))
  (heap-set! starting-point 'before-start)
  (set! starting-point (+ starting-point 1))
  (set! heap-ptr (+ heap-ptr 1))

  ;; update all the roots
  (for ([root (in-list (append roots (get-root-set)))])
    (set-root! root (+ (read-root root) 1)))
  
  ;; update all the internal pointers
  (let loop ([addr starting-point])
    (when (< addr heap-ptr)
      (case (heap-ref addr)
        [(closure)
         (define size (heap-ref (+ addr 2)))
         (for ([i (in-range size)])
           (inc-at-addr (+ addr 3 i)))
         (loop (+ addr size 3))]
        [(prim)
         (loop (+ addr 2))]
        [(cons)
         (inc-at-addr (+ addr 1))
         (inc-at-addr (+ addr 2))
         (loop (+ addr 3))]
        [else (error 'alloc "unknown value at addr ~a: ~s" addr (heap-ref addr))])))
  
  ;; do the actual allocation
  (define old-heap-ptr heap-ptr)
  (set! heap-ptr (+ heap-ptr n))
  old-heap-ptr)

(define (print-heap)
  (for ([i (in-range (+ heap-ptr 4))])
    (printf "~a: ~a\n" i (heap-ref i)))
  (newline))

(define (inc-at-addr a)
  (heap-set! a (+ (heap-ref a) 1)))

(define (init-allocator)
  (set! heap-ptr 0))

(define (gc:closure code roots)
  (define len (length roots))
  (define heap-ptr (alloc (+ len 3) roots))
  (heap-set! heap-ptr 'closure)
  (heap-set! (+ heap-ptr 1) code)
  (heap-set! (+ heap-ptr 2) len)
  (for ([r (in-list roots)]
        [i (in-naturals 3)])
    (heap-set! (+ i heap-ptr) (read-root r)))
  heap-ptr)

(define (gc:closure-code-ptr a)
  (heap-ref (+ a 1)))
(define (gc:closure-env-ref a i)
  (heap-ref (+ a 3 i)))
(define (gc:closure? a)
  (eq? (heap-ref a) 'closure))

(define (gc:alloc-flat p)
  (define heap-ptr (alloc 2 '()))
  (heap-set! heap-ptr 'prim)
  (heap-set! (+ 1 heap-ptr) p)
  heap-ptr)

(define (gc:cons f r)
  (define heap-ptr (alloc 3 (list f r)))
  (heap-set! heap-ptr 'cons)
  (heap-set! (+ 1 heap-ptr) (read-root f))
  (heap-set! (+ 2 heap-ptr) (read-root r))
  heap-ptr)

(define (gc:deref a)
  (heap-ref (+ 1 a)))

; number -> boolean
(define (gc:cons? a)
  (eq? (heap-ref a) 'cons))

; number -> any
(define (gc:first a)
  (heap-ref (+ 1 a)))

; number -> number
(define (gc:rest a)
  (heap-ref (+ 2 a)))

(define (gc:set-first! a f)
  (if (gc:cons? a)
      (heap-set! (+ 1 a) f)
      (error 'set-first! "expects address of cons")))

(define (gc:set-rest! a r)
  (heap-set! (+ 2 a) r))

; function number -> boolean
(define (gc:flat? a)
  (eq? 'prim (heap-ref a)))
