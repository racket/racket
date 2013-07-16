#lang plai/collector
(define heap-ptr 'uninitialized-heap-ptr)

(define (init-allocator)
  ; calling heap-offset before init-allocator is called gives 'undefined
 (set! heap-ptr 0))

(define (gc:alloc-flat p)
  (begin 
    (when (> (+ heap-ptr 2) (heap-size))
      (error "out of memory"))
    (heap-set! heap-ptr 'prim)
    (heap-set! (+ 1 heap-ptr) p)
    (set! heap-ptr (+ 2 heap-ptr))
    ; return the location of this flat data
    (- heap-ptr 2)))

(define (gc:cons f r)
  (begin
    (when (> (+ heap-ptr 3) (heap-size))
      (error "out of memory"))
    (heap-set! heap-ptr 'cons)
    (heap-set! (+ 1 heap-ptr) f)
    (heap-set! (+ 2 heap-ptr) r)
    (set! heap-ptr (+ 3 heap-ptr))
    (- heap-ptr 3)))

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

