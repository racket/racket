#lang plai/gc2/collector
(define heap-ptr 'uninitialized-heap-ptr)

(define (init-allocator)
  ; calling heap-offset before init-allocator is called gives 'undefined
 (set! heap-ptr 0))

(define (gc:closure code roots)
  (define len (length roots))
  (when (> (+ heap-ptr len) (heap-size))
        (error "out of memory"))
  (heap-set! heap-ptr 'closure)
  (heap-set! (+ 1 heap-ptr) code)
  (for ([r (in-list roots)]
        [i (in-naturals 1)])
    (heap-set! (+ 1 i heap-ptr) (read-root r)))
  (set! heap-ptr (+ len 2 heap-ptr))
  (- heap-ptr len 2))

(define (gc:closure-code-ptr a)
  (heap-ref (+ a 1)))
(define (gc:closure-env-ref a i)
  (heap-ref (+ a 1 1 i)))
(define (gc:closure? a)
  (eq? (heap-ref a) 'closure))

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
    (heap-set! (+ 1 heap-ptr) (read-root f))
    (heap-set! (+ 2 heap-ptr) (read-root r))
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

(module+ test
  (require rackunit)
  
  (check-equal? (let ([h (make-vector 7)])
                  (with-heap 
                   h
                   (init-allocator)
                   (define one (gc:alloc-flat 1))
                   (define clos (gc:closure 'something (list (make-root 'dummy (λ () one) void))))
                   (gc:alloc-flat 2))
                  h)
                (vector 'prim 1 'closure 'something 0 'prim 2)))
