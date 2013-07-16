#lang plai/gc2/collector

(define ptr 0)

(define (init-allocator) (void))

(define (gc:deref loc) #f)
(define (gc:alloc-flat hv) 0)
(define (gc:cons hd tl) 0)
(define (gc:first pr) 0)
(define (gc:rest pr) 0)
(define (gc:flat? loc) #t)
(define (gc:cons? loc) #f)
(define (gc:set-first! pr new) (void))
(define (gc:set-rest! pr new) (void))
(define (gc:closure code-ptr env) 0)
(define (gc:closure-code-ptr ptr) #f)
(define (gc:closure-env-ref ptr i) 0)
(define (gc:closure? ptr) #f)

(with-heap (vector 1 2 3)
  (test (gc:deref 0) #f))
