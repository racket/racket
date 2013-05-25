#lang racket/base

(require racket/match
         "contract.rkt")

(define-struct dv (vec-length next-avail-pos vec) #:mutable)

(define (dv:make size)
  (make-dv size 0 (make-vector size)))

(define (dv:length dv) (dv-next-avail-pos dv))

(define (dv:remove-last a-dv)
  (match a-dv
    [(struct dv (_ used vec))
     (set-dv-next-avail-pos! a-dv (sub1 used))
     (vector-set! vec (sub1 used) 0)]))

(define (dv:ref a-dv pos)
  (match a-dv
    [(struct dv (_ _ vec))
     (vector-ref vec pos)]))

(define (dv:set! a-dv pos new-val)
  (match a-dv
    [(struct dv (_ _ vec))
     (vector-set! vec pos new-val)]))

(define (dv:append a-dv item)
  (match a-dv
    [(struct dv (real used vec))
     (if (used . < . real)
         (begin (set-dv-next-avail-pos! a-dv (add1 used))
                (vector-set! vec used item))
         (let ([new-vec 
                (build-vector
                 (* 2 real)
                 (lambda (i) 
                   (if (i . < . used)
                       (vector-ref vec i)
                       0)))])
           (set-dv-vec! a-dv new-vec)
           (set-dv-vec-length! a-dv (* 2 real))
           (dv:append a-dv item)))]))

(define (non-empty-dv? dv)
  ((dv:length dv) . > . 0))

(provide/contract*
 [dv? (any/c . -> . boolean?)]
 [dv:make (exact-nonnegative-integer? . -> . dv?)]
 [dv:length (dv? . -> . exact-nonnegative-integer?)]
 [dv:remove-last (non-empty-dv? . -> . void)]
 [dv:ref (->d ([dv dv?] [pos exact-nonnegative-integer?]) () 
              #:pre-cond (pos . < . (dv:length dv))
              [r any/c])]
 [dv:set! (->d ([dv dv?] [pos exact-nonnegative-integer?] [val any/c]) () 
               #:pre-cond (pos . < . (dv:length dv))
               [r void])]
 [dv:append (dv? any/c . -> . void)])
