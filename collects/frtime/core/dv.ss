#lang scheme
(require "contract.ss")

(define-struct dv (real used vec) #:mutable)

(define (dv:make size)
  (make-dv size 0 (make-vector size)))

(define (dv:length dv) (dv-used dv))

(define (dv:remove-last a-dv)
  (match a-dv
    [(struct dv (_ used vec))
     (set-dv-used! a-dv (sub1 used))
     (vector-set! vec used 0)]))

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
         (begin (set-dv-used! a-dv (add1 used))
                (vector-set! vec used item))
         (let ([new-vec 
                (build-vector
                 (* 2 real)
                 (lambda (i) 
                   (if (i . < . used)
                       (vector-ref vec i)
                       0)))])
           (set-dv-vec! a-dv new-vec)
           (set-dv-real! a-dv (* 2 real))
           (set-dv-used! a-dv (add1 used))
           (vector-set! new-vec used item)))]))

(provide/contract*
 [dv:make (exact-nonnegative-integer? . -> . dv?)]
 [dv:length (dv? . -> . exact-nonnegative-integer?)]
 [dv:remove-last (dv? . -> . void)]
 [dv:ref (->d ([dv dv?] [pos exact-nonnegative-integer?]) () 
              #:pre-cond (pos . < . (dv:length dv))
              [r any/c])]
 [dv:set! (->d ([dv dv?] [pos exact-nonnegative-integer?] [val any/c]) () 
               #:pre-cond (pos . < . (dv:length dv))
               [r void])]
 [dv:append (dv? any/c . -> . void)])