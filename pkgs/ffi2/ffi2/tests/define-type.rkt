#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type percentage_t double_t
  #:predicate (lambda (v) (and (real? v) (<= 0.0 v 100.0)))
  #:racket->c (lambda (v) (/ v 100.0))
  #:c->racket (lambda (v) (* v 100.0)))

(check-true (percentage_t? 100.0))
(check-true (percentage_t? 0.0))
(check-true (percentage_t? 25))
(check-false (percentage_t? 101.0))
(check-false (percentage_t? -1.0))

(let ()
  (define p (ffi2-malloc double_t))
  (ffi2-set! p double_t 0.5)
  (check-equal? (ffi2-ref p double_t) 0.5)
  (check-equal? (ffi2-ref p percentage_t) 50.0)
  (ffi2-set! p percentage_t 25.5)
  (check-equal? (ffi2-ref p double_t) 0.255)
  (ffi2-set! p percentage_t #e5.25)
  (check-equal? (ffi2-ref p double_t) 0.0525))

(let ()
  (define pct-increment-ptr (ffi2-callback (lambda (v)
                                             (+ v 1.0))
                                           (percentage_t . -> . percentage_t)))
  (define increment-by-0.01 (ffi2-procedure pct-increment-ptr
                                            (double_t . -> . double_t)))
  (check-equal? (increment-by-0.01 0.0) 0.01)
  (define increment-by-1 (ffi2-procedure pct-increment-ptr
                                         (percentage_t . -> . percentage_t)))
  (check-equal? (increment-by-1 10.0) 11.0)
  (void (black-box pct-increment-ptr)))

(for ([i (in-range 10)])
  (define-ffi2-type percentage_box_t/gcable ptr_t/gcable
    #:predicate (lambda (bx) (percentage_t? (unbox bx)))
    #:racket->c (lambda (bx)
                  (define ptr (ffi2-malloc percentage_t))
                  (ffi2-set! ptr percentage_t (unbox bx))
                  ptr)
    #:c->racket (lambda (ptr)
                  (box (ffi2-ref ptr percentage_t))))

  (let ()
    (define p (ffi2-malloc #:gcable-traced ptr_t))
    (ffi2-set! p percentage_box_t/gcable (box 50.5))
    (check-true (percentage_box_t/gcable? (ffi2-ref p percentage_box_t/gcable)))
    (check-equal? (ffi2-ref p percentage_box_t/gcable) (box 50.5))
    (check-equal? (ffi2-ref (ffi2-ref p ptr_t/gcable) double_t) 0.505)))

(for ([i (in-range 10)])
  (define-ffi2-type percentage_box_t ptr_t
    #:predicate (lambda (bx) (percentage_t? (unbox bx)))
    #:racket->c (lambda (bx)
                  (define ptr (ffi2-malloc #:gcable-immobile percentage_t))
                  (ffi2-set! ptr percentage_t (unbox bx))
                  ptr)
    #:c->racket (lambda (ptr)
                  (box (ffi2-ref ptr percentage_t))))
  (let ()
    (define p (ffi2-malloc #:gcable-traced ptr_t))
    (ffi2-set! p percentage_box_t (box 50.5))
    (check-true (percentage_box_t? (ffi2-ref p percentage_box_t)))
    (check-equal? (ffi2-ref p percentage_box_t) (box 50.5))
    (check-equal? (ffi2-ref (ffi2-ref p ptr_t) double_t) 0.505)
    (void (black-box p))))

(for ([i (in-range 10)])
  (define-ffi2-type percentage_box_t ptr_t
    #:predicate (lambda (bx) (percentage_t? (unbox bx)))
    #:racket->c (lambda (bx)
                  (define ptr (ffi2-malloc #:manual percentage_t))
                  (ffi2-set! ptr percentage_t (unbox bx))
                  ptr)
    #:c->racket (lambda (ptr)
                  (begin0
                    (box (ffi2-ref ptr percentage_t))
                    (ffi2-free ptr))))
  (let ()
    (define p (ffi2-malloc #:gcable-traced ptr_t))
    (ffi2-set! p percentage_box_t (box 50.5))
    (check-equal? (ffi2-ref (ffi2-ref p ptr_t) double_t) 0.505)
    (define also-p (ffi2-ref p percentage_box_t))
    (check-true (percentage_box_t? also-p))
    (check-equal? also-p (box 50.5))))

(let ()
  (define-ffi2-type percentage_t* void_t*
    #:tag #f ; => still a generic pointer
    #:predicate (lambda (v) (and (void_t*? v)
                                 (percentage_t? (ffi2-ref v percentage_t)))))
  
  (define p (ffi2-malloc double_t))
  (ffi2-set! p double_t 10.0)
  (check-false (percentage_t*? p))
  (ffi2-set! p double_t 0.75)
  (check-true (percentage_t*? p)))

(let ()
  (define-ffi2-type tagged_percentage_t* void_t*
    #:tag percentage_t*)
  (define-ffi2-type percentage_t* void_t*
    #:predicate (lambda (v) (and (tagged_percentage_t*? v)
                                 (percentage_t? (ffi2-ref v percentage_t)))))  
  (define p0 (ffi2-malloc double_t))
  (define p (ffi2-malloc double_t #:as percentage_t*))
  (ffi2-set! p double_t 10.0)
  (check-false (percentage_t*? p))  
  (ffi2-set! p0 double_t 0.75)
  (ffi2-set! p double_t 0.75)
  (check-false (percentage_t*? p0))
  (check-true (percentage_t*? p)))
