#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type triple_t (array int32_t 3))
(define-ffi2-type int32_t* (array int32_t *))
(define-ffi2-type also_int32_t* void_t* #:tag int32_t*)

(check-equal? (ffi2-sizeof triple_t) 12)
(check-equal? (ffi2-sizeof int32_t*) (ffi2-sizeof ptr_t))
(check-equal? (ffi2-sizeof int32_t*/gcable) (ffi2-sizeof ptr_t))

(define p (ffi2-malloc triple_t))
(check-true (triple_t? p))
(check-true (int32_t*? p))
(check-true (also_int32_t*? p))
(check-true (ptr_t/gcable? p))

(check-false (triple_t? (ffi2-malloc int32_t)))

(check-equal? (triple_t-set! p 0 101) (void))
(check-equal? (triple_t-set! p 1 102) (void))
(check-equal? (triple_t-set! p 2 103) (void))

(check-equal? (triple_t-ref p 0) 101)
(check-equal? (triple_t-ref p 1) 102)
(check-equal? (triple_t-ref p 2) 103)

(check-exn exn:fail:contract? (lambda () (triple_t-ref p -1)))
(check-exn exn:fail:contract? (lambda () (triple_t-ref p "x")))
(check-exn exn:fail:contract? (lambda () (triple_t-ref p 3)))

(check-exn exn:fail:contract? (lambda () (triple_t-set! p -1 0)))
(check-exn exn:fail:contract? (lambda () (triple_t-set! p "x" 0)))
(check-exn exn:fail:contract? (lambda () (triple_t-set! p 3 0)))

(check-exn exn:fail:contract? (lambda () (triple_t-set! p 0 0.0)))

(define-ffi2-type point_t (struct
                            [x int_t]
                            [y int_t]))
(define-ffi2-type triangle_t (array point_t 3))

(define tp (ffi2-malloc triangle_t))
(check-true (triangle_t? tp))
(check-true (point_t*? tp))
(set-point_t-x! (triangle_t-ref tp 1) 100)
(check-equal? (point_t-x (triangle_t-ref tp 1)) 100)
(check-equal? (ffi2-ref tp int_t 2) 100)

(triangle_t-set! tp 2 (point_t 2 20))
(check-equal? (point_t-x (triangle_t-ref tp 2)) 2)
(check-equal? (ffi2-ref tp int_t 4) 2)
(check-equal? (ffi2-ref tp int_t 5) 20)

(define pp (ffi2-ref tp point_t 2))
(check-equal? (point_t-x pp) 2)
(check-equal? (point_t-y pp) 20)

(ffi2-set! tp point_t 0 pp)
(define pp0 (ffi2-ref tp point_t 0))
(check-equal? (point_t-x pp0) 2)
(check-equal? (point_t-y pp0) 20)

(define-ffi2-type int_pointers_t (array ptr_t/gcable 3))
(define ips (ffi2-malloc #:gcable-traced int_pointers_t))
(define p1 (ffi2-malloc int_t 3))
(int_pointers_t-set! ips 0 p1)
(check-true (ptr_t/gcable? (int_pointers_t-ref ips 0)))
(check-false (ptr_t/gcable? (int_pointers_t-ref ips 1))) ; should be null pointer
