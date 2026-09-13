#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type int_t* (array_t int_t *))

(define p (ffi2-cast '(1 2 3) #:from (list_t int_t) #:to int_t*))
(check-equal? (int_t*-ref p 0) 1)
(check-equal? (int_t*-ref p 1) 2)
(check-equal? (int_t*-ref p 2) 3)

(check-equal? (ffi2-cast p #:to (list_t int_t #:length 1)) '(1))
(check-equal? (ffi2-cast p #:to (list_t int_t #:length 3)) '(1 2 3))

(check-exn exn:fail:contract?
           (lambda () (ffi2-cast p #:to (list_t int_t #:length "oops"))))

(check-equal? (ffi2-cast p #:to (vector_t int_t #:length 1)) '#(1))
(check-equal? (ffi2-cast p #:to (vector_t int_t #:length 3)) '#(1 2 3))

(define p2 (ffi2-cast '#(10 20 30 40) #:from (vector_t int_t) #:to int_t*))
(check-equal? (int_t*-ref p2 0) 10)
(check-equal? (int_t*-ref p2 1) 20)
(check-equal? (int_t*-ref p2 2) 30)
(check-equal? (int_t*-ref p2 3) 40)

(let ()
  (define p (ffi2-cast '(1 2 3) #:from (list_t #:manual int_t) #:to int_t*))
  (ffi2-free p))

