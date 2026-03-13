#lang racket/base
(require ffi2
         rackunit)

(define-ffi2-type int32_t* (array int32_t *))

(define p (ffi2-malloc 32))

(check-false (int32_t*? p))

(check-true (int32_t*? (ffi2-cast p #:to int32_t*)))
(int32_t*-set! (ffi2-cast p #:to int32_t*) 0 11)
(check-equal? (int32_t*-ref (ffi2-cast p #:to int32_t*) 0) 11)
(check-exn exn:fail:contract? (lambda () (ffi2-cast p #:from int32_t*)))

(int32_t*-set! (ffi2-cast p #:to int32_t* #:offset int32_t 1) 0 12)
(check-equal? (int32_t*-ref (ffi2-cast p #:to int32_t*) 1) 12)
(int32_t*-set! (ffi2-cast p #:to int32_t* #:offset int32_t 1) -1 9)
(check-equal? (int32_t*-ref (ffi2-cast p #:to int32_t*) 0) 9)

(check-equal? (ffi2-cast p #:to int32_t* #:offset int32_t 1)
              (ffi2-cast p #:to int32_t* #:offset 4))

(check-equal? (ffi2-add p int32_t 1)
              (ffi2-cast p #:to int32_t* #:offset 4))
(check-equal? (ffi2-add p 4)
              (ffi2-cast p #:to int32_t* #:offset 4))

(check-true (int32_t*? (ffi2-malloc int32_t)))
(check-true (int32_t*? (ffi2-malloc int32_t 5)))
(check-true (int32_t*? (ffi2-add p int32_t 1)))
