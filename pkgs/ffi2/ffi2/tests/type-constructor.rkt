#lang racket/base
(require ffi2
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

;; ----------------------------------------

(define-ffi2-type (offset_double_t delta) double_t
  #:c->racket (lambda (v) (+ v delta))
  #:racket->c (lambda (v) (- v delta)))

(define-test-procedure double_sum ((offset_double_t 1.0) (offset_double_t 2.0) . -> . (offset_double_t 4.0)))
(check-equal? (double_sum 10.0 20.0) 31.0)

(define-ffi2-type posn_t (struct_t
                           [x (offset_double_t 1.0)]
                           [y (offset_double_t 2.0)]))
(define p1 (posn_t 10.0 20.0))
(check-equal? (ffi2-ref p1 double_t 0) 9.0)
(check-equal? (ffi2-ref p1 double_t 1) 18.0)
(set-posn_t-x! p1 100.0)
(check-equal? (posn_t-x p1) 100.0)
(check-equal? (ffi2-ref p1 double_t 0) 99.0)

(define-ffi2-type od_array_t (array_t (offset_double_t 0.5) 3))
(define-ffi2-type od_array_t* void_t*)
(define a0 (ffi2-malloc (array_t (offset_double_t 0.5) 3)))
(define a1 (ffi2-malloc od_array_t))
(od_array_t-set! a1 0 3.0)
(check-equal? (od_array_t-ref a1 0) 3.0)
(check-equal? (ffi2-ref a1 double_t 0) 2.5)

(check-exn (lambda (exn)
             (and (exn:fail:syntax? exn)
                  (regexp-match? #rx"incorrect number of arguments to ffi2 type constructor" (exn-message exn))))
           (lambda ()
             (parameterize ([current-namespace (make-base-namespace)])
               (namespace-require 'ffi2)
               (eval '(define-ffi2-type (complex_t x) int_t))
               (eval '(ffi2-cast 0 #:from (complex_t 1 2 3))))))

;; ----------------------------------------

(clean-ffi2-lib)
