#lang racket/base
(require ffi2
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

(define-test-procedure add1_and_errno (int_t . -> . int_t #:errno))

(for-each
 thread-wait
 (for/list ([i (in-range 8)])
   (thread #:pool 'own
           (lambda ()
             (for ([j (in-range 4096)])
               (check-equal? (let-values ([(v errno) (add1_and_errno (+ 11 j))])
                               (list v errno))
                             (list (+ 12 j) (+ 11 j))))))))

(clean-ffi2-lib)
