#lang racket/base
(require ffi2
         racket/flonum
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

(define-test-procedure sum_doubles (flvector_ptr_t . -> . double_t))
(define-test-procedure sum_doubles_ptr (ptr_t . -> . double_t)
  #:c-id sum_doubles)

(let ()
  (define flv (flvector 1.0 2.0))
  (define p (ffi2-cast flv #:from flvector_ptr_t #:to ptr_t))
  (check-true (ptr_t? p))
  (check-true (ptr_t/gcable? p))
  (check-equal? 1.0 (ffi2-ref p double_t))
  (check-equal? 2.0 (ffi2-ref p double_t 1))
  (check-equal? 3.0 (sum_doubles flv))
  (check-equal? 3.0 (sum_doubles_ptr p))
  (check-exn (lambda (x)
               (and (exn:fail:contract? x)
                    (regexp-match? #rx"cannot convert" (exn-message x))))
             (lambda ()
               (ffi2-ref p flvector_ptr_t))))
