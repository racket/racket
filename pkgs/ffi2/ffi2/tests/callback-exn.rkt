#lang racket/base
(require ffi2
         ffi/unsafe/global
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(define-ffi2-definer define-test-procedure #:lib test-lib)

(define-ffi2-type to_double_t (int_t float_t . -> . double_t))

(define-test-procedure double_built (to_double_t . -> . double_t
                                                 #:allow-callback-exn))
(define-test-procedure double_built_errno (to_double_t . -> . double_t #:errno
                                                       #:allow-callback-exn)
  #:c-id double_built)

(define N 102400) ; enough iterations to trigger stack over if calls not unwound
(define done 0)

(for ([i (in-range N)])
  (set! done (add1 done))
  (check-equal? (double_built (lambda (i f) (+ i f))) 220.0)
  (case (modulo i 3)
    [(2)
     (thread-wait
      (thread
       (lambda ()
         (check-exn exn:fail:user?
                    (lambda ()
                      (double_built (lambda (i f) (raise-user-error "oops"))))))))]
    [else
     (check-exn exn:fail:user?
                (lambda ()
                  (double_built (lambda (i f) (raise-user-error "oops")))))]))

(check-equal? done N)

(check-equal? (let-values ([(r e) (double_built_errno (lambda (i f) (+ i f)))])
                r)
              220.0)

(clean-ffi2-lib)
