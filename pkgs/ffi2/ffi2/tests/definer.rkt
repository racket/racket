#lang racket/base
(require ffi2
         rackunit
         "make-ffi2-lib.rkt")

(define-values (test-lib clean-ffi2-lib)
  (build-ffi2-lib))

(let ()
  (define-ffi2-definer define-test-procedure
    #:lib test-lib)

  (define-test-procedure double_sum (double_t double_t . -> . double_t))
  (check-equal? (double_sum 3.25 4.0) 7.25)

  (define-test-procedure list_double_sum (double_t double_t . -> . double_t)
    #:c-id double_sum
    #:wrap (lambda (proc) (lambda (a b) (list (proc a b)))))
  (check-equal? (list_double_sum 3.25 4.0) (list 7.25))

  (define-test-procedure nonesuch_double_sum (double_t double_t . -> . double_t)
    #:fail (lambda (name) 'oops))
  (check-equal? nonesuch_double_sum 'oops)
  
  (define-test-procedure still_nonesuch_double_sum (double_t double_t . -> . double_t)
    #:fail make-not-available)
  (check-true (procedure? still_nonesuch_double_sum))
  (check-exn exn:fail? (lambda () (still_nonesuch_double_sum 1.0 2.0))))

(let ()
  (define-ffi2-definer define-test-procedure
    #:lib test-lib
    #:default-fail (lambda (name) (lambda args name)))

  (define-test-procedure double_sum (double_t double_t . -> . double_t))
  (check-equal? (double_sum 3.25 4.0) 7.25)

  (define-test-procedure list_double_sum (double_t double_t . -> . double_t)
    #:c-id double_sum
    #:wrap (lambda (proc) (lambda (a b) (list (proc a b)))))
  (check-equal? (list_double_sum 3.25 4.0) (list 7.25))

  (define-test-procedure nonesuch_double_sum (double_t double_t . -> . double_t))
  (check-equal? (nonesuch_double_sum 1.0) 'nonesuch_double_sum)

  (define-test-procedure still_nonesuch_double_sum (double_t double_t . -> . double_t)
    #:fail make-not-available)
  (check-true (procedure? still_nonesuch_double_sum))
  (check-exn exn:fail? (lambda () (still_nonesuch_double_sum 1.0 2.0))))

(let ()
  (define-ffi2-definer define-test-procedure
    #:lib test-lib
    #:default-wrap  (lambda (proc) (lambda (a b) (vector (proc a b)))))

  (define-test-procedure double_sum (double_t double_t . -> . double_t))
  (check-equal? (double_sum 3.25 4.0) (vector 7.25))

  (define-test-procedure list_double_sum (double_t double_t . -> . double_t)
    #:c-id double_sum
    #:wrap (lambda (proc) (lambda (a b) (list (proc a b)))))
  (check-equal? (list_double_sum 3.25 4.0) (list 7.25))

  (define-test-procedure still_nonesuch_double_sum (double_t double_t . -> . double_t)
    #:fail make-not-available)
  (check-true (procedure? still_nonesuch_double_sum))
  (check-exn exn:fail? (lambda () (still_nonesuch_double_sum 1.0 2.0))))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib
               #:default-fail 'oops)
             (void)))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib
               #:default-fail (lambda () 'oops))
             (void)))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib
               #:default-wrap 'oops)
             (void)))

(check-exn exn:fail:contract?           
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib
               #:default-wrap (lambda () 'oops))
             (void)))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib)
             (define-test-procedure double_sum (double_t double_t . -> . double_t)
               #:fail 'oops)
             (void)))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib)
             (define-test-procedure double_sum (double_t double_t . -> . double_t)
               #:fail (lambda () 'oops))
             (void)))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib)
             (define-test-procedure double_sum (double_t double_t . -> . double_t)
               #:wrap 'oops)
             (void)))

(check-exn exn:fail:contract?
           (lambda ()
             (define-ffi2-definer define-test-procedure
               #:lib test-lib)
             (define-test-procedure double_sum (double_t double_t . -> . double_t)
               #:wrap (lambda () 'oops))
             (void)))

(define-ffi2-definer define/provide-test-procedure
  #:lib test-lib
  #:provide)

(define/provide-test-procedure double_sum_out (double_t double_t . -> . double_t)
  #:c-id double_sum)

(module* uses-provided-name racket/base
  (require (submod ".."))
  (void double_sum_out))

(clean-ffi2-lib)
