#lang racket/base

(require rackunit
         rackunit/private/check)

(define run? #f)

(define-test-suite define-test
  (check = 1 1))

(define/provide-test-suite test-suite-define-provide-test
  (check = 1 1))

(define test-suite-tests
  (test-suite
   "test-suite-tests"

   ;; We rely on order of evaluation to test that checks are
   ;; converted to test cases
   
   (test-begin
    (check-false run?))
   
   (check-not-exn (lambda () (begin (set! run? #t) run?)))

   (test-begin
    (check-true run?))

   ;; Reset state so tests can be run again.
   (set! run? #f)

   (test-case
    "define-test"
    (check-pred test-suite? define-test))

   (test-case
    "test-suite name must be string"
    (check-exn exn:fail:contract?
               (lambda ()
                 (test-suite (check = 1 1)))))

   (test-case
    "make-test-suite"
    (let* ([before? #f]
           [after? #f]
           [ran? #f]
           [results
            (run-test
             (make-test-suite
              "dummy1"
              (list
               (make-test-case
                "dummy-test-1"
                (lambda () (check-true #t)))
               (make-test-suite
                "dummy2"
                #:before (lambda () (set! before? #t))
                #:after  (lambda () (set! after? #t))
                (list
                 (make-test-case
                  "dummy-test-2"
                  (lambda ()
                    (set! ran? #t)
                    (check-true #t))))))))])
      (check-equal? (length results) 2)
      (map (lambda (r) (check-pred test-success? r)) results)
      (check-true before?)
      (check-true after?)
      (check-true ran?)))
   ))



(provide test-suite-tests)
