#lang racket/base

(require rackunit/private/base
         rackunit/private/check
         rackunit/private/test-case
         rackunit/private/test-suite
         rackunit/private/result)

(provide test-case-tests)

(define test-case-tests
  (test-suite
   "test-case-tests"

   (check-exn #rx"contract" (λ () (test-case 'foo)))

   (test-case
    "test-begin terminates when sub-expression fails"
    (let ([fail? #f])
      (delay-test
       (run-test
        (test-begin
         (check-eq? 'a 'b)
         (set! fail? #t)))
       (check-false fail?))))

   (test-case
    "test-case terminates when sub-expression fails"
    (let ([fail? #f])
      (delay-test
       (run-test
        (test-case
         "foo"
         (check-eq? 'a 'b)
         (set! fail? #t)))
       (check-false fail?))))

   (test-case
    "define allowed within test-begin"
    (check-pred
     test-success?
     (delay-test
      (car (run-test
            (test-begin
             (define yes #t)
             (check-true yes)))))))

   (test-case
    "define allowed within test-case"
    (check-pred
     test-success?
     (delay-test
      (car (run-test
            (test-case
             "dummy"
             (define yes #t)
             (check-true yes)))))))))

(module+ not-test
  (require rackunit/text-ui)
  (run-tests test-case-tests))
