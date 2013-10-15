#lang racket/base

(require rackunit
         rackunit/private/util)

(provide util-tests)

;; FIXME: Two problems
;; 1 - This is not the way to test require/expose: if this fails, it 
;;     prevents the tests from loading.
;; 2 - For whatever reason, it *does* fail when loaded via PLaneT. 
;;     Still waiting for resolution on a bug report.
(require/expose "check-test.rkt" (make-failure-test))

(define util-tests
  (test-suite
   "Util tests"
   (test-case
    "make-failure-test required from check-test.rkt"
    (begin
      (check-true (procedure? make-failure-test))
      (check-equal? (make-arity-at-least 2)
                    (procedure-arity make-failure-test))
      (check-pred rackunit-test-case?
                  (delay-test (make-failure-test "foo" string?)))))
   
   (test-case
    "Test test-suite*"
    (let ((result
           (run-test
            (test-suite*
             "Test test-suite*"
             ("Test 1" (check = 1 1))
             ("Test 2" (check = 1 1) (check = 2 4))))))
      (check = (length result) 2)
      (check-true (test-success? (car result)))
      (check-true (test-failure? (cadr result)))))
   
   (test-case
    "Simple check-regexp test"
    (check-regexp-match "a*bba"
                        "aaaaaabba"))
   
   (test-case
    "check-regexp-match failure"
    (check-exn
     exn:test:check?
     (lambda ()
       (check-regexp-match "a+bba" "aaaabbba"))))
   ))
