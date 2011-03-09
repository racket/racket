#lang racket/base

(require (for-syntax racket/base)
         racket/runtime-path
         srfi/1
         srfi/13
         rackunit
         rackunit/private/util
         rackunit/private/location)

(provide test-tests)

(define successful-suite
  (test-suite
   "Example A"
   (test-case
    "Example 1"
    #t)
   (test-case
    "Example 2"
    #t)
   (test-case
    "Example 3"
    #t)))

(define-check (check-test-results test successes failures errors)
  (let ((results (run-test test)))
    (check = (length results) (+ successes failures errors))
    (check =
           (length (filter test-success? results))
           successes
           "Successes not the expected number")
    (check =
           (length (filter test-failure? results))
           failures
           "Failures not the expected number")
    (check =
           (length (filter test-error? results))
           errors
           "Errors not the expected number")))

(define-check (check-syntax-error msg sexp)
  (let ((destns (make-base-namespace))
        (cns (current-namespace)))
    (parameterize ((current-namespace destns))
      (namespace-require 'rackunit)
      (check-exn (lambda (e)
                   (check-pred exn:fail:syntax? e)
                   (check string-contains (exn-message e) msg))
                 (lambda ()
                   (eval sexp))))))

(define test-tests
  (test-suite
   "Test tests"
   (test-case "Empty test" #t)

   (test-case 
    "After action is executed"
    (let ((foo 1))
      (after (check = foo 1) (set! foo 2))
      (check = foo 2)))

   (test-case
    "Before action is executed"
    (let ((foo 1))
      (before (set! foo 2) (check = foo 2))
      (check = foo 2)))

   (test-case
    "After action is executed in presence of exception"
    (let ((foo 1))
      (check-exn exn?
                 (lambda ()
                   (after (error "quit") (set! foo 2))))
      (check = foo 2)))

   (test-case
    "Around action is executed in presence of exception"
    (let ((foo 1))
      (check-exn exn?
                 (lambda ()
                   (around
                    (set! foo 0)
                    (check = foo 0)
                    (error "quit")
                    (set! foo 2))))
      (check = foo 2)))

   (test-case
    "Before macro catches badly formed syntax w/ helpful message"
    (check-syntax-error
     "Incorrect use of before macro.  Correct format is (before before-expr expr1 expr2 ...)"
     '(before 1))
    (check-syntax-error
     "Incorrect use of before macro.  Correct format is (before before-expr expr1 expr2 ...)"
     '(before)))

   (test-case
    "After macro catches badly formed syntax w/ helpful message"
    (check-syntax-error
     "Incorrect use of after macro.  Correct format is (after expr1 expr2 ... after-expr)"
     '(after 1))
    (check-syntax-error
     "Incorrect use of after macro.  Correct format is (after expr1 expr2 ... after-expr)"
     '(after)))

   (test-case
    "Around macro catches badly formed syntax w/ helpful message"
    (check-syntax-error
     "Incorrect use of around macro.  Correct format is (around before-expr expr1 expr2 ... after-expr)"
     '(around))
    (check-syntax-error
     "Incorrect use of around macro.  Correct format is (around before-expr expr1 expr2 ... after-expr)"
     '(around 1))
    (check-syntax-error
     "Incorrect use of around macro.  Correct format is (around before-expr expr1 expr2 ... after-expr)"
     '(around 1 2)))
     
   (test-case
    "Test around action"
    (around (with-output-to-file "test.dat"
              (lambda () (display "hello")))
            (check-true (file-exists? "test.dat"))
            (delete-file "test.dat")))

   (test-case
    "Before and after on test suite are run"
    (let ((foo 1))
      (check-equal? foo 1)
      (run-test
       (test-suite
        "Dummy suite"
        #:before (lambda () (set! foo 2))
        #:after (lambda () (set! foo 3))
        (test-case
         "Test foo"
         (check-equal? foo 2))))
      (check-equal? foo 3)))

   (test-case
    "Before on test suite is run"
    (let ((foo 1))
      (check-equal? foo 1)
      (run-test
       (test-suite
        "Dummy suite"
        #:before (lambda () (set! foo 2))
        (test-case
         "Test foo"
         (check-equal? foo 2))))
      (check-equal? foo 2)))

   (test-case
    "After on test suite is run"
    (let ((foo 1))
      (check-equal? foo 1)
      (run-test
       (test-suite
        "Dummy suite"
        #:after (lambda () (set! foo 3))
        (test-case
         "Test foo"
         (check-equal? foo 2))))
      (check-equal? foo 3)))
     
   (test-case
    "Test simple foldts-test-suite"
    (check-equal?
     '(S (C C C))
     (foldts-test-suite
      (lambda (suite name before after seed)
        seed)
      (lambda (suite name before after seed kid-seed)
        (list 'S kid-seed))
      (lambda (case name action seed)
        (cons 'C seed))
      (list)
      successful-suite)))

   (test-case
    "Test fold-test-results"
    (andmap
     (lambda (result)
       (check-pred test-success? result))
     (fold-test-results
      (lambda (result seed)
        (cons result null))
      null
      successful-suite
      #:fdown (lambda (name seed) (check-equal? name "Example A") seed))))

   (test-case
    "Test run-test"
    (let ((result (run-test successful-suite)))
      (check = (length result) 3)
      (check-true (test-success? (car result)))
      (check-true (test-success? (cadr result)))
      (check-true (test-success? (caddr result)))))

   (test-case
    "Shortcuts work as expected"

    (delay-test
     (check-test-results (test-check "dummy" = 1 1) 1 0 0)
     (check-test-results (test-check "dummy" string=? "foo" "bar") 0 1 0)
     (check-test-results (test-check "dummy" string=? 'a 'b) 0 0 1)
      
     (check-test-results (test-pred "dummy" number? 1) 1 0 0)
     (check-test-results (test-pred "dummy" number? #t) 0 1 0)
     (check-test-results (test-pred "dummy" number? (error 'a)) 0 0 1)
     (check-test-results (test-equal? "dummy" 1 1) 1 0 0)
     (check-test-results (test-equal? "dummy" 1 2) 0 1 0)
     (check-test-results (test-equal? "dummy" (error 'a) 2) 0 0 1)

     (check-test-results (test-eq? "dummy" 'a 'a) 1 0 0)
     (check-test-results (test-eq? "dummy" 'a 'b) 0 1 0)
     (check-test-results (test-eq? "dummy" (error 'a) 'a) 0 0 1)

     (check-test-results (test-eqv? "dummy" 'a 'a) 1 0 0)
     (check-test-results (test-eqv? "dummy" 'a 'b) 0 1 0)
     (check-test-results (test-eqv? "dummy" (error 'a) 'a) 0 0 1)

     (check-test-results (test-= "dummy" 1.0 1.0 0.001) 1 0 0)
     (check-test-results (test-= "dummy" '1.0 1.0 0.0) 0 1 0)
     (check-test-results (test-= "dummy" (error 'a) 'a 0.01) 0 0 1)

     (check-test-results (test-true "dummy" #t) 1 0 0)
     (check-test-results (test-true "dummy" #f) 0 1 0)
     (check-test-results (test-true "dummy" (error 'a)) 0 0 1)

     (check-test-results (test-false "dummy" #f) 1 0 0)
     (check-test-results (test-false "dummy" #t) 0 1 0)
     (check-test-results (test-false "dummy" (error 'a)) 0 0 1)

     (check-test-results (test-not-false "dummy" 1) 1 0 0)
     (check-test-results (test-not-false "dummy" #f) 0 1 0)
     (check-test-results (test-not-false "dummy" (error 'a)) 0 0 1)
      
     (check-test-results
      (test-exn "dummy" exn? (lambda () (error 'a))) 1 0 0)
     (check-test-results
      (test-exn "dummy" exn? (lambda () 1)) 0 1 0)
     (check-test-results
      (test-exn "dummy" (lambda (exn) (error 'a)) (lambda () (error 'a))) 0 0 1)

     (check-test-results
      (test-not-exn "dummy" (lambda () 2)) 1 0 0)
     (check-test-results
      (test-not-exn "dummy" (lambda () (error 'a))) 0 1 0)))

   (test-case
    "test-case captures location"
    (let ([failure
           (car
            (run-test
             (delay-test (test-case "dummy" (check-equal? 1 2)))))])
      (check-pred test-failure? failure)
      (let* ([stack (exn:test:check-stack
                     (test-failure-result failure))]
             [loc (check-info-value
                   (car (filter check-location? stack)))])
        (check-regexp-match #rx"test-test\\.rkt" (location->string loc)))))
     
   (test-case
    "Shortcuts capture location"
    (let ((failure
           (car
            (run-test
             (delay-test (test-equal? "dummy" 1 2))))))
      (check-pred test-failure? failure)
      (let* ((stack (exn:test:check-stack
                     (test-failure-result failure)))
             (loc (check-info-value
                   (car (filter check-location? stack)))))
        (check-regexp-match #rx"test-test\\.rkt" (location->string loc)))))
            
   (test-case
    "All names that should be exported are exported"
    check-info?
    check-info-name
    check-info-value)

   (test-case
    "make-test-case constructs a test case"
    (check-pred
     test-success?
     (car
      (run-test
       (make-test-case "dummy" (lambda () (check-true #t)))))))
   ))
