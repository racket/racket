;; This library is used by match.ss

;; This is the major data structure of the compiler.  It holds a
;; great deal of information.  This structure represents a
;; partially compiled match test.  This test is the basic unit of
;; compilation.  The order of these tests greatly affects the size
;; of the final compiled match expression.  it also affects the
;; amount of time it takes to compile a match expression.
;; the fields:
;; tst - an S-exp of the test such as (equal exp 5).  It can also
;;       be a name of a test that isn't meant to be compared to other
;;       tests such as 'list-ddk-pat.
;; comp - a function that takes a success-function, a fail-function and
;;        a list of let bindings
;; shape - a boolean that is true if the test tests the shape or type
;;         of the data rather than the value of the data
;; times-used - the number of clauses that use this test.  In reality
;;              the number of clauses in which this test will eliminate
;;              tests
;; used-set - a list of numbers which designate the test-lists that
;;            in which this test will eliminate tests
;; bind-exp-stx - the syntax of the actual expression that is being tested
;;                by this test ex. (syntax (car (cdr x)))
;; bind-exp - the s-exp that is being tested by this test,
;;            easily obtained by taking the syntax-object->datum
;;            of bind-exp-stx
;; bind-count - is the number of times in the bind-exp is found in the
;;              test list in which this test is a member
(define-struct test (tst
                     comp
                     shape
                     times-used
                     used-set
                     bind-exp-stx
                     bind-exp
                     bind-count
                     times-used-neg
                     used-set-neg
                     closest-shape-tst
                     equal-set)
  (make-inspector))

;;!(function make-shape-test
;;          (form (make-shape-test test exp comp) -> test-struct)
;;          (contract (s-exp syntax (((list list -> syntax) 
;;                                   (list list -> syntax) list) 
;;                    -> 
;;                    (list list -> syntax)))
;;                    -> test))
;; This function is essentially a constructor for a test struct.
;; This constructor makes a "shape" test - test that tests for type
;; rather than value.
;; Arguments:
;; test - s-exp of the test
;; exp - the syntax of the expression being tested
;; comp - the compilation function which will finish the compilation
;;        after tests have been reordered
(define (make-shape-test test exp comp)
  (make-test test comp #t 0 '() exp (syntax-object->datum exp) 1 0 '() #f '()))

;;!(function make-reg-test
;;          (form (make-shape-test test exp comp) -> test-struct)
;;          (contract (s-exp syntax (((list list -> syntax) 
;;                                   (list list -> syntax) list) 
;;                                   -> (list list -> syntax)))
;;                    -> test))
;; This function is essentially a constructor for a test struct.
;; This constructor makes a "regular" test
;; Arguments:
;; test - s-exp of the test
;; exp - the syntax of the expression being tested
;; comp - the compilation function which will finish the compilation
;;        after tests have been reordered
(define (make-reg-test test exp comp)
  (make-test test comp #f 0 '() exp (syntax-object->datum exp) 1 0 '() #f '()))

;;!(function make-act-test
;;          (form (make-shape-test test exp comp) -> test-struct)
;;          (contract (s-exp syntax (((list list -> syntax) 
;;                     (list list -> syntax) list) -> (list list -> syntax)))
;;                    -> test))
;; This function is essentially a constructor for a test struct.
;; This constructor makes an "action" test - an action test is not
;; neccessarily a test so to speak but rather an action that needs to be
;; taken in order to verify that a certain expression matches a pattern.
;; A good example of this is the binding of a pattern variable.
;; Arguments:
;; act-name -
;; exp - the syntax of the expression being tested
;; comp - the compilation function which will finish the compilation
;;        after tests have been reordered
(define (make-act act-name exp comp)
  (make-test act-name comp #f -1 '() exp (syntax-object->datum exp) 1 -1 '() #f '()))

;;!(function action-test?
;;          (form (action-test? test) -> bool)
;;          (contract  test -> bool))
;; a predicate that returns true if a test is an action test
(define (action-test? test)
  (= -1 (test-times-used test)))

;;!(function shape-test?
;;          (form (shape-test? test) -> bool)
;;          (contract  test -> bool))
;; a predicate that returns true if a test is an shape test
(define (shape-test? test)
  (test-shape test))

(define (negate-test? test)
  (test-closest-shape-tst test))
