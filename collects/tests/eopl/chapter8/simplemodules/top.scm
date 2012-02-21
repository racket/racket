(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Parse all the tests with (parse-all)
  ;; Run the test suite for the interpreter with (run-all).
  ;; Run the test suite for the checker with (check-all).
  
  (require "drscheme-init.scm")
  (require "data-structures.scm")       ; for expval constructors
  (require "lang.scm")                  ; for scan&parse
  (require "check-modules.scm")         ; for type-of-program
  (require "interp.scm")                ; for value-of-program

  ;; choose one of the following test suites

  (require "test-suite.scm")        ; ordinary test suite        
  ;; (require "tests-book.scm")     ; examples from book/lecture notes     

  (provide run run-all check check-all parse-all)

  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (value-of-program (scan&parse string))))
  
  ;; run-all : () -> Unspecified
  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? tests-for-run)))
  
  (define equal-answer?
    (lambda (ans correct-ans)
      (equal? ans (sloppy->expval correct-ans))))
  
  (define sloppy->expval 
    (lambda (sloppy-val)
      (cond
        ((number? sloppy-val) (num-val sloppy-val))
        ((boolean? sloppy-val) (bool-val sloppy-val))
        (else
         (eopl:error 'sloppy->expval 
                     "Can't convert sloppy value to expval: ~s"
                     sloppy-val)))))
    
  ;; run-one : Sym -> ExpVal
  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name tests-for-run)))
        (cond
          (the-test
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))
 
  ;; (run-all)
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; check : string -> external-type

  (define check
    (lambda (string)
      (type-to-external-form
        (type-of-program (scan&parse string)))))
  
  ;; check-all : () -> unspecified
  ;; checks all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define check-all
    (lambda ()
      (run-tests! check equal? tests-for-check)))

  ;; check-one : symbol -> expval
  ;; (check-one sym) checks the test whose name is sym
  
  (define check-one
    (lambda (test-name)
      (let ((the-test (assoc test-name tests-for-check)))
        (cond
          (the-test
           => (lambda (test)
                (check (cadr test))))
          (else (eopl:error 'check-one "no such test: ~s" test-name))))))
 
  ;; (check-all)

  ;;;;;;;;;;;;;;;; parsing ;;;;;;;;;;;;;;;;
  
  ;; writing syntactically correct programs in this language can take
  ;; some effort, so we've added a test that just parses the items in
  ;; the test list.  This requires a slightly different structure.

  ;; test-item ::= (test-name program correct-ans)
  ;; test-list is a list of test-items.

  (define parse-all
    (lambda ()
      (for-each
        (lambda (test-item)
          (let ((test-name (list-ref test-item 0))
                (pgm       (list-ref test-item 1)))
            (eopl:printf "~s... " test-name)               
            (let ((outcome
                    (apply-safely scan&parse (list pgm))))
              (if (car outcome)
                (eopl:printf "passed ~%")
                (begin
                  (eopl:printf "failed ~%")
                  (if (stop-after-first-error)
                    (eopl:error test-name
                      "incorrect outcome detected"))))))) 
        tests-for-parse)))

  ;; (parse-all)
  
  )




