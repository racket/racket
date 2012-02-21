(module top (lib "eopl.ss" "eopl")
  
  ;; top level module.  Loads all required pieces.
  ;; Run the test suite with (run-all).

  (require "drscheme-init.scm")
  (require "data-structures.scm")        ; for expval constructors
  (require "cps-in-lang.scm")            ; for scan&parse
  (require "cps.scm")                    ; for cps transformer
  (require "interp.scm")                 ; for value-of-program
  (require "tests.scm")                  ; for test-list

  (require "cps-out-lang.scm")              ; for cps-program->string 
  
  (provide (all-defined))
  (provide (all-from "interp.scm"))

  (define instrument-cps (make-parameter #f))  
  
  ;;;;;;;;;;;;;;;; interface to test harness ;;;;;;;;;;;;;;;;
  
  ;; run : String -> ExpVal

  (define run
    (lambda (string)
      (let ((cpsed-pgm
             (cps-of-program (scan&parse string))))
        (if (instrument-cps) (pretty-print cpsed-pgm))
        (value-of-program cpsed-pgm))))

  ;; run-all : () -> Unspecified

  ;; runs all the tests in test-list, comparing the results with
  ;; equal-answer?  

  (define run-all
    (lambda ()
      (run-tests! run equal-answer? test-list)))
  
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
    
  ;; run-one : Symbol -> ExpVal

  ;; (run-one sym) runs the test whose name is sym
  
  (define run-one
    (lambda (test-name)
      (let ((the-test (assoc test-name test-list)))
        (cond
          ((assoc test-name test-list)
           => (lambda (test)
                (run (cadr test))))
          (else (eopl:error 'run-one "no such test: ~s" test-name))))))

  ;; (stop-after-first-error #t)
  ;; (run-all)
  
  )




