#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

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

(define-syntax-rule (check-run (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run str)))]
          [else
           (check equal-answer? (run str) 'res (symbol->string 'name))])
    ...))


;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run  
 ;; simple arithmetic
 (positive-const "11" 11)
 (negative-const "-33" -33)
 (simple-arith-1 "-(44,33)" 11)
 
 ;; nested arithmetic
 (nested-arith-left "-(-(44,33),22)" -11)
 (nested-arith-right "-(55, -(22,11))" 44)
 
 ;; simple variables
 (test-var-1 "x" 10)
 (test-var-2 "-(x,1)" 9)
 (test-var-3 "-(1,x)" -9)
 
 ;; simple unbound variables
 (test-unbound-var-1 "foo" error)
 (test-unbound-var-2 "-(x,foo)" error)
 
 ;; simple conditionals
 (if-true "if zero?(0) then 3 else 4" 3)
 (if-false "if zero?(1) then 3 else 4" 4)
 
 ;; test dynamic typechecking
 (no-bool-to-diff-1 "-(zero?(0),1)" error)
 (no-bool-to-diff-2 "-(1,zero?(0))" error)
 (no-int-to-if "if 1 then 2 else 3" error)
 
 ;; make sure that the test and both arms get evaluated
 ;; properly. 
 (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
 (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
 
 ;; and make sure the other arm doesn't get evaluated.
 (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
 (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)
 
 ;; simple let
 (simple-let-1 "let x = 3 in x" 3)
 
 ;; make sure the body and rhs get evaluated
 (eval-let-body "let x = 3 in -(x,1)" 2)
 (eval-let-rhs "let x = -(4,1) in -(x,1)" 2)
 
 ;; check nested let and shadowing
 (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" -1)
 (check-shadowing-in-body "let x = 3 in let x = 4 in x" 4)
 (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" 2)
 
 ;; simple applications
 (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
 (apply-simple-proc "let f = proc (x) -(x,1) in (f 30)" 29)
 (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)
 
 
 (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
 (nested-procs2 "let f = proc(x) proc (y) -(x,y) in ((f -(10,5)) 6)"
                -1)
 
 (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
 )
