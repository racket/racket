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
      ((list? sloppy-val) (list-val (map sloppy->expval sloppy-val)))
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
 
 ;;       (y-combinator-1 "
 ;; let fix =  proc (f)
 ;;             let d = proc (x) proc (z) ((f (x x)) z)
 ;;             in proc (n) ((f (d d)) n)
 ;; in let
 ;;     t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
 ;; in let times4 = (fix t4m)
 ;;    in (times4 3)" 12)
 
 
 ;; make sure that the test and both arms get evaluated
 ;; properly. 
 (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" 3)
 (if-eval-test-false "if zero?(-(11, 12)) then 3 else 4" 4)
 
 ;; and make sure the other arm doesn't get evaluated.
 (if-eval-test-true-2 "if zero?(-(11, 11)) then 3 else foo" 3)
 (if-eval-test-false-2 "if zero?(-(11,12)) then foo else 4" 4)
 
 
 (twice "
        (proc (twice)
           ((twice proc (z) -(z,1)) 11)
         proc (f) proc (x) (f (f x)))"
        9)
 
 ;; simple letrecs
 (simple-letrec-1 "letrec f(x) = -(x,1) in (f 33)" 32)
 (simple-letrec-2
  "letrec f(x) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
  8)
 
 (simple-letrec-3
  "let m = -5 
 in letrec f(x) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
  20)
 
 ;      (fact-of-6  "letrec
 ;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
 ;in (fact 6)" 
 ;                  720)
 
 (HO-nested-letrecs
  "letrec even(odd)  = proc(x) if zero?(x) then 1 else (odd -(x,1))
   in letrec  odd(x)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)
 
 (lists-1
  "list(2, 3, 4)"
  (2 3 4))
 
 (car-1
  "car(list(2,3,4))"
  2)
 
 (cdr-1
  "cdr(list(2,3,4))"
  (3 4))
 
 
 ;; tests for try/catch
 (simple-succeed
  "try 33 
       catch (m) 44"
  33)
 
 (dont-run-handler-til-failure
  "try 33 
       catch (m) foo"
  33)
 
 (simple-failure
  "try -(1, raise 44) catch (m) m"
  44)
 
 (uncaught-exception
  "-(22, raise 13)"
  error)
 
 (exceptions-have-dynamic-scope-1 
  "let f = proc (x) -(x, -(raise 99, 1))   % no handler in lexical scope!
       in try (f 33) 
          catch (m) 44"
  44)
 
 (handler-in-non-tail-recursive-position 
  "let f = proc (x) -(x, -(raise 99, 1))   % no handler in lexical scope!
       in -(try (f 33) 
            catch (m) -(m,55), 
            1)"
  43)
 
 (propagate-error-1
  "try try -(raise 23, 11)
           catch (m) -(raise 22,1) 
       catch (m) m"
  22)
 
 (propagate-error-2
  "let f = proc (x) -(1, raise 99)
       in
          try
             try (f 44)
             catch (exc) (f 23)
          catch (exc) 11"
  
  11)
 
 (text-example-0.1
  "let index 
            = proc (n)
               letrec inner2 (lst)
                 % find position of n in lst else raise exception 
                  = if null?(lst) then lst       
                    else if zero?(-(car(lst),n)) then lst
                    else let v = (inner2 cdr(lst))
                         in v
               in proc (lst)
                   try (inner2 lst)
                   catch (x) -1
       in ((index 3) list(2, 3, 4))"
  (3 4))
 
 (text-example-0.2
  "let index 
            = proc (n)
               letrec inner2 (lst)
                 % find position of n in lst else raise exception 
                  = if null?(lst) then lst       
                    else if zero?(-(car(lst),n)) then lst
                    else let v = (inner2 cdr(lst))
                         in v
               in proc (lst)
                   try (inner2 lst)
                   catch (x) -1
       in ((index 3) list(2, 3, 4))"
  (3 4))
 
 (text-example-1.1
  "let index 
            = proc (n)
               letrec inner2 (lst)
                 % find position of n in lst else raise error
                 % exception 
                  = if null?(lst) then raise 99       
                    else if zero?(-(car(lst),n)) then 0
                    else let v = (inner2 cdr(lst))
                         in -(v,-1)
               in proc (lst)
                   try (inner2 lst)
                   catch (x) -1
       in ((index 2) list(2, 3, 4))"
  0)
 
 (text-example-1.2
  "let index 
            = proc (n)
               letrec inner2 (lst)
                 % find position of n in lst else raise error
                 % exception 
                  = if null?(lst) then raise 99       
                    else if zero?(-(car(lst),n)) then 0
                    else -((inner2 cdr(lst)), -1)
               in proc (lst)
                   try (inner2 lst)
                   catch (x) -1
       in ((index 5) list(2, 3))"
  -1)
 
 )

