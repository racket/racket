#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")       ; for expval constructors
(require "lang.rkt")                  ; for scan&parse
(require "inferrer.rkt")              ; for type-of-program
(require "interp.rkt")                ; for value-of-program

(require "equal-up-to-gensyms.rkt")   ; for equal-up-to-gensyms

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; tcheck : string -> external-type
(define tcheck
  (lambda (string)
    (type-to-external-form
     (type-of-program (scan&parse string)))))

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

(define-syntax-rule (check-type (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (tcheck str)))]
          [else
           (check equal-types? (tcheck str) 'res (symbol->string 'name))])
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
 (apply-proc-in-rator-pos "(proc(x : int) -(x,1)  30)" 29)
 (interp-ignores-type-info-in-proc "(proc(x : (int -> int)) -(x,1)  30)" 29)
 (apply-simple-proc "let f = proc (x : int) -(x,1) in (f 30)" 29)
 (let-to-proc-1 "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" 29)
 
 
 (nested-procs "((proc(x : int) proc (y : int) -(x,y)  5) 6)" -1)
 (nested-procs2 "let f = proc(x : int) proc (y : int) -(x,y) in ((f -(10,5)) 6)"
                -1)
 
 (y-combinator-1 "
let fix =  proc(f : bool)
            let d = proc (x : bool) proc (z : bool) ((f (x x)) z)
            in proc (n : bool) ((f (d d)) n)
in let
    t4m = proc (f : bool) proc(x : bool) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
 
 ;; simple letrecs
 (simple-letrec-1 "letrec int f(x : int) = -(x,1) in (f 33)" 32)
 (simple-letrec-2
  "letrec int f(x : int) = if zero?(x)  then 0 else -((f -(x,1)), -2) in (f 4)"
  8)
 
 (simple-letrec-3
  "let m = -5 
 in letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
  20)
 
 ;      (fact-of-6  "letrec
 ;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
 ;in (fact 6)" 
 ;                  720)
 
 (HO-nested-letrecs
  "letrec int even(odd : (int -> int))  = proc(x : int) if zero?(x) then 1 else (odd -(x,1))
   in letrec  int odd(x : int)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)
 
 )

(check-type 
 ;; tests from run-tests:
 
 ;; simple arithmetic
 (positive-const "11" int)
 (negative-const "-33" int)
 (simple-arith-1 "-(44,33)" int)
 
 ;; nested arithmetic
 (nested-arith-left "-(-(44,33),22)" int)
 (nested-arith-right "-(55, -(22,11))" int)
 
 ;; simple variables
 (test-var-1 "x" int)
 (test-var-2 "-(x,1)" int)
 (test-var-3 "-(1,x)" int)
 
 (zero-test-1 "zero?(-(3,2))" bool)
 (zero-test-2 "-(2,zero?(0))" error)
 
 ;; simple unbound variables
 (test-unbound-var-1 "foo" error)
 (test-unbound-var-2 "-(x,foo)" error)
 
 ;; simple conditionals
 (if-true "if zero?(1) then 3 else 4" int)
 (if-false "if zero?(0) then 3 else 4" int)
 
 ;; make sure that the test and both arms get evaluated
 ;; properly. 
 (if-eval-test-true "if zero?(-(11,12)) then 3 else 4" int)
 (if-eval-test-false "if zero?(-(11, 11)) then 3 else 4" int)
 (if-eval-then "if zero?(1) then -(22,1) else -(22,2)" int)
 (if-eval-else "if zero?(0) then -(22,1) else -(22,2)" int)
 
 ;; make sure types of arms agree (new for lang5-1)
 
 (if-compare-arms "if zero?(0) then 1 else zero?(1)" error)
 (if-check-test-is-boolean "if 1 then 11 else 12" error)
 
 ;; simple let
 (simple-let-1 "let x = 3 in x" int)
 
 ;; make sure the body and rhs get evaluated
 (eval-let-body "let x = 3 in -(x,1)" int)
 (eval-let-rhs "let x = -(4,1) in -(x,1)" int)
 
 ;; check nested let and shadowing
 (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" int)
 (check-shadowing-in-body "let x = 3 in let x = 4 in x" int)
 (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" int)
 
 ;; simple applications
 (apply-proc-in-rator-pos "(proc(x : int) -(x,1)  30)" int)
 (checker-doesnt-ignore-type-info-in-proc 
  "(proc(x : (int -> int)) -(x,1)  30)"
  error) 
 (apply-simple-proc "let f = proc (x : int) -(x,1) in (f 30)" int)
 (let-to-proc-1 "(proc(f : (int -> int))(f 30)  proc(x : int)-(x,1))" int)
 
 
 (nested-procs "((proc (x : int) proc (y : int) -(x,y)  5) 6)" int)
 (nested-procs2
  "let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)"
  int)
 
 ;; simple letrecs
 (simple-letrec-1 "letrec int f(x : int) = -(x,1) in (f 33)" int)
 (simple-letrec-2
  "letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), -2) in (f 4)"
  int)
 
 (simple-letrec-3
  "let m = -5 
 in letrec int f(x : int) = if zero?(x) then -((f -(x,1)), m) else 0 in (f 4)"
  int)
 
 (double-it "
letrec int double (n : int) = if zero?(n) then 0 
                                  else -( (double -(n,1)), -2)
in (double 3)"
            int)
 
 ;; tests of expressions that produce procedures
 
 (build-a-proc-typed "proc (x : int) -(x,1)" (int -> int))
 
 (build-a-proc-typed-2 "proc (x : int) zero?(-(x,1))" (int -> bool))
 
 (bind-a-proc-typed
  "let f = proc (x : int) -(x,1) in (f 4)"
  int) 
 
 (bind-a-proc-return-proc
  "let f = proc (x : int) -(x,1) in f"
  (int -> int))
 
 (type-a-ho-proc-1
  "proc(f : (int -> bool)) (f 3)"
  ((int  -> bool) -> bool))
 
 (type-a-ho-proc-2
  "proc(f : (bool -> bool)) (f 3)"
  error)
 
 (apply-a-ho-proc
  "proc (x : int) proc (f : (int -> bool)) (f x)"
  (int -> ((int -> bool) -> bool)))
 
 (apply-a-ho-proc-2
  "proc (x : int) proc (f : (int -> (int -> bool))) (f x)"
  (int -> ((int -> (int -> bool)) -> (int -> bool))) )
 
 (apply-a-ho-proc-3
  "proc (x : int) proc (f : (int -> (int -> bool))) (f zero?(x))"
  error)
 
 (apply-curried-proc
  "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
  int)
 
 (apply-a-proc-2-typed
  "(proc (x : int) -(x,1) 4)" 
  int)
 
 (apply-a-letrec "
letrec int f(x : int) = -(x,1)
in (f 40)"
                 int)
 
 (letrec-non-shadowing
  "(proc (x : int)
      letrec bool loop(x : bool) =(loop x)
       in x
     1)"
  int)
 
 
 (letrec-return-fact "
let times = proc (x : int) proc (y : int) -(x,y)    % not really times
in letrec 
     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in fact"
                     (int -> int))
 
 (letrec-apply-fact "
let times = proc (x : int) proc (y : int) -(x,y)    % not really times
in letrec 
     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in (fact 4)"
                    int)
 
 (pgm7b "
letrec 
  ? fact (x : ?) = if zero?(x) then 1 else -(x, (fact -(x,1)))
in fact"
        (int -> int))
 
 
 
 ;; multiple letrecs no longer in the language
 ;;       (pgm8b "
 ;;       letrec ? odd(x : ?)  = if zero?(x) then 0 else (even -(x,1))
 ;;              ? even(x : ?) = if zero?(x) then 1 else (odd  -(x,1))
 ;;       in odd" (int -> int))
 
 
 ;;       (pgm8ab "
 ;;       letrec ? odd(x : ?)  = if zero?(x) then 0 else (even -(x,1))
 ;;              ? even(bool x) = if zero?(x) then 1 else (odd  -(x,1))
 ;;       in (odd 13)" error)
 
 ;; circular type
 (circular-type "
      let fix =  proc (f : ?)
                  let d = proc (x : ?) proc (z : ?) (f (x x) z)
                  in proc (n : ?) (f (d d) n)
          t4m = proc (f : ?, x : ?) if zero?(x) then 0 else +(4,(f -(x,1)))
      in let times4 = (fix t4m)
         in (times4 3)"
                error)
 
 ;; multiple arguments not in the language
 ;;       (pgm11b
 ;;         "letrec ? even (odd : ?, x : ?) = if zero?(x) then 1 else (odd -(x,1))
 ;;          in letrec  ? odd(x : ?) = if zero?(x) then 0 else (even odd -(x,1))
 ;;          in (odd 13)"
 ;;         int)
 
 (pgm11b-curried
  "letrec ? even (odd : ?) = proc (x : ?) if zero?(x) then 1 else (odd -(x,1))
         in letrec  ? odd(x : ?) = if zero?(x) then 0 else ((even odd) -(x,1))
         in (odd 13)"
  int)
 
 (dont-infer-circular-type 
  "letrec ? f (x : ?) = (f f) in 33"
  error)
 
 (polymorphic-type-1 
  "letrec ? f (x : ?) = (f x) in f"
  (tvar01 -> tvar02))
 
 ;; this test should fail, because the type given is insufficiently
 ;; polymorphic.  So we use it for testing the test harness, but not for
 ;; testing the checker.
 
 ;;       (polymorphic-type-1a
 ;;         "letrec ? f (x : ?) = (f x) in f"
 ;;         (tvar01 -> tvar01))
 
 (polymorphic-type-2 
  "letrec ? f (x : ?) = (f x) in proc (n : ?) (f -(n,1))"
  (int -> tvar01))
 
 )
