(module test-suite mzscheme

  (provide tests-for-run tests-for-check tests-for-parse)

  (define the-test-suite
    '(
      ;; tests from run-tests:
  
;;       ;; simple arithmetic
;;       (positive-const "11" int 11)
;;       (negative-const "-33" int -33)
;;       (simple-arith-1 "-(44,33)" int 11)
  
;;       ;; nested arithmetic
;;       (nested-arith-left "-(-(44,33),22)" int -11)
;;       (nested-arith-right "-(55, -(22,11))" int 44)
  
;;       ;; simple variables
;;       (test-var-1 "x" error)
;;       (test-var-2 "-(x,1)" error)
;;       (test-var-3 "-(1,x)" error)

;;       (zero-test-1 "zero?(-(3,2))" bool #f)
;;       (zero-test-2 "-(2,zero?(0))" error)
      
;;       ;; simple unbound variables
;;       (test-unbound-var-1 "foo" error)
;;       (test-unbound-var-2 "-(x,foo)" error)
  
;;       ;; simple conditionals
;;       (if-true "if zero?(0) then 3 else 4" int 3)
;;       (if-false "if zero?(1) then 3 else 4" int 4)

;;       ;; make sure that the test and both arms get evaluated
;;       ;; properly. 
;;       (if-eval-test-true "if zero?(-(11,11)) then 3 else 4" int 3)
;;       (if-eval-test-false "if zero?(-(11,12)) then 3 else 4" int 4)
;;       (if-eval-then "if zero?(0) then -(22,1) else -(22,2)" int 21)
;;       (if-eval-else "if zero?(1) then -(22,1) else -(22,2)" int 20)
      
;;       ;; make sure types of arms agree (new for lang5-1)
      
;;       (if-compare-arms "if zero?(0) then 1 else zero?(1)" error)
;;       (if-check-test-is-boolean "if 1 then 11 else 12" error)

;;       ;; simple let
;;       (simple-let-1 "let x = 3 in x" int 3)

;;       ;; make sure the body and rhs get evaluated
;;       (eval-let-body "let x = 3 in -(x,1)" int 2)
;;       (eval-let-rhs "let x = -(4,1) in -(x,1)" int 2)

;;       ;; check nested let and shadowing
;;       (simple-nested-let "let x = 3 in let y = 4 in -(x,y)" int -1)
;;       (check-shadowing-in-body "let x = 3 in let x = 4 in x" int 4)
;;       (check-shadowing-in-rhs "let x = 3 in let x = -(x,1) in x" int 2)

      ;; simple applications
      (apply-proc-in-rator-pos "(proc(x : int) -(x,1)  30)" int 29)
      (checker-doesnt-ignore-type-info-in-proc-but-interp-does 
        "(proc(x : (int -> int)) -(x,1)  30)"
        error 29) 
      (apply-simple-proc "let f = proc (x : int) -(x,1) in (f 30)" int 29)
      (let-to-proc-1
        "(proc( f : (int -> int))(f 30)  proc(x : int)-(x,1))" int 29)

      (nested-procs "((proc (x : int) proc (y : int) -(x,y)  5) 6)" int -1)
      (nested-procs2
        "let f = proc (x : int) proc (y : int) -(x,y) in ((f -(10,5)) 3)"
        int 2)
      
      ;; simple letrecs
      (simple-letrec-1 "letrec int f(x : int) = -(x,1) in (f 33)" int 32)
      (simple-letrec-2
        "letrec int double(x : int) = if zero?(x) then 0 else -((double -(x,1)), -2) in (double 4)"
        int 8)

      (simple-letrec-3
        "let m = -5 
 in letrec int f(x : int) = if zero?(x) then 0 else -((f -(x,1)), m) in (f 4)"
        int 20)

      (double-it "
letrec int double (n : int) = if zero?(n) then 0 
                                  else -( (double -(n,1)), -2)
in (double 3)"
        int 6)

      ;; tests of expressions that produce procedures

      (build-a-proc-typed "proc (x : int) -(x,1)" (int -> int))

      (build-a-proc-typed-2 "proc (x : int) zero?(-(x,1))" (int -> bool))
      
      (bind-a-proc-typed
        "let f = proc (x : int) -(x,1) in (f 4)"
        int 3) 

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
        "proc (x : int) proc ( f : (int -> bool)) (f x)"
        (int -> ((int -> bool) -> bool)))

      (apply-a-ho-proc-2
        "proc (x : int) proc ( f : (int -> (int -> bool))) (f x)"
        (int -> ((int -> (int -> bool)) -> (int -> bool)))
        )

      (apply-a-ho-proc-3
        "proc (x : int) proc ( f : (int -> (int -> bool))) (f zero?(x))"
	error)

      (apply-curried-proc
        "((proc(x : int) proc (y : int)-(x,y)  4) 3)"
        int 1)

      (apply-a-proc-2-typed
        "(proc (x : int) -(x,1) 4)" 
        int 3)

      (apply-a-letrec "
letrec int f(x : int) = -(x,1)
in (f 40)"
        int 39)

      (letrec-non-shadowing
        "(proc (x : int)
      letrec bool loop(x : bool) =(loop x)
       in x
     1)"
        int 1)


      (letrec-return-fact "
let times = proc (x : int) proc (y : int) -(x,y)    % not really times
in letrec 
     int fact(x : int) = if zero?(x) then 1 else ((times x) (fact -(x,1)))
   in fact"
        (int -> int))

      (letrec-apply-the-fcn "
let f = proc (x : int) proc (y : int) -(x,y)   
in letrec 
     int loop(x : int) = if zero?(x) then 1 else ((f x) (loop -(x,1)))
   in (loop 4)"
        int 3)

       (modules-declare-and-ignore "
module m
 interface 
  [u : int]
 body 
  [u = 3]

33"
        int 33)

       (modules-take-one-value "
module m
 interface 
  [u : int]
 body 
  [u = 3]

from m take u"
        int 3)

       (modules-take-one-value-no-import
        "module m 
          interface 
           [u : int] 
          body 
           [u = 3]
         from m take u"
        int 3)

       (modules-take-from-parameterized-module "
module m
 interface 
  ((m1 : []) => [u : int])
 body 
  module-proc (m1 : []) [u = 3]

from m take u
"
        error error)

       (modules-check-iface-subtyping-1 "
module m 
 interface 
  [u : int]
 body 
  [u = 3 v = 4]
from m take u"
        int 3)


       ;; if the interpreter always called the typechecker, or put
       ;; only declared variables in the module, this would raise an
       ;; error.  Exercise: make this modification.

       (modules-take-one-value-but-interface-bad "
         module m interface []  body [u = 3]
         from m take u"
; this version for permissive interp
        error 3
; this version for strict interp
;         error error
        )

       (modules-take-bad-value
        "module m interface []  body [u = 3]
         from m take x"
        error error)       

        (modules-two-vals "
module m
 interface 
  [u : int 
   v : int] 
 body
  [u = 44
   v = 33]

  -(from m take u, from m take v)"
        int 11)


        (modules-two-vals-bad-interface-1
        "module m interface [u : int v : bool]  
                  body [u = 44 v = 33]
         -(from m take u, from m take v)"
        error 11)

        (modules-extra-vals-are-ok-1 "
          module m interface [x : int] body [x = 3 y = 4]
          from m take x"
          int 3)

        (module-extra-vals-are-ok-2 "
          module m interface [y : int] body [x = 3 y = 4]
          from m take y"
          int)

       (modules-two-vals-bad-interface-14
        "module m interface 
            [v : int 
             u : bool]
          body 
           [v = zero?(0) u = 33]
         -(from m take u, from m take v)"
        error)


        (modules-check-let*-1
        "module m interface      [u : int v : int]
                  body [u = 44  v = -(u,11)]
         -(from m take u, from m take v)"
        int 11)

      (modules-check-let*-2.0
        "module m1 interface [u : int] body [u = 44]
         module m2 interface [v : int] 
          body 
           [v = -(from m1 take u,11)]
         -(from m1 take u, from m2 take v)"
        int 11)

      (modules-check-let*-2.05
        "module m1 interface [u : int] body [u = 44]
         module m2 interface [v : int] body [v = -(from m1 take u,11)]
         33"
        int 33)                       ; doesn't actually import anything

      (modules-check-let*-2.1
        "module m1 interface [u : int] body [u = 44]
         module m2   
          interface [v : int] 
          body [v = -(from m1 take u,11)]
         -(from m1 take u, from m2 take v)"
        int 11)

     (modules-check-let*-2.2
        "module m2
          interface [v : int] 
          body 
           [v = -(from m1 take u,11)]
         module m1 interface [u : int] body [u = 44]
         -(from m1 take u, from m2 take v)"
        error)

    ))

  (define tests-for-run
    (let loop ((lst the-test-suite))
      (cond
        ((null? lst) '())
        ((= (length (car lst)) 4)
         ;; (printf "creating item: ~s~%" (caar lst))
         (cons
           (list
             (list-ref (car lst) 0)
             (list-ref (car lst) 1)
             (list-ref (car lst) 3))
           (loop (cdr lst))))
        (else (loop (cdr lst))))))

  (define tests-for-parse
    (let loop ((lst the-test-suite))
      (cond
        ((null? lst) '())
        (else
         ;; (printf "creating item: ~s~%" (caar lst))
         (cons
           (list
             (list-ref (car lst) 0)
             (list-ref (car lst) 1)
             #t)
           (loop (cdr lst)))))))

  ;; ok to have extra members in a test-item.
  (define tests-for-check the-test-suite)


  )
