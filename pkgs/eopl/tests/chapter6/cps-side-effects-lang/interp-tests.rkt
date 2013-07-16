#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "interp.rkt")           ; for value-of-program
(require "cps-out-lang.rkt")     ; for cps-program->string, cps-out-scan&parse 

(require (only-in racket pretty-print))

(define instrument-cps (make-parameter #f))  

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (cps-out-scan&parse string))))

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



;; this consists entirely of expressions that are already in cps.

;; exercise: for each expression that is marked "not in cps",
;; explain why it is not cps.

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-run  
 ;; simple arithmetic
 (positive-const "11" 11)
 (negative-const "-33" -33)
 (simple-arith-1 "-(44,33)" 11)
 
 
 ;; nested arithmetic
 (nested-arith-left "-(-(44,33),22)" -11)
 (nested-arith-right "-(55, -(22,11))" 44)
 
 (cps-nested-arith-left "let x = -(44,33) in -(x,22)" -11)
 (cps-nested-arith-right "let y = -(22,11) in -(55, y)" 44)
 
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
 (if-eval-test-true "let x = -(11,11) in if zero?(x) then 3 else 4" 3)
 (if-eval-test-false "let x = -(11,12)in if zero?(x) then 3 else 4" 4)
 
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
 
 ;; (nested-procs-not-in-cps "((proc (x) proc (y) -(x,y)  5) 6)" -1)
 
 (nested-procs-in-tf "(proc (x y) -(x,y)  5 6)" -1)
 
 (nested-procs2 "let f = proc(x y) -(x,y) in (f -(10,5) 6)"
                -1)
 
 ;;       (y-combinator-1-not-in-tf "
 ;; let fix =  proc (f)
 ;;             let d = proc (x) proc (z) ((f (x x)) z)
 ;;             in proc (n) ((f (d d)) n)
 ;; in let
 ;;     t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
 ;; in let times4 = (fix t4m)
 ;;    in (times4 3)" 12)
 
 
 ;;       ;; this one is not in cps
 ;;     (twice "
 ;;         (proc (twice)
 ;;            ((twice proc (z) -(z,1)) 11)
 ;;          proc (f) proc (x) (f (f x)))"
 ;;       9)
 
 (twice-in-cps "
      let twice = proc(f x k)
                    (f x  proc (z) (f z k))
      in (twice 
          proc (x k) (k -(x,1))
          11
          proc(z) z)"
               9)
 
 (cps-both-simple "
       let f = proc (x) -(x,1)
       in (f 27)"
                  26)
 
 (sum-test-1 "+()" 0)
 (sum-test-2 "+(2,3,4)" 9)
 
 (letrec-test-1 "letrec f(x) = 17 in 34" 34)
 
 (letrec-test-2 "letrec f(x y) = -(x,y) in -(34, 2)" 32)
 
 (letrec-test-3 "
       letrec even(x) = if zero?(x) then zero?(0) else (odd -(x,1))
              odd (x) = if zero?(x) then zero?(1) else (even -(x,1))
       in (even 5)"
                #f)
 
 ;; not in cps
 ;;     (cps-simple-rator "
 ;;        let f = proc (x) -(x,1)
 ;;        in (f (f 27))"
 ;;       25)
 
 ;;     (cps-simple-rand "
 ;;        let f = proc (x) proc (y) -(x,y)
 ;;        in ((f 27) 4)"
 ;;       23)
 
 ;;     (cps-neither-simple "
 ;;       let f = proc (x) proc (y) -(x, y)
 ;;       in let g = proc (z) -(z, 1)
 ;;       in ((f 27) (g 11))"
 ;;       17)
 
 ;;     (cps-serious-zero-test "
 ;;       let f = proc (x) -(x, 1)
 ;;       in if zero?((f 1)) then 11 else 22"
 ;;       11)
 
 (print-test-1
  "let x = 3 in printk(-(x,1)); 33"
  33)
 
 (store-test-0
  "newrefk(33, proc (loc1) 44)"
  44)
 
 (store-test-1
  "newrefk(33, proc (loc1)
                    newrefk(44, proc (loc2)
                                 derefk(loc1, proc(ans)ans)))"
  33)
 
 (store-test-2 "
      newrefk(33, proc (loc1)
                   newrefk(44, proc (loc2)
                                setrefk(loc1, 22);
                                derefk(loc1, proc(ans)ans)))"
               22)
 
 (store-test-2a "
      newrefk(33, proc (loc1)
                   newrefk(44, proc (loc2)
                                setrefk(loc1, 22);
                                derefk(loc1, proc (ans) -(ans,1))))"
                21)
 
 (store-test-3  "
      newrefk(33, proc (loc1)
                   newrefk(44, proc (loc2)
                                setrefk(loc2, 22);
                                derefk(loc1, proc(ans)ans)))"
                33)
 
 (gensym-cps "
       newrefk(0,
        proc(ctr)
         let g = proc(k) derefk(ctr,
                                proc(v) setrefk(ctr, -(v,-1)); (k v))
         in (g 
            proc (x) (g 
                      proc (y) -(x,y))))"
             -1)
 
 ;; in the example above, ctr is public.  Here it is local.
 (gensym-cps-2 "
      let makeg = proc (k1)
                   newrefk(0, proc (ctr)
                    (k1  proc (k) 
                          derefk(ctr, 
                                 proc (v) 
                                  setrefk(ctr,-(v,-1));(k v))))
      in (makeg
          proc(g)
           (g 
            proc (x) (g 
                      proc (y) -(x,y))))"
               -1)
 
 )
