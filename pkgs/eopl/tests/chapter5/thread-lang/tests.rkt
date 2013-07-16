#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")  ; for expval constructors
(require "lang.rkt")             ; for scan&parse
(require "interp.rkt")           ; for value-of-program

(define run
  (lambda (timeslice string)
    (value-of-program timeslice (scan&parse string))))

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

(define-syntax-rule (check-run timeslice (name str res) ...)
  (begin
    (cond [(eqv? 'res 'error)
           (check-exn always? (lambda () (run timeslice str)))]
          [else
           (check equal-answer? (run timeslice str) 'res (symbol->string 'name))])
    ...))


;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;
(check-run 
 5
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
 
 ;; simple applications
 (apply-proc-in-rator-pos "(proc(x) -(x,1)  30)" 29)
 (let-to-proc-1 "(proc(f)(f 30)  proc(x)-(x,1))" 29)
 
 (nested-procs "((proc (x) proc (y) -(x,y)  5) 6)" -1)
 
 ;; many more tests imported from previous test suite:
 
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
 
 ;; from implicit-refs:
 
 (y-combinator-1 "
let fix =  proc (f)
            let d = proc (x) proc (z) ((f (x x)) z)
            in proc (n) ((f (d d)) n)
in let
    t4m = proc (f) proc(x) if zero?(x) then 0 else -((f -(x,1)),-4)
in let times4 = (fix t4m)
   in (times4 3)" 12)
 
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
 
 
 (begin-test-1
   "begin 1; 2; 3 end"
   3)
 
 ;; extremely primitive testing for mutable variables
 
 (assignment-test-1 "let x = 17
                          in begin set x = 27; x end"
                    27)
 
 
 (gensym-test
  "let g = let count = 0 in proc(d) 
                        let d = set count = -(count,-1)
                        in count
in -((g 11), (g 22))"
  -1)
 
 ;; this one requires letrec2
 (even-odd-via-set "
let x = 0
in letrec even(d) = if zero?(x) then 1 
                                  else let d = set x = -(x,1)
                                       in (odd d)
              odd(d)  = if zero?(x) then 0 
                                  else let d = set x = -(x,1)
                                       in (even d)
   in let d = set x = 13 in (odd -99)" 1)
 
 (example-for-book-1 "
let f = proc (x) proc (y) 
                  begin
                   set x = -(x,-1);
                   -(x,y)
                  end
in ((f 44) 33)"
                     12)
 
 (begin-1 "begin 33 end" 33)
 
 (begin-2 "begin 33; 44 end" 44)
 
 
 
 (insanely-simple-spawn "begin spawn(proc(d) 3); 44 end" 44)
 
 ;; could we do these without lists? ans: yes, but the programs
 ;; wouldn't be so clear.
 
 (two-threads "
letrec 
  noisy (l) = if null?(l) 
              then 0 
              else begin print(car(l)); yield() ; (noisy cdr(l)) end
in
   begin
    spawn(proc (d) (noisy [1,2,3,4,5])) ;
    spawn(proc (d) (noisy [6,7,8,9,10]));
    print(100);
    33
   end
"
              33)
 
 (producer-consumer "
let buffer = 0
in let
    producer = proc (n) 
              letrec 
                waitloop(k) = if zero?(k) 
                          then set buffer = n
                          else begin
                                       print(-(k,-100));
                                       yield();
                                       (waitloop -(k,1))
                               end
              in (waitloop 5)
in let consumer = proc (d) letrec
                     busywait (k) = if zero?(buffer)
                                   then begin
                                         print(-(k,-200));
                                         yield();
                                         (busywait -(k,-1))
                                        end
                                   else buffer
                in (busywait 0)
in
 begin
  spawn(proc (d) (producer 44));
  (consumer 88)
 end
"
                    44)
 
 
 (two-non-cooperating-threads "
letrec 
  noisy (l) = if null?(l) 
              then 0 
              else begin print(car(l)); (noisy cdr(l)) end
in
   begin
    spawn(proc (d) (noisy [1,2,3,4,5])) ;
    spawn(proc (d) (noisy [6,7,8,9,10])) ;
    print(100);
    33
   end
"
                              33)
 
 (unyielding-producer-consumer "
let buffer = 0
in let
  producer = proc (n) 
              letrec 
                waitloop(k) = if zero?(k) 
                          then set buffer = n
                          else begin
                                       print(-(k,-200));
                                       (waitloop -(k,1))
                               end
              in (waitloop 5)
in let consumer = proc (d) letrec
                     busywait (k) = if zero?(buffer)
                                   then begin
                                         print(-(k,-100));
                                         (busywait -(k,-1))
                                        end
                                   else buffer
                in (busywait 0)
in
 begin
  spawn(proc (d) (producer 44));
  print(300);
  (consumer 86)
 end
"
                               44)
 
 ;;       ;; > (set! the-time-slice 50)
 ;;       ;; > (run-one 'unyielding-producer-consumer)
 ;;       ;; 200
 ;;       ;; 105
 ;;       ;; 104
 ;;       ;; 201
 ;;       ;; 202
 ;;       ;; 103
 ;;       ;; 102
 ;;       ;; 203
 ;;       ;; 204
 ;;       ;; 101
 ;;       ;; 205
 ;;       ;; 44
 ;;       ;; > (set! the-time-slice 100)
 ;;       ;; > (run-one 'unyielding-producer-consumer)
 ;;       ;; 200
 ;;       ;; 201
 ;;       ;; 202
 ;;       ;; 105
 ;;       ;; 104
 ;;       ;; 103
 ;;       ;; 102
 ;;       ;; 203
 ;;       ;; 204
 ;;       ;; 205
 ;;       ;; 206
 ;;       ;; 101
 ;;       ;; 207
 ;;       ;; 44
 ;;       ;; > 
 
 (unsafe-ctr
  "let ctr = let x = 0
             in proc (n) proc (d)
                  begin
                   print(n); 
                   print(x);
                   set x = -(x,-1);
                   print(n); 
                   print(x)
                  end
   in begin
       spawn((ctr 100));
       spawn((ctr 200));
       spawn((ctr 300));
       999
      end"
  999)
 
 ;; 3 guys trying to increment ctr, but ctr ends at 2 instead of 3 when
 ;; timeslice is 10.
 
 ;;       ;; > (set! the-time-slice 20)
 ;;       ;; > (run-one 'unsafe-ctr)
 ;;       ;; 100
 ;;       ;; 0
 ;;       ;; 100
 ;;       ;; 1
 ;;       ;; 200
 ;;       ;; 1
 ;;       ;; 300
 ;;       ;; 1
 ;;       ;; 200
 ;;       ;; 2
 ;;       ;; 300
 ;;       ;; 2
 ;;       ;; 999
 ;;       ;; > 
 
 
 
 
 (safe-ctr
  "let ctr = let x = 0 in let mut = mutex()
             in proc (n) proc (d)
                  begin
                   wait(mut);
                   print(n); 
                   print(x);
                   set x = -(x,-1);
                   print(n); 
                   print(x);
                   signal(mut)
                  end
   in begin
       spawn((ctr 100));
       spawn((ctr 200));
       spawn((ctr 300));
       999
      end"
  999)
 
 ;;       ;; > (set! the-time-slice 20)
 ;;       ;; > (run-one 'safe-ctr)
 ;;       ;; 100
 ;;       ;; 0
 ;;       ;; 100
 ;;       ;; 1
 ;;       ;; 200
 ;;       ;; 1
 ;;       ;; 200
 ;;       ;; 2
 ;;       ;; 300
 ;;       ;; 2
 ;;       ;; 300
 ;;       ;; 3
 ;;       ;; 999
 ;;       ;; > 
 
 (producer-consumer-with-mutex "
let buffer = 0 
in let   mut = mutex()  % mutex open means the buffer is non-empty
in let
  producer = proc (n) 
              letrec 
                waitloop(k) 
                 = if zero?(k) 
                   then 
                    begin
                     set buffer = n;
                     signal(mut)  % give it up
                    end
                   else 
                    begin
                        print(-(k,-200));
                        (waitloop -(k,1))
                    end
              in (waitloop 5)
in let consumer = proc (d)
                     begin
                      wait(mut);
                      buffer
                     end
in
 begin
  wait(mut);   % grab the mutex before the consumer starts
  spawn(proc (d) (producer 44));
  print(300);
  (consumer 86)
 end
"
                               44)
 
 
 )



