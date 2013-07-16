#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")       ; for expval constructors
(require "lang.rkt")                  ; for scan&parse
(require "checker.rkt")               ; for type-of-program
(require "interp.rkt")                ; for value-of-program

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

;; tcheck : String -> ExternalType
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

(define-syntax-rule (check-type (name str typ) ...)
  (begin
    (cond [(eqv? 'typ 'error)
           (check-exn always? (lambda () (tcheck str)))]
          [else
           (check equal? (tcheck str) 'typ (symbol->string 'name))])
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
 
 
 (nested-procs "((proc (x : int) proc (y : int) -(x,y)  5) 6)" -1)
 (nested-procs2 "let f = proc(x : int) proc (y : int) -(x,y) in ((f -(10,5)) 6)"
                -1)
 
 (y-combinator-1 "
let fix =  proc (f : bool)
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
 
 (HO-nested-letrecs
  "letrec int even(odd : (int -> int))  = proc(x : int) if zero?(x) then 1 else (odd -(x,1))
   in letrec  int odd(x : int)  = if zero?(x) then 0 else ((even odd) -(x,1))
   in (odd 13)" 1)
 
 
 ;;;;;;;;;;;;;;;; typed oop ;;;;;;;;;;;;;;;;
 
 (test-self-1 "
class c extends object 
         field int s
         method void initialize(v : int)set s = v
         method void sets(v : int)set s = v    
         method int gets()s
         method void testit()send self sets(13)
              
let o = new c (11)
       t1 = 0
       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" (11 13))
 
 (counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = +(count,1)
   method int getcount()count
   
let o1 = new counter ()
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getcount();
    send o1 countup();
    set t2 = send o1 getcount();
    list(t1,t2)
end
" (0 1))
 
 (shared-counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = +(count,1)
   method int getcount()count
   
class c1 extends object 
   field int n
   field counter counter1
   method void initialize(a_counter : counter)
    begin
     set n = 0;
     set counter1 = a_counter
    end
   method void countup()
     begin
      send counter1 countup();
      set n = +(n,1)
     end
   method listof int getstate()list(n, send counter1 getcount())
   
let counter1 = new counter()
in let o1 = new c1(counter1)
       o2 = new c1(counter1)
in begin
     send o1 countup();
     send o2 countup();
     send o2 countup();
     list( send o1 getstate(),
           send o2 getstate())
   end
" ((1 3) (2 3)))
 
 
 (inherit-1 "
class c1 extends object 
  field int ivar1
  method void initialize()set ivar1 = 1
  
class c2 extends c1 
  field int ivar2
  method void initialize() 
   begin
    super initialize(); 
    set ivar2 = 1
   end
  method void setiv1(n : int)set ivar1 = n
  method int getiv1()ivar1
  
let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end                      
" 33)
 
 (inherit-3 "
class c1 extends object 
  method int initialize()1
  method int m1()1
  
class c2 extends c1 
  method int initialize()1 
  method int m1()super m1()
  method int m2()2
  
class c3 extends c2 
  method int initialize()1
  method int m1()3
  method int m2()super m2()
  method int m3()super m1()
  
let o = new c3 ()
in list( send o m1(),
         send o m2(),
         send o m3()
        )
" (3 2 1))
 
 (chris-1 "
class aclass extends object 
  field int i
  method void initialize(x : int) set i = x
  method int m(y : int) +(i,y)
  
let o1 = new aclass(3)
in send o1 m(2)" 5)
 
 (chris-2 "
class c1 extends object 
  method int initialize() 1
  method int ma()1
  method int mb()send self ma()
  
class c2 extends c1   % just use c1's initialize
  method int ma() 2
  
let x = new c2 ()
in list(send x ma(),send x mb())
" (2 2))
 
 (for-book-1 "
class c1 extends object 
  field int i
  field int j
  method void initialize(x : int) begin set i = x; set j = -(0,x) end
  method void countup(d : int) begin set i = +(i,d); set j = -(j,d) end
  method listof int getstate()list(i,j)
  
let o1 = new c1(3)
    t1 = list(1)
    t2 = list(1)
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end" ((3 -3) (5 -5)))
 
 
 (odd-even-via-self "
class oddeven extends object 
  method int initialize()1
  method bool even(n : int)if zero?(n) then 1 else send self odd(-(n,1))
  method bool odd(n : int) if zero?(n) then 0 else send self even(-(n,1))
  
let o1 = new oddeven() in send o1 odd(13)" 1)
 
 (for-book-2 "
class c1 extends object 
  method int initialize()1
  method int m1()1
  method int m2()100
  method int m3()send self m2()
  
class c2 extends c1 
  method int initialize()1
  method int m2()2
  
let o1 = new c1()
    o2 = new c2()
in list(send o1 m1(),           % returns 1
        send o1 m2(),           % returns 100
        send o1 m3(),           % returns 100
        send o2 m1(),           % returns 1 (from c1)
        send o2 m2(),           % returns 2 (from c2)
        send o2 m3()            % returns 2 (c1's m3 calls c2's m2)
       )
" (1 100 100 1 2 2))
 
 (sum-leaves "
class tree extends object 
  method int initialize()1
  
class interior_node extends tree 
  field node left
  field node right
  method void initialize(l : node, r : node)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  
class leaf_node extends tree 
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" 12)
 
 (sum-leaves-2 "
interface tree
  method int sum (l : tree, r : tree)
  
class interior_node extends object
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum() +(send left sum(), send right sum())
  
class leaf_node extends object
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" 12)
 
 (sum-leaves-with-abstract-method "
interface tree
  method int sum()
  
class interior_node extends object 
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  
class leaf_node extends object 
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   %% need subtyping to make this ok.
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" 12)
 
 
 (equal-trees-1 "
interface tree
  method int sum()
  method bool equal(t : tree)
  
class interior_node extends object 
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method tree getleft()left
  method tree getright()right
  method int sum()+(send left sum(), send right sum())
  method bool equal(t : tree) 
    if instanceof t interior_node
     then if send left equal(send cast t interior_node getleft())
          then send right equal(send cast t interior_node getright())
          else false
     else false 
     
  
class leaf_node extends object 
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  method int getvalue()value
  method bool equal(t : tree) 
   if instanceof t leaf_node
    then zero?(-(value, send cast t leaf_node getvalue()))
    else zero?(1)
    
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
" #t)
 
 (good-instanceof-1 "
class c1 extends object 
 method int initialize () 1
class c2 extends object 
 method int initialize () 2
let p = proc (o : c1) instanceof o c2 in 11
" 11)
 
 (up-cast-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends c1 
let f = proc (o : c2) send cast o c1 get() in (f new c2())
" 2)
 
 (up-instance-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends c1 
let f = proc (o : c2) instanceof o c1 in (f new c2())
" #t)
 
 (duplicate-methods-1 "
class c1 extends object
  method int initialize() 1
class c2 extends c1
  method int m1() 1
  method int m1() 2
33" 33)
 
 (incomparable-instanceof-2 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends object 
  method int initialize () 100
    
let f = proc (o : c2) if instanceof o c1 then 1 else 2 in (f new c2())
" 2)
 
 (equal-trees-by-double-dispatch "
interface tree
  method int sum()
  method bool equal(t : tree)
  method bool equal_int(l : tree, r : tree)
  method bool equal_leaf(val : int)
  
class interior_node extends object 
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum() +(send left sum(), send right sum())
  method bool equal(t : tree) send t equal_int(left, right)
  method bool equal_int(l : tree, r : tree) 
     if send left equal(l)
     then send right equal(r)
     else zero?(1)  % false
     
  method bool equal_leaf(v : int) false
  
class leaf_node extends object 
  field int value
  field bool false
  method void initialize(v : int) begin set value = v; set
                                      false=zero?(1) end
  method int sum()value
  method bool equal(t : tree) send t equal_leaf(value)
  method bool equal_int(l : tree, r : tree) false
  method bool equal_leaf(otherval : int) zero?(-(value, otherval))
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
" #t)
 
 (goldberg-80 "
class c1 extends object 
  method int initialize () 1
  method int test () 1
  method int result1 () send self test ()
  
class c2 extends c1 
  method int test () 2
  
class c3 extends c2 
  method int result2 () send self result1 ()
  method int result3 () super test ()
  
class c4 extends c3 
  method int test () 4
  
let o3 = new c3 ()
    o4 = new c4 ()
in list(send o3 test(),
        send o4 result1 (),
        send o3 result2 (),   
        send o4 result2 (),
        send o3 result3 (),
        send o4 result3 ())
" (2 4 2 4 2 2))
 
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
 
 ;; oop tests
 ;; these should all check.
 (test-self-1 "
class c extends object 
         field int s
         method void initialize(v : int)set s = v
         method void sets(v : int)set s = v
         method int gets()s
         method void testit()send self sets(13)
              
let o = new c (11)
       t1 = 0
       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" (listof int))
 
 (counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = +(count,1)
   method int getcount()count
   
let o1 = new counter ()
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getcount();
    send o1 countup();
    set t2 = send o1 getcount();
    list(t1,t2)
end
" (listof int))
 
 (shared-counter-1 "
class counter extends object
  field int count
   method void initialize()set count = 0
   method void countup()set count = +(count,1)
   method int getcount()count
   
class c1 extends object 
   field int n
   field counter counter1
   method void initialize(a_counter : counter)
    begin
     set n = 0;
     set counter1 = a_counter
    end
   method void countup()
     begin
      send counter1 countup();
      set n = +(n,1)
     end
   method listof int getstate()list(n, send counter1 getcount())
   
let counter1 = new counter()
in let o1 = new c1(counter1)
       o2 = new c1(counter1)
in begin
     send o1 countup();
     send o2 countup();
     send o2 countup();
     list( send o1 getstate(),
           send o2 getstate())
   end
" (listof (listof int)))
 
 
 (inherit-1 "
class c1 extends object 
  field int ivar1
  method void initialize()set ivar1 = 1
  
class c2 extends c1 
  field int ivar2
  method void initialize() 
   begin
    super initialize(); 
    set ivar2 = 1
   end
  method void setiv1(n : int)set ivar1 = n
  method int getiv1()ivar1
  
let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end                      
" int)
 
 (inherit-3 "
class c1 extends object 
  method int initialize()1
  method int m1()1
  
class c2 extends c1 
  method int initialize()1 
  method int m1()super m1()
  method int m2()2
  
class c3 extends c2 
  method int initialize()1
  method int m1()3
  method int m2()super m2()
  method int m3()super m1()
  
let o = new c3 ()
in list( send o m1(),
         send o m2(),
         send o m3()
        )
" (listof int))
 
 (chris-1 "
class aclass extends object 
  field int i
  method void initialize(x : int) set i = x
  method int m(y : int) +(i,y)
  
let o1 = new aclass(3)
in send o1 m(2)" int)
 
 (chris-2 "
class c1 extends object 
  method int initialize() 1
  method int ma()1
  method int mb()send self ma()
  
class c2 extends c1   % just use c1's initialize
  method int ma() 2
  
let x = new c2 ()
in list(send x ma(),send x mb())
" (listof int))
 
 (for-book-1 "
class c1 extends object 
  field int i
  field int j
  method void initialize(x : int) begin set i = x; set j = -(0,x) end
  method void countup(d : int) begin set i = +(i,d); set j = -(j,d) end
  method listof int getstate()list(i,j)
  
let o1 = new c1(3)
    t1 = list(1)
    t2 = list(1)
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end" (listof (listof int)))
 
 (odd-even-via-self "
class oddeven extends object 
  method int initialize()1
  method int even(n : int)if zero?(n) then 1 else send self odd(-(n,1))
  method int odd(n : int) if zero?(n) then 0 else send self even(-(n,1))
  
let o1 = new oddeven() in send o1 odd(13)" int)
 
 (for-book-2 "
class c1 extends object 
  method int initialize()1
  method int m1()1
  method int m2()100
  method int m3()send self m2()
  
class c2 extends c1 
  method int initialize()1
  method int m2()2
  
let o1 = new c1()
    o2 = new c2()
in list(send o1 m1(),           % returns 1
        send o1 m2(),           % returns 100
        send o1 m3(),           % returns 100
        send o2 m1(),           % returns 1 (from c1)
        send o2 m2(),           % returns 2 (from c2)
        send o2 m3()            % returns 2 (c1's m3 calls c2's m2)
       )
" (listof int))
 
 (sum-leaves "
class tree extends object 
  method int initialize()1
  
class interior_node extends tree 
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum () +(send left sum(), send right sum())
  
class leaf_node extends tree 
  field int value
  method void initialize(v : int)set value = v
  method int sum () value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" error)
 
 (sum-leaves-1.5 "
class tree extends object 
  method int initialize()1
  method int sum () 17
  
class interior_node extends tree 
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum () +(send left sum(), send right sum())
  
class leaf_node extends tree 
  field int value
  method void initialize(v : int)set value = v
  method int sum () value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" int)
 
 
 (sum-leaves-2 "
interface tree
  method int sum (l : tree, r : tree)
  
class interior_node extends object
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum() +(send left sum(), send right sum())
  
class leaf_node extends object
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" error)
 
 (sum-leaves-with-abstract-method "
interface tree
  method int sum()
  
class interior_node extends object 
  implements tree
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum()+(send left sum(), send right sum())
  
class leaf_node extends object
  implements tree 
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   %% need subtyping to make this ok.
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
" int)
 
 
 (equal-trees-1 "
interface tree
  method int sum()
  method bool equal(t : tree)
  
class interior_node extends object 
  implements tree
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method tree getleft()left
  method tree getright()right
  method int sum()+(send left sum(), send right sum())
  method bool equal(t : tree) 
    if instanceof t interior_node
     then if send left equal(send cast t interior_node getleft())
          then send right equal(send cast t interior_node getright())
          else zero?(1)
     else zero?(1)
     
  
class leaf_node extends object 
  implements tree
  field int value
  method void initialize(v : int)set value = v
  method int sum()value
  method int getvalue()value
  method bool equal(t : tree)
   if instanceof t leaf_node
    then zero?(-(value, send cast t leaf_node getvalue()))
    else zero?(1)
    
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
" bool)
 
 (good-instanceof-1 "
class c1 extends object 
 method int initialize () 1
class c2 extends object 
 method int initialize () 2
let p = proc (o : c1) instanceof o c2 in 11
" int)
 
 (up-cast-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends c1 
let f = proc (o : c2) send cast o c1 get() in (f new c2())
" int)
 
 (up-instance-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends c1 
let f = proc (o : c2) instanceof o c1 in (f new c2())
" bool)
 
 (duplicate-methods-1 "
class c1 extends object
  method int initialize() 1
class c2 extends c1
  method int m1() 1
  method int m1() 2
33" error)
 
 (incomparable-instanceof-2 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends object 
  method int initialize () 100
    
let f = proc (o : c2) if instanceof o c1 then 1 else 2 in (f new c2())
" int)
 
 (equal-trees-by-double-dispatch "
interface tree
  method int sum()
  method bool equal(t : tree)
  method bool equal_int(l : tree, r : tree)
  method bool equal_leaf(val : int)
  
class interior_node extends object
  implements tree 
  field tree left
  field tree right
  method void initialize(l : tree, r : tree)
   begin
    set left = l; set right = r
   end
  method int sum() +(send left sum(), send right sum())
  method bool equal(t : tree) send t equal_int(left, right)
  method bool equal_int(l : tree, r : tree)
     if send left equal(l)
     then send right equal(r)
     else zero?(1)  % false
     
  method bool equal_leaf(v : int) zero?(1)
  
class leaf_node extends object 
  implements tree
  field int value
  field bool false
  method void initialize(v : int) begin set value = v; set
                                        false=zero?(1) end
  method int sum()value
  method bool equal(t : tree) send t equal_leaf(value)
  method bool equal_int(l : tree, r : tree) false
  method bool equal_leaf(otherval : int) zero?(-(value, otherval))
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),   
            new leaf_node(4)),
          new leaf_node(5))
in send o1 equal(o1)
" bool)
 
 (goldberg-80 "
class c1 extends object 
  method int initialize () 1
  method int test () 1
  method int result1 () send self test ()
  
class c2 extends c1 
  method int test () 2
  
class c3 extends c2 
  method int result2 () send self result1 ()
  method int result3 () super test ()
  
class c4 extends c3 
  method int test () 4
  
let o3 = new c3 ()
    o4 = new c4 ()
in list(send o3 test(),
        send o4 result1 (),
        send o3 result2 (),   
        send o4 result2 (),
        send o3 result3 (),
        send o4 result3 ())
"
              (listof int))
 
 (check-interface-implementation-1 "
interface i1 
 method int foo ()

class c1 extends object
 implements i1
 methid int initialize () 1
 method int bar () 27

13"
                                   error)
 
 (check-interface-implementation-2 "
interface i1 
 method int foo ()

class c1 extends object
 implements i1
 method int initialize () 1
 method bool foo () 27

13"
                                   error)
 
 ;; with exercise 9.34, this should become an error
 
 (bad-cast-1 "
class c1 extends object 
 method int initialize () 1
class c2 extends object 
 method int initialize () 2
proc (o : c1) cast o c2
"
             (c1 -> c2))
 
 (missing-initialize-method-1 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends object   % no initialize method!
let f = proc (o : c2) instanceof o c1 in (f new c2())
"
                              error)
 
 (duplicate-methods-1 "
class c1 extends object
  method int initialize() 1
class c2 extends c1
  method int m1() 1
  method int m1() 2
33"
                      error)
 
 (incomparable-instanceof-2 "
class c1 extends object 
  method int initialize ()1
  method int get()2
  
class c2 extends object 
  method int initialize () 100
    
let f = proc (o : c2) if instanceof o c1 then 1 else 2 in (f new c2())
"
                            ;; this is stupid but legal
                            ;; exercise: make this illegal (9.34)
                            int)
 
 (bad-super-1 "
class c1 extends object 
 method int initialize() 1
 
class c2 extends c1 
 method int m1() super m2()
 
class c3 extends c2 
 method int m2() 2
 
class c4 extends c3 
let o = new c4() in send o m1()
"
              error)
 
 (unsupplied-method-2 "
interface c1 
  method int m1() 
  
class c2 extends object implements c1 
  method int initialize () 0
  method int m2 ()send self m1()
  
33"
                      error)
 
 (overriding-method-changes-type-1 "
class c1 extends object 
  method int initialize () 1
  method int m1() 22
  
class c2 extends c1 
  method bool m1() zero?(0)
  
33"
                                   error)
 
 (test6-3-1 "
class c1 extends object
 method int initialize () 1 
 method int m1 () 11
 method int m2 () 12
class c2 extends c1 
 method int m1 () 21
 method int m2 () 22
 method int m3 () 23
class c3 extends c2 
 method int m4 () 34
class c4 extends c3 
 method int m2 () 42 
proc (o : c3) send o m2()
"
            (c3 -> int))
 
 ;; here c2 is bad, so the interprter runs successfully and returns
 ;; false. 
 (bad-instance-of-1 "
class c1 extends object
 method int initialize () 1

instanceof new c1() c2"
                    bool)
 
 ;; here c1 is unrelated to c2, so the interpreter runs
 ;; successfully and returns false.
 (bad-instance-of-2 "
class c1 extends object
 method int initialize () 1

interface c2

instanceof new c1() c2"
                    bool)
 
 
 )



