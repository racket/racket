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
 
 ;; multiple arguments
 (nested-procs2 "let f = proc(x,y) -(x,y) in (f -(10,5) 6)"
                -1)
 
 
 (twice-cps "
      let twice = proc(f, x, k)
                    (f x  proc (z) (f z k))
      in (twice 
          proc (x, k) (k -(x,1))
          11
          proc(z) z)"
            9)
 
 (cps-neither-basic "
      let f = proc (x) proc (y) -(x, y)
          g = proc (z) -(z, 1)
      in ((f 27) (g 11))"
                    17)
 
 (create-empty-class
  "class c1 extends object  3" 3)
 
 (create-class-with-method "
class c1 extends object 
  field y 
  method gety()y 33 "
                           33)
 
 (create-object "
class c1 extends object  
 method initialize()0 
let o1 = new c1() in 11
" 11)
 
 
 (send-msg-1 "
class c1 extends object 
  field s 
  method initialize()set s = 44
  method gets()s
  method sets(v)set s = v
  
let o1 = new c1() in send o1 gets()
"
             44)
 
 (send-msg-2 "
class c1 extends object 
  field s 
  method initialize()set s = 44
  method gets()s
  method sets(v)set s = v
  
let o1 = new c1() 
    t1 = 0
    t2 = 0 
in begin
     set t1 = send o1 gets();
     send o1 sets(33);
     set t2 = send o1 gets();
     list(t1, t2)
  end
"
             (44 33))
 
 (test-self-1 "
class c extends object 
  field s
  method initialize(v)set s = v
  method sets(v)set s = v
  method gets()s
  method testit()send self sets(13)
  
let o = new c (11)
       t1 = 0
       t2 = 0
   in begin 
       set t1 = send o gets();
       send o testit();
       set t2 = send o gets();
       list(t1,t2)
      end" (11 13))
 
 ;;   (two-queues "
 ;; class queue extends object 
 ;;   field q_in
 ;;   field q_out
 ;;   field ans
 ;;   method initialize()send self reset()
 ;;   method reset() 
 ;;    begin 
 ;;     set q_in = nil();
 ;;     set q_out = nil();
 ;;     send self countup()
 ;;    end
 
 ;;    method empty?()if null?(q_in) then null?(q_out)
 ;;                                   else 0
 ;;    method enq(x)begin
 ;;                   send self countup();
 ;;                   set q_in = cons(x,q_in)
 ;;                  end
 ;;    method deq()
 ;;      letrec reverse(l) = (reverse_help l nil())
 ;;                  reverse_help(inp,out) = if null?(inp) then out
 ;;                                          else (reverse_help 
 ;;                                                  cdr(inp) cons(car(inp), out))
 ;;       in if send self empty?() then 0
 ;;                                 else begin
 ;;                                       send self countup();
 ;;                                       if null?(q_out) then
 ;;                                         begin set q_out = (reverse q_in);
 ;;                                               set q_in = nil()
 ;;                                         end
 ;;                                         else 0;
 ;;                                       set ans = car(q_out);
 ;;                                       set q_out = cdr(q_out);
 ;;                                       ans
 ;;                                      end 
 ;;       method countup()1    % stub
 ;;       method get_total()1  % stub
 
 ;; let o1 = new queue ()
 ;;     o2 = new queue ()
 ;;     t1 = 0 t2 = 0 t3 = 0
 ;;     t4 = 0 t5 = 0 t6 = 0
 ;;     tot1 = 0 tot2 = 0
 ;; in begin
 ;;        send o1 enq(11);
 ;;        send o2 enq(21);
 ;;        send o1 enq(12);
 ;;        send o2 enq(22);
 ;;        set t1 = send o1 deq();
 ;;        set t2 = send o1 deq();
 ;;        set t3 = send o2 deq();
 ;;        set t4 = send o2 deq();
 ;;        set t5 = send o1 get_total();
 ;;        set t6 = send o2 get_total();
 ;;        list(t1,t2,t3,t4,t5,t6)
 ;;   end" (11 12 21 22 1 1))
 
 ;; next one is queue with shared counter object (passed at initialization)
 
 (counter-1 "
class counter extends object 
 field count
 method initialize()set count = 0
 method countup()set count = -(count, -1)
 method getcount()count
 
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
  field count
   method initialize()set count = 0
   method countup()set count = -(count, -1)
   method getcount()count
   
class c1 extends object 
   field n
   field counter1
   method initialize(a_counter)
    begin
     set n = 0;
     set counter1 = a_counter
    end
   method countup()
     begin
      send counter1 countup();
      set n = -(n,-1)
     end
   method getstate()list(n, send counter1 getcount())
   
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
"
                   ((1 3) (2 3)))
 
 ;;   (two-queues-with-counter "
 ;; class counter extends object
 ;;   field c_count
 ;;    method initialize()set c_count = 0
 ;;    method countup()set c_count = add1(c_count)
 ;;    method getcount()c_count
 
 ;; class queue extends object
 ;;   field q_in 
 ;;    field q_out
 ;;    field ans
 ;;    field count
 ;;    method initialize(the_counter)
 ;;     begin
 ;;       set count = the_counter;  % must do this first, because reset counts.
 ;;       send self reset()
 ;;      end
 
 ;;    method reset()begin set q_in = nil();
 ;;                         set q_out = nil();
 ;;                         send self countup()
 ;;                   end
 ;;    method empty?()if null?(q_in) then null?(q_out)
 ;;                                   else 0
 ;;    method enq(x)begin
 ;;                   send self countup();
 ;;                   set q_in = cons(x,q_in)
 ;;                  end
 ;;    method deq()
 ;;      letrec reverse(l) = (reverse_help l nil())
 ;;                  reverse_help(inp,out) = if null?(inp) then out
 ;;                                          else (reverse_help 
 ;;                                                  cdr(inp) cons(car(inp), out))
 ;;       in if send self empty?() then 0
 ;;                                 else begin
 ;;                                       send self countup();
 ;;                                       if null?(q_out) then
 ;;                                         begin set q_out = (reverse q_in);
 ;;                                               set q_in = nil()
 ;;                                         end
 ;;                                         else 0;
 ;;                                       set ans = car(q_out);
 ;;                                       set q_out = cdr(q_out);
 ;;                                       ans
 ;;                                      end
 ;;       method countup()send count countup()
 ;;       method get_total()send count getcount()
 
 ;; let counter1 = new counter() in
 ;; let o1 = new queue (counter1)
 ;;     o2 = new queue (counter1)
 ;;     t1 = 0 t2 = 0 t3 = 0
 ;;     t4 = 0 t5 = 0 t6 = 0
 ;;     tot1 = 0 tot2 = 0
 ;; in begin
 ;;        send o1 enq(11);
 ;;        send o2 enq(21);
 ;;        send o1 enq(12);
 ;;        send o2 enq(22);
 ;;        set t1 = send o1 deq();
 ;;        set t2 = send o1 deq();
 ;;        set t3 = send o2 deq();
 ;;        set t4 = send o2 deq();
 ;;        set t5 = send o1 get_total();
 ;;        set t6 = send o2 get_total();
 ;;        list(t1,t2,t3,t4,t5,t6)
 ;;   end" '(11 12 21 22 10 10))
 
 
 ;; Chris's first example
 
 (chris-1 "
class aclass extends object 
  field i
  method initialize(x) set i = x
  method m(y) -(i,-(0,y))
  
let o1 = new aclass(3)
in send o1 m(2)"                        
          5)
 
 (for-book-1 "
class c1 extends object
  field i
  field j
  method initialize(x) begin set i = x; set j = -(0,x) end
  method countup(d) begin set i = -(i,-(0,d)); set j = -(j,d) end
  method getstate()list(i,j)
  
let o1 = new c1(3)
    t1 = 0
    t2 = 0
in begin
    set t1 = send o1 getstate();
    send o1 countup(2);
    set t2 = send o1 getstate();
    list(t1,t2)
   end"
             ((3 -3) (5 -5)))
 
 
 (odd-even-via-self "
class oddeven extends object 
  method initialize()1
  method even(n)if zero?(n) then 1 else send self odd (-(n,1))
  method odd(n) if zero?(n) then 0 else send self even (-(n,1))
  
let o1 = new oddeven() in send o1 odd(13)"
                    1)
 
 (inherit-1 "
class c1 extends object 
  field ivar1
  method initialize()set ivar1 = 1
  
class c2 extends c1 
  field ivar2
  method initialize() 
   begin
    super initialize(); 
    set ivar2 = 1
   end
  method setiv1(n)set ivar1 = n
  method getiv1()ivar1
  
let o = new c2 ()
    t1 = 0
in begin
       send o setiv1(33);
       send o getiv1()
   end                      
" 33)
 
 (inherit-2 "
class c1 extends object 
  field ivar1
  method initialize()set ivar1 = 1

  method setiv1(n)set ivar1 = n
  method getiv1()ivar1

  method foo()1
  method call-foo-from-superclass()send self foo()

  
class c2 extends c1 
  field ivar2
  method initialize() 
   begin super initialize(); set ivar2 = 1 end
   

  method foo()2

  method setiv2(n)set ivar2 = n
  method getiv2()ivar2

  method self-and-super-foo()
    list( send self foo(),  super foo())

  method test-self-from-super()
     super call-foo-from-superclass()

   
let o = new c2 ()
    t1 = 0 t2 = 0 t3 = 0 t4 = 0
in begin
       send o setiv1(33);
       list(
         send o getiv1(),
         send o self-and-super-foo(),
         send o call-foo-from-superclass(),
         send o test-self-from-super()
         )
      end                      
" (33 (2 1) 2 2))
 
 (inherit-3 "
class c1 extends object 
  method initialize()1
  method m1()1
  
class c2 extends c1 
  method m1()super m1()
  method m2()2
  
class c3 extends c2 
  method m1()3
  method m2()super m2()
  method m3()super m1()
  
let o = new c3 ()
in list( send o m1(),
         send o m2(),
         send o m3()
        )
" (3 2 1))
 
 (chris-2 "
class c1 extends object 
  method initialize() 1
  method ma()1
  method mb()send self ma()
  
class c2 extends c1   % just use c1's initialize
  method ma() 2
  
let x = new c2 ()
in list(send x ma(),send x mb())
"
          (2 2))
 
 
 (for-book-2 "
class c1 extends object 
  method initialize()1
  method m1()1
  method m2()100
  method m3()send self m2()
  
class c2 extends c1 
  method m2()2
  
let o1 = new c1()
    o2 = new c2()
in list(send o1 m1(),           % returns 1
        send o1 m2(),           % returns 100
        send o1 m3(),           % returns 100
        send o2 m1(),           % returns 1 (from c1)
        send o2 m2(),           % returns 2 (from c2)
        send o2 m3()            % returns 2 (c1's m3 calls c2's m2)
       )
"
             (1 100 100 1 2 2))
 
 (sum-leaves "
class tree extends object 
  method initialize()1
  
class interior_node extends tree 
  field left
  field right
  method initialize(l,r)
   begin
    set left = l; set right = r
   end
  method sum() -(send left sum(), -(0, send right sum()))
  
class leaf_node extends tree 
  field value
  method initialize(v)set value = v
  method sum()value
  
let o1 = new interior_node (
          new interior_node (
            new leaf_node(3),
            new leaf_node(4)),
          new leaf_node(5))
in send o1 sum()
"
             12)
 
 (check-shadowed-fields "
class c1 extends object 
  field x
  field y
  method initialize(v) begin set x = v; set y = 0 end
  method m1() x

class c2 extends c1 
  field x
  method initialize(v1,v2) begin set x = v2; 
                                    super initialize(v1) end
  method m2()list(x,y)

class c3 extends c2 
  field x
  method initialize(v1,v2,v3) begin set x = v3; 
                                       super initialize(v1,v2)
                                 end
  method m3()x

let o = new c3(1,2,3)
in list (send o m1(), send o m2(), send o m3())
"
                        (1 (2 0) 3))
 
 (static-super "
class c1 extends object
 method initialize () 1
 method m2() send self m3()
 method m3() 13
class c2 extends c1
 method m2() 22
 method m3() 23
 method m1() super m2()
class c3 extends c2
 method m2() 32
 method m3() 33
let o3 = new c3()
in send o3 m1()"
               33)
 
 
 (every-concept "
class a extends object
  field i
  field j
  method initialize() 1
  method setup()
    begin
      set i = 15;
      set j = 20;
      50
    end    
  method f() send self g()
  method g() -(i,-(0,j))

class b extends a
  field j
  field k
  method setup()
    begin
      set j = 100;
      set k = 200;
      super setup();
      send self h()
    end
  method g()
    list(i,j,k)
  method h() super g()

class c extends b
  method g() super h()
  method h() -(k,-(0,j))

let p = proc(o)
         let u = send o setup ()
         in list(u,
                 send o g(),
                 send o f())
in list((p new a()),
        (p new b()),
        (p new c()))
"
                ((50 35 35) (35 (15 100 200) (15 100 200)) (300 35 35))
                )
 
 (colorpoint-1 "
class point extends object
  field x
  field y
  method initialize (initx, inity)
    begin 
      set x = initx; 
      set y = inity 
    end
  method move (dx, dy) 
    begin 
      set x = -(x,-(0,dx)); 
      set y = -(y,-(0,dy)) 
    end
  method get_location () list(x,y)
class colorpoint extends point
  field color
  method set_color (c) set color = c
  method get_color () color
let p = new point(3, 4)
    cp = new colorpoint(10, 20)
in begin
     send p move(3, 4);
     send cp set_color(87);
     send cp move(10, 20);
     list(send p get_location(),    % returns (6 8)
          send cp get_location(),   % returns (20 40)
          send cp get_color())      % returns 87
   end"
               ((6 8) (20 40) 87)
               )
 
 
 (colorpoint-2 "
class point extends object
  field x
  field y
  method initialize (initx, inity)
    begin 
      set x = initx; 
      set y = inity 
    end
  method move (dx, dy) 
    begin 
      set x = +(x,dx);
      set y = +(y,dy)
    end
  method get_location () list(x,y)
class colorpoint extends point
  field color
  method set_color (c) set color = c
  method get_color () color
  method initialize (x,y,c)
   begin
     super initialize (x,y);
     set color = c
   end
let p = new point(3, 4)
    cp = new colorpoint(10, 20, 30)
in begin
     send p move(3, 4);
     send cp set_color(87);
     send cp move(10, 20);
     list(send p get_location(),    % returns (6 8)
          send cp get_location(),   % returns (20 40)
          send cp get_color())      % returns 87
   end"
               ((6 8) (20 40) 87)
               
               )
 
 
 (example-for-impl "
class c1 extends object
  field x
  field y
  method initialize () 
    begin 
      set x = 11; 
      set y = 12 
    end
  method m1 () -(x,y)
  method m2 () send self m3()
class c2 extends c1
  field y
  method initialize () 
    begin 
      super initialize(); 
      set y = 22 
    end
  method m1 (u,v) -(-(x,u), -(y,v))
  method m3 () 23
class c3 extends c2
  field x
  field z
  method initialize () 
    begin 
      super initialize(); 
      set x = 31; 
      set z = 32 
    end
  method m3 () -(x,-(y,z))
let o3 = new c3()
in send o3 m1(7,8)
"
                   -10)
 
 
 )

