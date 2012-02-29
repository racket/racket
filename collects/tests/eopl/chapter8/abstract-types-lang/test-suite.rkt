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
          
        (module-extra-types-are-ok-11 
          "module m interface [y : int] body [x = 3 type t = int y = 4]
         from m take y"
          int 4)

        (module-extra-types-are-ok-12
          "module m interface [opaque t y : int] 
                    body [type u = bool x = 3 type t = int y = 4]
          from m take y"
          int)

        (module-extra-types-are-ok-13
          "module m interface [transparent t = int y : int] 
                    body [type u = bool x = 3 type t = int y = 4]
          from m take y"
          int 4)


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

     ;; modules declaring types

      (modules-export-abs-type-1
        "module m1 interface [opaque t] body [type t = int]
         33"
        int 33)

      (modules-take-from-ints-0.1
        "module m1
          interface [opaque t  
             zero : t] 
          body [type t = int
             zero = 0]
         33"
        int 33)

      (modules-take-from-ints-0.1a
        "module m1
          interface [opaque t  
             zero : t] 
          body [type t = int
             zero = 0]
         from m1 take zero"
        (from m1 take t) 0)

       (modules-take-from-ints-0.1.91
        "module m1
            interface [opaque t
               zero : t]
            body [type t = int
               zero = 0
               foo = 3]
         let check = proc (x : from m1 take t) zero?(0)
         in (check  from m1 take zero)"
        bool #t)

       (modules-take-from-ints-0.1.91a
        "module m1
            interface [opaque t
               zero : t]
            body [type t = int
               zero = 0
               foo = 3]
         let check = proc (x : from m1 take t  ) zero?(0)
         in check"
        ((from m1 take t) -> bool))

       (modules-take-from-ints-0.2
        "module m1
          interface [opaque t
             zero : t
             check : (t -> bool)] 
          body [type t = int
             zero = 0
             check = proc(x : t) zero?(x)]
         (from m1 take check  from m1 take zero)"
        bool #t)

       (modules-mybool-1
        "module mybool 
          interface [opaque t
             true : t
             false : t
             and : (t -> (t -> t))
             not : (t -> t)
             to-bool : (t -> bool)]
          body [type t = int
             true = 0
             false = 1
             and = proc (x : t) proc (y : t)
                        if zero?(x) then y else false
             not = proc (x : t) if zero?(x) then false else true
             to-bool = proc (x : t) 
                            if zero?(x) then zero?(0) else zero?(1)
             ]
          (from mybool take to-bool  
           from mybool take false)
          "
         bool #f)

       (modules-mybool-1a
        "module mybool 
          interface [opaque t
             true : t
             false : t
             and : (t -> (t -> t))
             not : (t -> t)
             to-bool : (t -> bool)]
          body [type t = int
             true = 0
             false = 1
             and = proc (x : t) proc (y : t)
                        if zero?(x) then y else false
             not = proc (x : t) if zero?(x) then false else true
             to-bool = proc (x : t) 
                            if zero?(x) then zero?(0) else zero?(1)
             ]
          from mybool take to-bool"
        ((from mybool take t) -> bool))

       (modules-mybool-1b
        "module mybool 
          interface [opaque t
             true : t
             false : t
             and : (t -> (t -> t))
             not : (t -> t)
             to-bool : (t -> bool)]
          body [type t = int
             true = 0
             false = 1
             and = proc (x : t) proc (y : t)
                        if zero?(x) then y else false
             not = proc (x : t) if zero?(x) then false else true
             to-bool = proc (x : t) 
                            if zero?(x) then zero?(0) else zero?(1)
             ]
           from mybool take false
          "
         (from mybool take t) )

      (modules-take-from-ints-1
        "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t) 
             check : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             check = proc(x : t) zero?(0)]
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let check = from ints-1 take check
         in (check (s (s (p (s z)))))"
        bool #t)

      (modules-take-from-ints-1a
        "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t) 
             check : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             check = proc(x : t) zero?(0)]
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let check = from ints-1 take check
         in s"
        ((from ints-1 take t) -> (from ints-1 take t)))


      (modules-take-from-ints-1b
        "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t) 
             check : (t -> bool)]
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             check = proc(x : t) zero?(0)]
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let check = from ints-1 take check
         in check"
        ((from ints-1 take t) -> bool))


   (modules-take-from-ints-2
        "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> bool)] 
          body [type t = int
             zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             is-zero = proc (x : t) zero?(x)]
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let z? = from ints-1 take is-zero
         in if (z? (s z)) then 22 else 33"
        int 33)

   
   (modules-take-from-ints-2-bad-1
        "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> bool)] 
          body [zero = proc (x : t) x
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             is-zero = proc (x : t) zero?(x)
             ]
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let z? = from ints-1 take is-zero
         in if (z? (s z)) then 22 else 33"
        error)

   (modules-take-from-ints-3
        "module ints-1
          interface [opaque t
             zero : t
             succ : (t -> t)
             pred : (t -> t)
             is-zero : (t -> int)] 
          body [zero = 0
             succ = proc(x : t) -(x,-1)
             pred = proc(x : t) -(x,1)
             is-zero = proc (x : t) zero?(x)]
         let z = from ints-1 take zero
         in let s = from ints-1 take succ
         in let p = from ints-1 take pred
         in let z? = from ints-1 take is-zero
         in if (z? (s z)) then 22 else 33"
        error)

   (modules-check-polymorphism-1 "
     module m interface [opaque t
                 f : (t -> t)]
              body [type t = int
                 f = proc (x : t) x]
     from m take f"
     ((from m take t) -> (from m take t)))


   (modules-check-polymorphism-1a "
     module m interface [opaque t
                 f : (t -> t)]
              body [type t = int
                 f = proc (x : t) x]
     from m take f"
     ((from m take t) -> (from m take t)))

    (modules-check-polymorphism-1b "
     module m interface [opaque t
                 f : (t -> t)]
              body [type t = int
                 f = proc (x : t) -(x,1)]
     from m take f"
     ((from m take t) -> (from m take t)))

    (modules-check-shadowing-1 "
      module ints-1
       interface 
        [opaque t
          zero : t
          succ : (t -> t)
          is-zero : (t -> bool)] 
       body 
        [type t = int
         zero = 0
         succ = proc(x : t) -(x,-1)
         pred = proc(x : t) -(x,1)
         is-zero = proc (x : t) zero?(x)]
      module ints-2 
       interface 
        [zero : from ints-1 take t
         succ : (from ints-1 take t -> from ints-1 take t)
         is-zero : (from ints-1 take t -> bool)]
       body
        [zero = from ints-1 take zero
         succ = from ints-1 take succ
         is-zero = from ints-1 take is-zero]
      let s = from ints-2 take succ
      in let z? = from ints-2 take is-zero
      in let z = from ints-2 take zero
      in (z? (s z))"
     bool #f)


  (modules-check-shadowing-1.8 "
      module ints-1 
       interface 
        [opaque t  
         zero : t] 
       body 
        [type t = int 
         zero = 0]
      module ints-2 
       interface     
        [foo : from ints-1 take t]
       body 
        [foo = from ints-1 take zero]
      let v = from ints-2 take foo
      in 33
     " int 33)

   (modules-check-shadowing-1.8a
     "module ints-1 
       interface [opaque t  zero : t] 
       body [type t = int zero = 0]
      module ints-2 
       interface [ foo : from ints-1 take t]
       body 
        [foo = from ints-1 take zero]
      from ints-2 take foo
     " 
     (from ints-1 take t))

   ;; this test is bogus, because duplicate module names are not
   ;; allowed.  

;;    (modules-check-shadowing-1.9.1 
;;      "module ints-1 interface [opaque t  zero : t] 
;;                   body [type t = int zero = 0]
;;       module ints-1 interface [foo : from ints-1 take t]
;;                   body import ints-1
;;                    [foo = from ints-1 take zero]
;;       let v = from ints-1 take foo
;;       in 33
;;      " int)

   ;; Once exercise 8.1 (reject duplicated module names) is done, the
   ;; test should be:

;;    (modules-check-shadowing-1.9.2 
;;      "module ints-1 interface [opaque t  zero : t] 
;;                   body [type t = int zero = 0]
;;       module ints-1 interface [foo : from ints-1 take t]
;;                   body import ints-1
;;                    [foo = from ints-1 take zero]
;;       let v = from ints-1 take foo
;;       in 33
;;      " error)    ; <<<---- changed outcome.


   ;; This is bogus in yet another way.  In the following example, v
   ;; has the type of from ints-1 take foo, which is from ints-1 take
   ;; t.  But at the point where v is used, ints-1 has been rebound,
   ;; and doesn't even have a type component t.

;;    (modules-check-shadowing-1.9.2 
;;      "module ints-1 interface [opaque t  zero : t] 
;;                   body [type t = int zero = 0]
;;       module ints-1 interface [foo : from ints-1 take t]
;;                   body import ints-1
;;                    [foo = from ints-1 take zero]
;;       let v = from ints-1 take foo
;;       in v
;;      " (from ints-1 take t))


   ;; We can take advantage of this confusion to generate an unsound
   ;; program that type-checks:

;;    (modules-check-shadowing-1.9.3 "
;; module ints-1 
;;  interface [opaque t zero : t]
;;  body [type t = int zero = 0]
;; module ints-1
;;  interface [zero : from ints-1 take t
;;             opaque t
;;             f : (t -> int)]
;;  body [zero = from ints-1 take zero
;;        type t = bool
;;        f = proc (b : t) if b then 33 else 44]
;;  (from ints-1 take f
;;   from ints-1 take zero)"
;;      int)

   ;; this code allows the application of ints-1.f because its type is
   ;; (ints-1.t -> int), and zero has type ints-1.t .  But those are
   ;; two different modules both named ints-1.

   ;; In general, the solution is to rename the inner ints-1 to avoid
   ;; the conflict.  Exercise: do this. When you do this,
   ;; modules-check-shadowing-1.9.3 should give back "error".
     
   ;; Aren't you sorry you asked?

   (transparent-0
     "module m1 interface [transparent t = int
                   zero : t]
                body [type t = int
                   zero = 0]
      -(from m1 take zero,1)"
     int)

   (transparent-1 
     "module m1 
       interface [opaque t zero : t]
       body [type t = int zero = 0]
      module m2 
       interface [transparent t = from m1 take t   % don't know
                                           % what's in m1!
          one : t]
       body [type t = int
          one = 1]
     -(from m2 take one, from m1 take zero)
     "
     error)

  (transparent-2
    "module m1 
      interface 
       [transparent t = int
        zero : t]
      body 
       [type t = int
        zero = 0]

     module m2 
      interface 
       [transparent t = from m1 take t   % now known to be int.
        one : t]
      body 
        [type t = int
         one = 1]
      -(from m2 take one, from m1 take zero)
     "
     int 1)

   (modules-myints-0.1 "
        module ints-1
         interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]  
         body [type t = int
            zero = 0
            succ = proc(x : t) -(x,-2)
            pred = proc(x : t) -(x,2)
            is-zero = proc (x : t) zero?(x)
            ]
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in let is-zero = from ints-1 take is-zero
         in (succ (succ zero))"
     (from ints-1 take t)
     4)

   (modules-myints-0.20 "
        module ints-1
         interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]  
         body [zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)
            ]
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in let is-zero = from ints-1 take is-zero
         in (succ (succ zero))"
     error
     -4)


   (modules-myints-0.2a "
        module ints-1
         interface [opaque t
            zero : t
            succ : (t -> t)
            pred : (t -> t)
            is-zero : (t -> bool)]  
         body [type t = int
            zero = 0
            succ = proc(x : t) -(x,2)
            pred = proc(x : t) -(x,-2)
            is-zero = proc (x : t) zero?(x)
            ]
         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in let is-zero = from ints-1 take is-zero
         in (succ (succ zero))"
     (from ints-1 take t) -4)

    (lift-type-from-scope-0.01 "
       module m1 
        interface 
         [transparent u = int
          transparent t = int]
        body 
         [type u = int
          type t = u]
      module m2 
       interface 
        [transparent u = int
         x : from m1 take t]
       body
        [type u = int
         x = 3]

       33"
      int
      33)

    (lift-type-from-scope-0.1 "
       module m1 
        interface 
         [transparent u = int
          transparent t = u]
        body 
         [type u = int
          type t = u]
       module m2 
        interface 
         [transparent u = int
          x : from m1 take t]
        body
         [type u = int
          x = 3]

       33"
      int
      33)

    (lift-type-from-scope-1 "
       module m1 
        interface 
         [opaque u
          transparent t = u]
        body 
         [type u = bool
          type t = u]
       module m2 
        interface 
         [transparent u = int
          x : from m1 take t]
        body
         [type u = int
          x = 3]

       33"
      error                             
      33)

    (lift-type-from-scope-2 "
       module m1 
        interface 
         [opaque t1
          f : (t1 -> t1)]
        body 
         [type t1 = bool
          f = proc (x : t1) x]

       from m1 take f"
      ((from m1 take t1) -> (from m1 take t1))
      )                                 

    (lift-type-from-scope-3 "
       module m1 
        interface 
         [opaque t2
          f : (t1 -> t1)]
        body 
         [type t1 = bool
          f = proc (x : t1) x]

       from m1 take f"
      error                             ; this should be rejected because t1
                                        ; is unbound.
      )

      (modules-14.1 "
      module m1 interface 
                 [transparent t = int
                  z : t]
                body
                  [type t = int
                   z = 0]

      module m2 
       interface
        [foo : (from m1 take t -> int)]
       body
        [foo = proc (x : from m1 take t) x]

      (from m2 take foo  33)"
      int)

      (modules-14 "
      module m1 
       interface
        [transparent t = int
         z : t]
       body
        [type t = int
         z = 0]
      module m2 
       interface
        [foo : (from m1 take t -> int)]
       body
        [foo = proc (x : from m1 take t) x]

      from m2 take foo"
        (int -> int))


      (modules-14b "
module m1 interface [transparent t1 = int] body [type t1 = int]
module m2 interface [foo : from m1 take t1] body [foo = 3]
from m2 take foo"
        int)


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
