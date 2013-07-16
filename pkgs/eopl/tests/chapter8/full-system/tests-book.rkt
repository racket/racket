#lang eopl
(require eopl/tests/private/utils)

(require "data-structures.rkt")       ; for expval constructors
(require "lang.rkt")                  ; for scan&parse
(require "check-modules.rkt")         ; for type-of-program
(require "interp.rkt")                ; for value-of-program

;; run : String -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define tcheck
  (lambda (string)
    (type-to-external-form
     (type-of-program (scan&parse string)))))

(define parse
  (lambda (string)
    (scan&parse string)))

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

(define-syntax check-parse/type/run
  (syntax-rules ()
    [(check-parse/type/run (name str typ res) r ...)
     (begin
       (begin (check-not-exn (lambda () (parse str)))
              (check equal-answer? (run str) 'res (symbol->string 'name))
              (cond [(eqv? 'typ 'error)
                     (check-exn always? (lambda () (tcheck str)))]
                    [else
                     (check equal? (tcheck str) 'typ (symbol->string 'name))]))
       (check-parse/type/run r ...))]
    [(check-parse/type/run (name str typ) r ...)
     (begin (check-not-exn (lambda () (parse str)))
            (cond [(eqv? 'typ 'error)
                   (check-exn always? (lambda () (tcheck str)))]
                  [else
                   (check equal? (tcheck str) 'typ (symbol->string 'name))]))]))

;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

(check-parse/type/run
 (modules-dans-simplest "
         module m1
          interface 
           [a : int
            b : int]
          body
           [a = 33
            c = -(a,1)
            b = -(c,a)]

         let a = 10
         in -(-(from m1 take a, from m1 take b), 
              a)"
                        int 24)
 
 
 (example-8.2 "
         module m1 
          interface 
           [u : bool]
          body 
           [u = 33]

         44"
              error 44)
 
 (example-8.3 "
         module m1 
          interface 
           [u : int 
            v : int]
          body 
           [u = 33]

         44"
              error)
 
 (example-8.4 "
         module m1 
          interface 
           [u : int 
            v : int] 
          body 
           [v = 33 
            u = 44]

         from m1 take u" 
              error)
 
 (example-8.5a "
         module m1 
          interface 
           [u : int] 
          body 
           [u = 44]

         module m2 
          interface
           [v : int] 
          body 
           [v = -(from m1 take u,11)]

         -(from m1 take u, from m2 take v)"
               int)
 
 (example-8.5b "
         module m2 
          interface [v : int] 
          body 
           [v = -(from m1 take u,11)]

         module m1 
          interface [u : int] 
          body [u = 44]

         -(from m1 take u, from m2 take v)"
               error)
 
 (example-8.10"
       module m1 
       interface 
        [transparent t = int 
         z : t
         s : (t -> t)
         is-z? : (t -> bool)]
       body 
        [type t = int
         z = 0
         s = proc (x : t) -(x,-1)
         is-z? = proc (x : t) zero?(x)]

      let foo = proc (z : from m1 take t) 
                 -(0, (from m1 take s 
                       z))
      in
      (foo 
       from m1 take z)"
              int -1)
 
 (example-8.14 "
         module m1 
       interface [transparent t = int
                  z : t]
       body [type t = int
                       z = 0]
      module m2 
       interface 
        [foo : (from m1 take t -> int)]
       body 
        [foo = proc (x : from m1 take t) x]

      from m2 take foo"
               (int -> int))
 
 (example-8.15 "
         module m1 
       interface 
        [opaque t
         z : t
         s : (t -> t)
         is-z? : (t -> bool)]
       body 
        [type t = int
         z = 0
         s = proc (x : t) -(x,-1)
         is-z? = proc (x : t) zero?(x)]

      let foo = proc (z : from m1 take t) 
                 (from m1 take s 
                  (from m1 take s
                   z))
      -(0, (foo 
            from m1 take z))"
               error)
 
 (example-8.15a "
      module m1 
       interface 
        [opaque t
         z : t
         s : (t -> t)
         is-z? : (t -> bool)]
       body 
        [type t = int
         z = 0
         s = proc (x : t) -(x,-1)
         is-z? = proc (x : t) zero?(x)]

      let foo = proc (z : from m1 take t) 
                 (from m1 take s
                  z)
      in (foo 
       from m1 take z)"
                (from m1 take t))
 
 (example-8.8 "
         module colors
         interface
          [opaque color
           red : color
           green : color
           is-red? : (color -> bool)
           switch-colors : (color -> color)]
         body
          [type color = int
           red = 0
           green = 1
           is-red? = proc (c : color) zero?(c)
           switch-colors = proc (c : color) 
                            if (is-red? c) then green else red]

         44"
              int)
 
 (example-8.9 "
         module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]

         let zero = from ints-1 take zero
         in let succ = from ints-1 take succ
         in (succ (succ zero))"
              (from ints-1 take t) 10)
 
 (example-8.10 "
         module ints-2
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,3)
                         pred = proc(x : t) -(x,-3)
                         is-zero = proc (x : t) zero?(x)]

         let z = from ints-2 take zero
         in let s = from ints-2 take succ
         in (s (s z))"
               (from ints-2 take t) -6)
 
 (example-8.11 "
         module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]
        let z = from ints-1 take zero
        in let s = from ints-1 take succ
        in let p = from ints-1 take pred
        in let z? = from ints-1 take is-zero
        in letrec int to-int (x : from ints-1 take t) =
                      if (z? x) then 0
                         else -((to-int (p x)), -1)
        in (to-int (s (s z)))"
               int 2)
 
 (example-8.12 "
         module ints-2
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,3)
                         pred = proc(x : t) -(x,-3)
                         is-zero = proc (x : t) zero?(x)
                         ]

         let z = from ints-2 take zero
         in let s = from ints-2 take succ
         in let p = from ints-2 take pred
         in let z? = from ints-2 take is-zero
         in letrec int to-int (x : from ints-2 take t) =
                       if (z? x) then 0
                          else -((to-int (p x)), -1)
         in (to-int (s (s z)))"
               int 2)
 
 (example-8.13 "
         module mybool 
          interface [opaque t
                     true : t
                     false : t
                     and : (t -> (t -> t))
                     not : (t -> t)
                     to-bool : (t -> bool)]
          body [type t = int
                          true = 0
                          false = 13
                          and = proc (x : t) 
                                 proc (y : t)
                                  if zero?(x) 
                                   then y 
                                   else false
                          not = proc (x : t) 
                                 if zero?(x) 
                                  then false 
                                  else true
                          to-bool = proc (x : t) zero?(x)] 

         let true = from mybool take true
         in let false = from mybool take false
         in let and = from mybool take and
         in ((and true) false)"
               (from mybool take t) 13)
 
 ;;       (exercise-8.15 "
 ;;          module tables
 ;;          interface [opaque table
 ;;                     empty : table
 ;;                     add-to-table : (int -> (int -> (table -> table)))
 ;;                     lookup-in-table : (int -> (table -> int))]
 ;;          body
 ;;           [type table = (int -> int)
 ;;            ...  % to be filled in for exercise 8.15
 ;;            ]
 
 ;;           let empty = from tables take empty
 ;;           in let add-binding = from tables take add-to-table
 ;;           in let lookup = from tables take lookup-in-table
 ;;           in let table1 = (((add-binding 3) 301)
 ;;                            (((add-binding 4) 400)
 ;;                             (((add-binding 3) 301)
 ;;                               empty)))
 ;;           in -( ((lookup 4) table1),
 ;;                 ((lookup 3) table1))"
 ;;         int 99)
 
 (exercise-8.14 "
         module mybool 
         interface [opaque t
                    true : t
                    false : t
                    and : (t -> (t -> t))
                    not : (t -> t)
                    to-bool : (t -> bool)]
         body [type t = int
                         true = 1
                         false = 0
                         and = proc (x : t) 
                                proc (y : t)
                                 if zero?(x) 
                                  then false 
                                  else y
                         not = proc (x : t) 
                                if zero?(x) 
                                 then true 
                                 else false
                         to-bool = proc (x : t) 
                                    if zero?(x) 
                                     then zero?(1) 
                                     else zero?(0)]
          44"
                int 44)
 
 (alice-bob-and-charlie "
  module Alices-point-builder
   interface
    ((database : [opaque db-type
                  opaque node-type
                  insert-node : (node-type -> (db-type -> db-type))
                  ])
     => [opaque point
         initial-point : (int -> point)])

   body
    module-proc
     (database : [opaque db-type
                  opaque node-type
                  insert-node : (node-type -> (db-type -> db-type))])

     [type point = int
      initial-point = proc (x : int) x]

  module Bobs-db-module
   interface 
    [opaque db-type
     opaque node-type
     insert-node : (node-type -> (db-type -> db-type))]
   body
    [type db-type = int
     type node-type = bool
     insert-node = proc (n : node-type) proc (d : db-type) d]

  module Alices-points
   interface
    [opaque point
     initial-point : (int -> point)]
   body
    (Alices-point-builder Bobs-db-module)

  module Davids-db-module
   interface 
    [opaque db-type
     opaque node-type
     insert-node : (node-type -> (db-type -> db-type))]
   body
   [type db-type = bool
     type node-type = int
     insert-node = proc (n : node-type) proc (d : db-type) d]

  module Charlies-points
   interface
     [opaque point
     initial-point : (int -> point)]
   body
    (Alices-point-builder Davids-db-module)

  44"
                        int 44)
 
 (example-8.15 "
        module to-int-maker 
         interface
          ((m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)]) 
            => [to-int : (from m1 take t -> int)])
         body 
          module-proc 
           (m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)])
           [to-int 
             = let z? = from m1 take is-zero
               in let p = from m1 take pred
               in letrec int to-int (x : from m1 take t) 
                 = if (z? x) 
                   then 0
                   else -((to-int (p x)), -1)
               in to-int]

        module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]
        
        module ints-1-to-int
         interface [to-int : (from ints-1 take t -> int)]
         body  
          (to-int-maker ints-1)

        let two1 = (from ints-1 take succ
                    (from ints-1 take succ
                     from ints-1 take zero))
        in (from ints-1-to-int take to-int
            two1)"
               int 2)
 
 
 (example-8.16 "
        module to-int-maker 
         interface
          ((m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)]) 
            => [to-int : (from m1 take t -> int)])
         body 
          module-proc 
           (m1 : [opaque t
                  zero : t
                  succ : (t -> t)
                  pred : (t -> t)
                  is-zero : (t -> bool)])
           [to-int 
             = let z? = from m1 take is-zero
               in let p = from m1 take pred
               in letrec int to-int (x : from m1 take t) 
                 = if (z? x) 
                   then 0
                   else -((to-int (p x)), -1)
               in to-int]

        module ints-1
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,-5)
                         pred = proc(x : t) -(x,5)
                         is-zero = proc (x : t) zero?(x)]


         module ints-2
         interface [opaque t
                    zero : t
                    succ : (t -> t)
                    pred : (t -> t)
                    is-zero : (t -> bool)]  
         body [type t = int
                         zero = 0
                         succ = proc(x : t) -(x,3)
                         pred = proc(x : t) -(x,-3)
                         is-zero = proc (x : t) zero?(x)
                         ]

        module ints-1-to-int
         interface [to-int : (from ints-1 take t -> int)]
         body 
          (to-int-maker ints-1)

        module ints-2-to-int
         interface [to-int : (from ints-2 take t -> int)]
         body 
          (to-int-maker ints-2)


        let s1 = from ints-1 take succ
        in let z1 = from ints-1 take zero
        in let to-ints-1 = from ints-1-to-int take to-int

        in let s2 = from ints-2 take succ
        in let z2 = from ints-2 take zero
        in let to-ints-2 = from ints-2-to-int take to-int

        in let two1 = (s1 (s1 z1))
        in let two2 = (s2 (s2 z2))
        in -((to-ints-1 two1), (to-ints-2 two2))"
               int 0)
 
 ;;      (exercise-8.19 "
 ;;          module sum-prod-maker 
 ;;          interface 
 ;;           ((m1 : [opaque t
 ;;                   zero : t
 ;;                   succ : (t -> t)
 ;;                   pred : (t -> t)
 ;;                   is-zero : (t -> bool)])
 ;;            => [plus : (from m1 take t 
 ;;                        -> (from m1 take t 
 ;;                            -> from m1 take t))
 ;;                times : (from m1 take t 
 ;;                         -> (from m1 take t 
 ;;                             -> from m1 take t))])
 ;;          body
 ;;           ...  % to be filled in for exer. 8.19
 
 ;;          44"
 ;;        int 44)
 
 ;;      (exercise-8.22 "
 ;;          module equality-maker 
 ;;          interface 
 ;;           ((m1 : [opaque t
 ;;                   zero : t
 ;;                   succ : (t -> t)
 ;;                   pred : (t -> t)
 ;;                   is-zero : (t -> bool)])
 ;;            => [equal : (from m1 take t 
 ;;                        -> (from m1 take t 
 ;;                            -> bool))])
 ;;          body
 ;;           ...            
 ;;          33"
 ;;        int 33)
 
 ;;      (exercise-8.19 "
 ;;          module from-int-maker 
 ;;          interface 
 ;;           ((m1 : [opaque t
 ;;                   zero : t
 ;;                   succ : (t -> t)
 ;;                   pred : (t -> t)
 ;;                   is-zero : (t -> bool)])
 ;;             => [from-int : (int -> from m1 take t)])
 ;;          body
 ;;           ...
 ;;          33"
 ;;        int 33)
 
 )

#;
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

;; ok to have extra members in a test-item.
;;(define tests-for-check the-test-suite)

;;(define tests-for-parse the-test-suite)


