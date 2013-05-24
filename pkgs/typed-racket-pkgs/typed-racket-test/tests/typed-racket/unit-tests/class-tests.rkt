#lang racket

;; Unit tests for typed classes
;;
;; FIXME: make this work with the unit testing framework for
;;        typecheck eventually (it's finnicky).
;;
;; FIXME: these tests are slow

(require "test-utils.rkt"
         rackunit)

(provide tests)
(gen-test-main)

(define-syntax-rule (run/tr-module e ...)
  (parameterize ([current-output-port (open-output-nowhere)]
                 [current-error-port (open-output-nowhere)])
    (define ns (make-base-namespace))
    (eval (quote (module typed typed/racket
                   e ...))
          ns)
    (eval (quote (require 'typed)) ns)))

(define-syntax-rule (check-ok e ...)
  (begin (check-not-exn (thunk (run/tr-module e ...)))))

(define-syntax-rule (check-err e ...)
  (check-exn exn:fail:syntax? (thunk (run/tr-module e ...))))

(define tests
  (test-suite
   "Class type-checking tests"

   ;; Basic class with init and public method
   (check-ok
    (: c% (Class (init [x Integer])
                 [m (Integer -> Integer)]))
    (define c%
      (class: object%
        (super-new)
        (init x)
        (define/public (m x) 0)))
    (send (new c% [x 1]) m 5))

   ;; Fails, bad superclass expression
   (check-err
    (: d% (Class (init [x Integer])
                 [m (Integer -> Integer)]))
    (define d% (class: 5
                 (super-new)
                 (init x)
                 (define/public (m x) 0))))

   ;; Method using argument type
   (check-ok
    (: e% (Class (init [x Integer])
                 [m (Integer -> Integer)]))
    (define e% (class: object%
                 (super-new)
                 (init x)
                 (define/public (m x) x))))

   ;; Send inside a method
   (check-ok
    (: f% (Class (init [x Integer])
                 [m (Integer -> Integer)]))
    (define f% (class: object%
                 (super-new)
                 (init x)
                 (define/public (m x) (send this m 3)))))

   ;; Fails, send to missing method
   (check-err
    (: g% (Class (init [x Integer #:optional])
                 [m (Integer -> Integer)]))
    (define g% (class: object%
                 (super-new)
                 (init x)
                 (define/public (m x) (send this z)))))

   ;; Send to other methods
   (check-ok
    (: h% (Class [n (-> Integer)]
                 [m (Integer -> Integer)]))
    (define h% (class: object%
                 (super-new)
                 (define/public (n) 0)
                 (define/public (m x) (send this n)))))

   ;; Local sends
   (check-ok
    (: i% (Class [n (-> Integer)]
                 [m (Integer -> Integer)]))
    (define i% (class: object%
                 (super-new)
                 (define/public (n) 0)
                 (define/public (m x) (n)))))

   ;; Field access via get-field
   (check-ok
    (: j% (Class (field [n Integer])
                 [m (-> Integer)]))
    (define j% (class: object%
                 (super-new)
                 (field [n 0])
                 (define/public (m) (get-field n this)))))

   ;; fails, field's default value has wrong type
   (check-err
    (class: object% (super-new)
      (: x Integer)
      (field [x "foo"])))

   ;; Fail, field access to missing field
   (check-err
    (: k% (Class (field [n Integer])
                 [m (-> Integer)]))
    (define k% (class: object%
                 (super-new)
                 (define/public (m) (get-field n this)))))

   ;; Fail, conflict with parent field
   (check-err
    (: j% (Class (field [n Integer])
                 [m (-> Integer)]))
    (define j% (class: object%
                 (super-new)
                 (field [n 0])
                 (define/public (m) (get-field n this))))
    (: l% (Class (field [n Integer])))
    (define l% (class: j%
                 (field [n 17])
                 (super-new))))

   ;; Fail, conflict with parent method
   (check-err
    (: j% (Class (field [n Integer])
                 [m (-> Integer)]))
    (define j% (class: object%
                 (super-new)
                 (field [n 0])
                 (define/public (m) (get-field n this))))
    (: m% (Class [m (-> Integer)]))
    (define m% (class: j%
                 (super-new)
                 (define/public (m) 17))))

   ;; Inheritance
   (check-ok
    (: j% (Class (field [n Integer])
                 [m (-> Integer)]))
    (define j% (class: object%
                 (super-new)
                 (field [n 0])
                 (define/public (m) (get-field n this))))
    (: n% (Class (field [n Integer])
                 [m (-> Integer)]))
    (define n% (class: j% (super-new))))

   ;; should fail, too many methods
   (check-err
    (: o% (Class))
    (define o% (class: object%
                 (super-new)
                 (define/public (m) 0))))

   ;; same as previous
   (check-err
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m x) (add1 x))
                 (define/public (n) 0))))

   ;; fails, too many inits
   (check-err
    (: c% (Class (init [str String #:optional])))
    (define c% (class: object% (super-new)
                 (init str x))))

   ;; fails, too many fields
   (check-err
    (: c% (Class (field [str String])))
    (define c% (class: object% (super-new)
                 (field [str "foo"] [x 0]))))

   ;; FIXME: for the following two tests, we could improve
   ;;        things by either figuring out the init or field
   ;;        type when a default expr is provided. Otherwise,
   ;;        we should still provide a better error message.
   ;;
   ;; fails, init with no type annotation
   (check-err
     (define c% (class: object% (super-new) (init x))))

   ;; fails, field with no type annotation
   (check-err
     (define c% (class: object% (super-new) (field [x 0]))))

   ;; Mixin on classes without row polymorphism
   (check-ok
     (: mixin ((Class [m (-> Integer)])
               ->
               (Class [m (-> Integer)]
                      [n (-> String)])))
     (define (mixin cls)
       (class: cls
         (super-new)
         (define/public (n) "hi")))

     (: arg-class% (Class [m (-> Integer)]))
     (define arg-class%
       (class: object%
         (super-new)
         (define/public (m) 0)))

     (mixin arg-class%))

   ;; Fail, bad mixin
   (check-err
     (: mixin ((Class [m (-> Integer)])
               ->
               (Class [m (-> Integer)]
                      [n (-> String)])))
     (define (mixin cls)
       (class: cls
         (super-new)))

     (: arg-class% (Class [m (-> Integer)]))
     (define arg-class%
       (class: object%
         (super-new)
         (define/public (m) 0)))

     (mixin arg-class%))

   ;; Fail, bad mixin argument
   (check-err
     (: mixin ((Class [m (-> Integer)])
               ->
               (Class [m (-> Integer)]
                      [n (-> String)])))
     (define (mixin cls)
       (class: cls
         (super-new)
         (define/public (n) "hi")))

     (: arg-class% (Class [k (-> Integer)]))
     (define arg-class%
       (class: object%
         (super-new)
         (define/public (k) 0)))

     (mixin arg-class%))

   ;; classes that don't use define/public directly
   (check-ok
     (: c% (Class [m (Number -> String)]))
     (define c%
       (class: object%
         (super-new)
         (public m)
         (define-values (m)
           (lambda (x) (number->string x)))))
     (send (new c%) m 4))

   ;; check that classes work in let clauses
   (check-ok
    (let: ([c% : (Class [m (Number -> String)])
            (class: object%
              (super-new)
              (public m)
              (define-values (m)
                (lambda (x) (number->string x))))])
      (send (new c%) m 4)))

   ;; check a good super-new call
   (check-ok
    (: c% (Class (init [x Integer])))
    (define c% (class: object% (super-new) (init x)))
    (: d% (Class))
    (define d% (class: c% (super-new [x (+ 3 5)]))))

   ;; fails, missing super-new
   (check-err
    (: c% (Class (init [x Integer])))
    (define c% (class: object% (init x))))

   ;; fails, non-top-level super-new
   (check-err
    (: c% (Class (init [x Integer])))
    (define c% (class: object% (let () (super-new)) (init x))))

   ;; fails, bad super-new argument
   (check-err
    (: c% (Class (init [x Integer])))
    (define c% (class: object% (super-new) (init x)))
    (: d% (Class))
    (define d% (class: c% (super-new [x "bad"]))))

   ;; test override
   (check-ok
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y) (add1 y))))
    (: d% (Class [m (Integer -> Integer)]))
    (define d% (class: c% (super-new)
                 (define/override (m y) (* 2 y)))))

   ;; test local call to overriden method
   (check-ok
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y) (add1 y))))
    (: d% (Class [n (Integer -> Integer)]
                 [m (Integer -> Integer)]))
    (define d% (class: c% (super-new)
                 (define/public (n x) (m x))
                 (define/override (m y) (* 2 y)))))

   ;; fails, superclass missing public for override
   (check-err
    (: d% (Class [m (Integer -> Integer)]))
    (define d% (class: object% (super-new)
                 (define/override (m y) (* 2 y)))))

   ;; local field access and set!
   (check-ok
    (: c% (Class (field [x Integer])
                 [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (field [x 0])
                 (define/public (m y)
                   (begin0 x (set! x (+ x 1)))))))

   ;; fails, missing local field
   (check-err
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y)
                   (begin0 x (set! x (+ x 1)))))))

   ;; test top-level expressions in the class
   (check-ok
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y) 0)
                 (+ 3 5))))

   ;; test top-level method call
   (check-ok
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y) 0)
                 (m 3))))

   ;; test top-level field access
   (check-ok
    (: c% (Class (field [f String])))
    (define c% (class: object% (super-new)
                 (field [f "foo"])
                 (string-append f "z"))))

   ;; fails, bad top-level expression
   (check-err
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y) 0)
                 (+ "foo" 5))))

   ;; fails, ill-typed method call
   (check-err
    (: c% (Class [m (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (define/public (m y) 0)
                 (m "foo"))))

   ;; fails, ill-typed field access
   (check-err
    (: c% (Class [f String]))
    (define c% (class: object% (super-new)
                 (field [f "foo"])
                 (set! f 5))))

   ;; test private field
   (check-ok
    (class: object%
      (super-new)
      (: x Integer)
      (define x 5)
      (set! x 8)
      (+ x 1))
    (: d% (Class (field [y String])))
    (define d%
      (class: object%
        (super-new)
        (: x Integer)
        (define x 5)
        (: y String)
        (field [y "foo"]))))

   ;; fails, bad private field set!
   (check-err
    (class: object%
      (super-new)
      (: x Integer)
      (define x 5)
      (set! x "foo")))

   ;; fails, bad private field default
   (check-err
    (class: object%
      (super-new)
      (: x Integer)
      (define x "foo")))

   ;; fails, private field needs type annotation
   (check-err
    (class: object%
      (super-new)
      (define x "foo")))

   ;; test private method
   (check-ok
    (class: object% (super-new)
      (: x (-> Integer))
      (define/private (x) 3)
      (: m (-> Integer))
      (define/public (m) (x))))

   ;; fails, public and private types conflict
   (check-err
    (class: object% (super-new)
      (: x (-> Integer))
      (define/private (x) 3)
      (: m (-> String))
      (define/public (m) (x))))

   ;; fails, not enough annotation on private
   (check-err
    (class: object% (super-new)
      (define/private (x) 3)
      (: m (-> Integer))
      (define/public (m) (x))))

   ;; test optional init arg
   (check-ok
    (: c% (Class (init [x Integer #:optional])))
    (define c% (class: object% (super-new)
                 (: x Integer)
                 (init [x 0]))))

   ;; test init coverage when all optionals are
   ;; in the superclass
   (check-ok
    (: c% (Class (init [x Integer #:optional])))
    (: d% (Class (init [x Integer #:optional])))
    (define c% (class: object% (super-new)
                 (: x Integer)
                 (init [x 0])))
    (define d% (class: c% (super-new))))

   ;; fails, expected mandatory but got optional
   (check-err
    (: c% (Class (init [x Integer])))
    (define c% (class: object% (super-new)
                 (: x Integer)
                 (init [x 0]))))

   ;; fails, mandatory init not provided
   (check-err
    (define d% (class: object% (super-new)
                 (: x Integer)
                 (init x)))
    (new d%))

   ;; test that provided super-class inits don't count
   ;; towards the type of current class
   (check-ok
    (: c% (Class))
    (define c% (class: (class: object% (super-new)
                         (: x Integer)
                         (init x))
                 (super-new [x 3]))))

   ;; fails, super-class init already provided
   (check-err
    (define c% (class: (class: object% (super-new)
                         (: x Integer)
                         (init x))
                 (super-new [x 3])))
    (new c% [x 5]))

   ;; fails, super-new can only be called once per class
   (check-err
    (class: object%
      (super-new)
      (super-new)))

   ;; test passing an init arg to super-new
   (check-ok
    (define c% (class: (class: object% (super-new)
                         (: x Integer)
                         (init x))
                 (: x Integer)
                 (init x)
                 (super-new [x x])))
    (new c% [x 5]))

   ;; fails, bad argument type to super-new
   (check-err
    (define c% (class: (class: object% (super-new)
                         (: x Integer)
                         (init x))
                 (: x String)
                 (init x)
                 (super-new [x x]))))

   ;; test inherit method
   (check-ok
    (class: (class: object% (super-new)
              (: m (Integer -> Integer))
              (define/public (m x) (add1 x)))
      (super-new)
      (inherit m)
      (m 5)))

   ;; test internal name with inherit
   (check-ok
    (class: (class: object% (super-new)
              (: m (Integer -> Integer))
              (define/public (m x) (add1 x)))
      (super-new)
      (inherit [n m])
      (n 5)))

   ;; fails, missing super method for inherit
   (check-err
    (class: (class: object% (super-new)) (super-new) (inherit z)))

   ;; fails, bad argument type to inherited method
   (check-err
    (class: (class: object% (super-new)
              (: m (Integer -> Integer))
              (define/public (m x) (add1 x)))
      (super-new)
      (inherit m)
      (m "foo")))

   ;; test that keyword methods type-check
   ;; FIXME: send with keywords does not work yet
   (check-ok
    (: c% (Class [n (Integer #:foo Integer -> Integer)]))
    (define c%
      (class: object%
        (super-new)
        (define/public (n x #:foo foo)
          (+ foo x)))))

   ;; test instance subtyping
   (check-ok
    (define c%
      (class: object%
        (super-new)
        (: x (U False Number))
        (field [x 0])))
    (: x (Instance (Class)))
    (define x (new c%)))

   ;; test use of `this` in field default
   (check-ok
    (class: object%
      (super-new)
      (: x Integer)
      (field [x 0])
      (: y Integer)
      (field [y (get-field x this)])))

   ;; test super calls
   (check-ok
    (define c%
      (class: object%
        (super-new)
        (: m (Integer -> Integer))
        (define/public (m x) 0)))
    (define d%
      (class: c%
        (super-new)
        (define/override (m x) (add1 (super m 5)))))
    (send (new d%) m 1))

   ;; test super calls at top-level
   (check-ok
    (define c%
      (class: object%
        (super-new)
        (: m (Integer -> Integer))
        (define/public (m x) 0)))
    (define d%
      (class: c%
        (super-new)
        (super m 5)
        (define/override (m x) 5))))

   ;; fails, bad super call argument
   (check-err
    (define c%
      (class: object%
        (super-new)
        (: m (Integer -> Integer))
        (define/public (m x) 0)))
    (define d%
      (class: c%
        (super-new)
        (super m "foo")
        (define/override (m x) 5))))

   ;; test different internal/external names
   (check-ok
    (define c% (class: object% (super-new)
                 (public [m n])
                 (define m (lambda () 0))))
    (send (new c%) n))

   ;; test local calls with internal/external
   (check-ok
    (define c% (class: object% (super-new)
                 (: m (-> Integer))
                 (public [m n])
                 (define m (lambda () 0))
                 (: z (-> Integer))
                 (define/public (z) (m))))
    (send (new c%) z))

   ;; internal/external the same is ok
   (check-ok
    (define c% (class: object% (super-new)
                 (public [m m])
                 (define m (lambda () 0))))
    (send (new c%) m))

   ;; fails, internal name not accessible
   (check-err
    (define c% (class: object% (super-new)
                 (public [m n])
                 (define m (lambda () 0))))
    (send (new c%) m))

   ;; test internal/external with expected
   (check-ok
    (: c% (Class [n (-> Integer)]))
    (define c% (class: object% (super-new)
                 (public [m n])
                 (define m (lambda () 0))))
    (send (new c%) n))

   ;; test internal/external field
   (check-ok
    (define c% (class: object% (super-new)
                 (: f Integer)
                 (field ([f g] 0))))
    (get-field g (new c%)))

   ;; fail, internal name not accessible
   (check-err
    (define c% (class: object% (super-new)
                 (: f Integer)
                 (field ([f g] 0))))
    (get-field f (new c%)))

   ;; test internal/external init
   (check-ok
    (define c% (class: object% (super-new)
                 (: i Integer)
                 (init ([i j]))))
    (new c% [j 5]))

   ;; fails, internal name not accessible
   (check-err
    (define c% (class: object% (super-new)
                 (: i Integer)
                 (init ([i j]))))
    (new c% [i 5]))

   ;; test init default values
   (check-ok
    (class: object% (super-new)
      (: z Integer)
      (init [z 0])))

   ;; fails, bad default init value
   (check-err
    (class: object% (super-new)
      (: z Integer)
      (init [z "foo"])))

   ;; test init field default value
   (check-ok
    (define c% (class: object% (super-new)
                 (: x Integer)
                 (init-field ([x y] 0)))))

   ;; fails, wrong init-field default
   (check-err
    (define c% (class: object% (super-new)
                 (: x Integer)
                 (init-field ([x y] "foo")))))

   ;; test type-checking method with internal/external
   (check-err
    (: c% (Class [n (Integer -> Integer)]))
    (define c% (class: object% (super-new)
                 (public [m n])
                 (define m (lambda () 0)))))

   ;; test type-checking without expected class type
   (check-ok
    (define c% (class: object% (super-new)
                 (: m (Integer -> Integer))
                 (define/public (m x)
                   0)))
    (send (new c%) m 5))

   ;; fails, because the local call type is unknown
   ;; and is assumed to be Any
   (check-err
    (class: object% (super-new)
            (define/public (m) (n))
            (define/public (n x) 0)))

   ;; test type-checking for classes without any
   ;; internal type annotations on methods
   (check-ok
    (define c% (class: object% (super-new)
                 (define/public (m) 0)))
    (send (new c%) m))

   ;; test inheritance without expected
   (check-ok
    (define c% (class: (class: object% (super-new)
                         (: m (-> Integer))
                         (define/public (m) 0))
                 (super-new)
                 (: n (-> Integer))
                 (define/public (n) 1)))
    (send (new c%) m)
    (send (new c%) n))

   ;; test fields without expected class type
   (check-ok
    (define c% (class: object% (super-new)
                 (: x Integer)
                 (field [x 0])))
    (get-field x (new c%)))))

