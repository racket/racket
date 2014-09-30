#lang racket/base

;; Unit tests for typed classes

(require (submod "typecheck-tests.rkt" test-helpers)
         (except-in "test-utils.rkt" private)
         (for-syntax racket/base
                     typed-racket/tc-setup
                     typed-racket/utils/tc-utils))

(provide tests)
(gen-test-main)

(begin-for-syntax
  ;; for checking the printing of type aliases
  (current-type-names (init-current-type-names)))

;; see typecheck-tests.rkt for rationale on imports
(require rackunit
         (except-in racket/class
                    class define/public define/override
                    define/pubment define/augment define/private)
         (except-in typed-racket/utils/utils private)
         (except-in (base-env extra-procs prims class-prims
                              base-types base-types-extra)
                    define lambda λ case-lambda)
         (prefix-in tr: (only-in (base-env prims) define lambda λ case-lambda))
         (for-syntax (rep type-rep filter-rep object-rep)
                     (rename-in (types abbrev union numeric-tower filter-ops utils)
                                [Un t:Un]
                                [-> t:->])))

(define tests
  (test-suite
   "class typechecking tests"
   #reader typed-racket/typed-reader
   ;; Basic class with init and public method
   [tc-e (let ()
           (: c% (Class (init [x Integer])
                        [m (Integer -> Integer)]))
           (define c%
             (class object%
               (super-new)
               (init x)
               (define/public (m x) 0)))
           (send (new c% [x 1]) m 5))
         -Integer]
   ;; Fails, bad superclass expression
   [tc-err (let ()
             (: d% (Class (init [x Integer])
                          [m (Integer -> Integer)]))
             (define d% (class 5
                          (super-new)
                          (init x)
                          (define/public (m x) 0)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"superclass expression should produce a class"]
   ;; Method using argument type
   [tc-e (let ()
           (: e% (Class (init [x Integer])
                        [m (Integer -> Integer)]))
           (define e% (class object%
                        (super-new)
                        (init x)
                        (define/public (m x) x)))
           (void))
         -Void]
   ;; Send inside a method
   [tc-e (let ()
           (: f% (Class (init [x Integer])
                        [m (Integer -> Integer)]))
           (define f% (class object%
                        (super-new)
                        (init x)
                        (define/public (m x) (send this m 3))))
           (void))
         -Void]
   ;; Fails, send to missing method
   [tc-err (let ()
             (: g% (Class (init [x Integer #:optional])
                          [m (Integer -> Integer)]))
             (define g% (class object%
                          (super-new)
                          (init [x 0])
                          (define/public (m x) (send this z))))
             (void))
           #:ret (ret -Void)
           #:msg #rx"method not understood.*method name: z"]
   ;; Send to other methods
   [tc-e (let ()
           (: h% (Class [n (-> Integer)]
                        [m (Integer -> Integer)]))
           (define h% (class object%
                        (super-new)
                        (define/public (n) 0)
                        (define/public (m x) (send this n))))
           (void))
         -Void]
   ;; Local sends
   [tc-e (let ()
           (: i% (Class [n (-> Integer)]
                        [m (Integer -> Integer)]))
           (define i% (class object%
                        (super-new)
                        (define/public (n) 0)
                        (define/public (m x) (n))))
           (void))
         -Void]
   ;; Send to non object
   [tc-err (send 4 m 3)
      #:ret (ret (-val 5) -bot-filter)
      #:expected (ret (-val 5) -no-filter -no-obj)]
   ;; Fails, sending to multiple/unknown values
   [tc-err (send (values 'a 'b) m 'c)
           #:msg #rx"expected single value"]
   [tc-err (send (eval "3") m 'c)
           #:msg #rx"expected single value"]
   ;; Send to a union of objects in various ways
   [tc-e (let ()
           (: f (-> (U (Object [m (-> String)])
                       (Object [m (-> Symbol)]
                               [n (-> Void)]))
                    (U Symbol String)))
           (define (f o) (send o m))
           (f (new (class object% (super-new) (define/public (m) "foo")))))
         (t:Un -String -Symbol)]
   [tc-e (let ()
           (: f (-> (U (Object [m (-> (values String Symbol))])
                       (Object [m (-> (values Symbol String))]
                               [n (-> Void)]))
                    (values (U Symbol String) (U Symbol String))))
           (define (f o) (send o m))
           (f (new (class object% (super-new)
                     (define/public (m) (values "foo" 'bar))))))
         #:ret (ret (list (t:Un -String -Symbol) (t:Un -String -Symbol)))]
   [tc-err
    (let ()
      (define obj
        (if (> (random) 0.5)
            (new (class object% (super-new)
                   (define/public (m) "foo")))
            (new (class object% (super-new)
                   (define/public (m) (values "foo" "bar"))))))
      (send obj m))
    #:msg #rx"Expected the same number of values.*got 1 and 2"]
   ;; Field access via get-field
   [tc-e (let ()
           (: j% (Class (field [n Integer])
                        [m (-> Integer)]))
           (define j% (class object%
                        (super-new)
                        (field [n 0])
                        (define/public (m) (get-field n this))))
           (void))
         -Void]
   ;; Test set-field!
   [tc-e (set-field! x
           (new (class object%
                  (super-new)
                  (field [x : String "foo"])))
           "bar")
         -Void]
   ;; fails, check set-field! type error
   [tc-err (set-field! x
             (new (class object%
                    (super-new)
                    (field [x : String "foo"])))
             'not-string)
           #:ret (ret -Void)
           #:msg #rx"set-field! only allowed with"]
   ;; fails, field's default value has wrong type
   [tc-err (class object% (super-new)
             (: x Symbol)
             (field [x "foo"]))
           #:ret (ret (-class #:field ([x -Symbol])))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; Fail, field access to missing field
   [tc-err (let ()
             (: k% (Class [m (-> Integer)]))
             (define k% (class object%
                          (super-new)
                          (define/public (m) (get-field n this))))
             (void))
           #:ret (ret -Void)
           #:msg #rx"missing an expected field.*field: n"]
   ;; Fail, conflict with parent field
   [tc-err (let ()
             (: j% (Class (field [n Integer])
                          [m (-> Integer)]))
             (define j% (class object%
                          (super-new)
                          (field [n 0])
                          (define/public (m) (get-field n this))))
             (: l% (Class (field [n Integer])
                          [m (-> Integer)]))
             (define l% (class j%
                          (field [n 17])
                          (super-new)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"has a conflicting public field.*field: n"]
   ;; Fail, conflict with parent method
   [tc-err (let ()
             (: j% (Class [m (-> Integer)]))
             (define j% (class object%
                          (super-new)
                          (define/public (m) 15)))
             (: m% (Class [m (-> Integer)]))
             (define m% (class j%
                          (super-new)
                          (define/public (m) 17)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"has a conflicting public method.*method: m"]
   ;; Inheritance
   [tc-e (let ()
           (: j% (Class (field [n Integer])
                        [m (-> Integer)]))
           (define j% (class object%
                        (super-new)
                        (field [n 0])
                        (define/public (m) (get-field n this))))
           (: n% (Class (field [n Integer])
                        [m (-> Integer)]))
           (define n% (class j% (super-new)))
           (void))
         -Void]
   ;; fail, superclass expression is not a class with no expected type
   [tc-err (class "foo" (super-new))
           #:ret (ret (-class))
           #:msg "expected: a class"]
   ;; should fail, too many methods
   [tc-err (let ()
             (: o% (Class))
             (define o% (class object%
                          (super-new)
                          (define/public (m) 0)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"public method that should be absent.*method: m"]
   ;; same as previous
   [tc-err (let ()
             (: c% (Class [m (Integer -> Integer)]))
             (define c% (class object% (super-new)
                          (define/public (m x) (add1 x))
                          (define/public (n) 0)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"public method that should be absent.*method: n"]
   ;; fails, too many inits
   [tc-err (let ()
             (: c% (Class))
             (define c% (class object% (super-new)
                          (init x)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"initialization argument that should be absent.*argument: x"]
   ;; fails, init should be optional but is mandatory
   [tc-err (let ()
             (: c% (Class (init [str String #:optional])))
             (define c% (class object% (super-new)
                          (init str)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"missing a required optional init argument.*argument: str"]
   ;; fails, too many fields
   [tc-err (let ()
             (: c% (Class (field [str String])))
             (define c% (class object% (super-new)
                          (field [str "foo"] [x 0])))
             (void))
           #:ret (ret -Void)
           #:msg #rx"has a public field that should be absent.*public field: x"]
   ;; test that an init with no annotation still type-checks
   ;; (though it will have the Any type)
   [tc-e (let () (class object% (super-new) (init x)) (void)) -Void]
   ;; test that a field with no annotation still type-checks
   ;; (though it will have the Any type)
   [tc-e (let () (class object% (super-new) (field [x 0])) (void)) -Void]
   ;; Mixin on classes without row polymorphism
   [tc-e (let ()
           (: mixin ((Class [m (-> Integer)])
                     ->
                     (Class [m (-> Integer)]
                            [n (-> String)])))
           (define (mixin cls)
             (class cls
               (super-new)
               (define/public (n) "hi")))

           (: arg-class% (Class [m (-> Integer)]))
           (define arg-class%
             (class object%
               (super-new)
               (define/public (m) 0)))

           (mixin arg-class%)
           (void))
         -Void]
   ;; Fail, bad mixin
   [tc-err (let ()
             (: mixin ((Class [m (-> Integer)])
                       ->
                       (Class [m (-> Integer)]
                              [n (-> String)])))
             (define (mixin cls)
               (class cls
                 (super-new)))

             (: arg-class% (Class [m (-> Integer)]))
             (define arg-class%
               (class object%
                 (super-new)
                 (define/public (m) 0)))

             (mixin arg-class%))
           #:ret (ret (-class #:method ([m (t:-> -Integer)] [n (t:-> -String)])))
           #:msg #rx"missing a required public method.*missing public method: n"]
   ;; Fail, bad mixin argument
   [tc-err (let ()
             (: mixin ((Class [m (-> Symbol)])
                       ->
                       (Class [m (-> Symbol)]
                              [n (-> String)])))
             (define (mixin cls)
               (class cls
                 (super-new)
                 (define/public (n) "hi")))

             (: arg-class% (Class [k (-> Symbol)]))
             (define arg-class%
               (class object%
                 (super-new)
                 (define/public (k) 'k)))

             (mixin arg-class%)
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: \\(Class \\(m \\(-> Symbol\\)\\)\\)"]
   ;; classes that don't use define/public directly
   [tc-e (let ()
           (: c% (Class [m (Number -> String)]))
           (define c%
             (class object%
               (super-new)
               (public m)
               (define-values (m)
                 (lambda (x) (number->string x)))))
            (send (new c%) m 4))
         -String]
   ;; check that classes work in let clauses
   [tc-e (let: ([c% : (Class [m (Number -> String)])
                 (class object%
                   (super-new)
                   (public m)
                   (define-values (m)
                     (lambda (x) (number->string x))))])
           (send (new c%) m 4))
         -String]
   ;; check a good super-new call
   [tc-e (let ()
           (: c% (Class (init [x Integer])))
           (define c% (class object% (super-new) (init x)))
           (: d% (Class))
           (define d% (class c% (super-new [x (+ 3 5)])))
           (void))
         -Void]
   ;; fails, missing super-new
   [tc-err (let ()
             (: c% (Class (init [x Integer])))
             (define c% (class object% (init x)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"must call `super-new'"]
   ;; fails, non-top-level super-new
   ;; FIXME: this case also spits out additional untyped identifier
   ;;        errors which should be squelched maybe
   [tc-err (let ()
             (: c% (Class (init [x Integer])))
             (define c% (class object% (let () (super-new)) (init x)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"must call `super-new'"]
   ;; fails, bad super-new argument
   [tc-err (let ()
             (: c% (Class (init [x Symbol])))
             (define c% (class object% (super-new) (init x)))
             (: d% (Class))
             (define d% (class c% (super-new [x "bad"])))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: Symbol.*given: String"]
   ;; test override
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) (add1 y))))
           (: d% (Class [m (Integer -> Integer)]))
           (define d% (class c% (super-new)
                        (define/override (m y) (* 2 y))))
           (void))
         -Void]
   ;; test local call to overriden method
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) (add1 y))))
           (: d% (Class [n (Integer -> Integer)]
                        [m (Integer -> Integer)]))
           (define d% (class c% (super-new)
                        (define/public (n x) (m x))
                        (define/override (m y) (* 2 y))))
           (void))
         -Void]
   ;; fails, superclass missing public for override
   [tc-err (let ()
             (: d% (Class [m (String -> String)]))
             (define d% (class object% (super-new)
                          (define/override (m y)
                            (string-append (assert y string?) "foo"))))
             (void))
           #:ret (ret -Void)]
   ;; local field access and set!
   [tc-e (let ()
           (: c% (Class (field [x Integer])
                        [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (field [x 0])
                        (define/public (m y)
                          (begin0 x (set! x (+ x 1))))))
           (void))
         -Void]
   ;; test top-level expressions in the class
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) 0)
                        (+ 3 5)))
           (void))
         -Void]
   ;; test top-level method call
   [tc-e (let ()
           (: c% (Class [m (Integer -> Integer)]))
           (define c% (class object% (super-new)
                        (define/public (m y) 0)
                        (m 3)))
           (void))
         -Void]
   ;; test top-level field access
   [tc-e (let ()
           (: c% (Class (field [f String])))
           (define c% (class object% (super-new)
                        (field [f "foo"])
                        (string-append f "z")))
           (void))
         -Void]
   ;; fails, bad top-level expression
   [tc-err (let ()
             (: c% (Class [m (Symbol -> Symbol)]))
             (define c% (class object% (super-new)
                          (define/public (m y) 'a)
                          (string-append (string->symbol "a") "a")))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: String.*given: Symbol"]
   ;; fails, ill-typed method call
   [tc-err (let ()
             (: c% (Class [m (Symbol -> Symbol)]))
             (define c% (class object% (super-new)
                          (define/public (m y) 'a)
                          (m "foo")))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: Symbol.*given: String"]
   ;; fails, ill-typed field access
   [tc-err (let ()
             (: c% (Class (field [f String])))
             (define c% (class object% (super-new)
                          (field [f "foo"])
                          (set! f 'a)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: String.*given: 'a"]
   ;; test private field
   [tc-e (let ()
           (class object%
             (super-new)
             (: x Integer)
             (define x 5)
             (set! x 8)
             (+ x 1))
           (: d% (Class (field [y String])))
           (define d%
             (class object%
               (super-new)
               (: x Integer)
               (define x 5)
               (: y String)
               (field [y "foo"])))
           (void))
         -Void]
   ;; fails, bad private field set!
   [tc-err (class object%
             (super-new)
             (: x Symbol)
             (define x 'a)
             (set! x "foo"))
           #:ret (ret (-class))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; fails, bad private field default
   [tc-err (class object%
             (super-new)
             (: x Symbol)
             (define x "foo"))
           #:ret (ret (-class))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; ok, synthesis works on private fields
   [tc-e (class object% (super-new)
           (define x "foo") (string-append x "bar"))
         (-class)]
   ;; test private method
   [tc-e (let ()
           (class object% (super-new)
             (: x (-> Integer))
             (define/private (x) 3)
             (: m (-> Integer))
             (define/public (m) (x)))
           (void))
         -Void]
   ;; fails, public and private types conflict
   [tc-err (class object% (super-new)
             (: x (-> Symbol))
             (define/private (x) 'a)
             (: m (-> String))
             (define/public (m) (x)))
           #:ret (ret (-class #:method ([m (t:-> -String)])))
           #:msg #rx"expected: String.*given: Symbol"]
   ;; fails, not enough annotation on private
   [tc-err (class object% (super-new)
             (define/private (x) 3)
             (: m (-> Integer))
             (define/public (m) (x)))
           #:ret (ret (-class #:method ([m (t:-> -Integer)])))
           #:msg #rx"Cannot apply expression of type Any"]
   ;; fails, ill-typed private method implementation
   [tc-err (class object% (super-new)
             (: x (-> Symbol))
             (define/private (x) "bad result"))
           #:ret (ret (-class))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; test optional init arg
   [tc-e (let ()
           (: c% (Class (init [x Integer #:optional])))
           (define c% (class object% (super-new)
                        (: x Integer)
                        (init [x 0])))
           (void))
         -Void]
   ;; test init coverage when all optionals are
   ;; in the superclass
   [tc-e (let ()
           (: c% (Class (init [x Integer #:optional])))
           (: d% (Class (init [x Integer #:optional])))
           (define c% (class object% (super-new)
                        (: x Integer)
                        (init [x 0])))
           (define d% (class c% (super-new)))
           (void))
         -Void]
   ;; fails, expected mandatory but got optional
   [tc-err (let ()
             (: c% (Class (init [x Integer])))
             (define c% (class object% (super-new)
                          (: x Integer)
                          (init [x 0])))
             (void))
           #:ret (ret -Void)
           #:msg #rx"has a optional init argument that should be absent"]
   ;; fails, mandatory init not provided
   [tc-err (let ()
             (define d% (class object% (super-new)
                          (: x Integer)
                          (init x)))
             (new d%))
           #:ret (ret (-object #:init ([x -Integer #f])))
           #:msg #rx"value not provided for named init arg x"]
   ;; test that provided super-class inits don't count
   ;; towards the type of current class
   [tc-e (let ()
           (: c% (Class))
           (define c% (class (class object% (super-new)
                               (: x Integer)
                               (init x))
                        (super-new [x 3])))
           (void))
         -Void]
   ;; fails, super-class init already provided
   [tc-err (let ()
             (define c% (class (class object% (super-new)
                                 (: x Integer)
                                 (init x))
                          (super-new [x 3])))
             (new c% [x 5]))
           #:ret (ret (-object))]
   ;; fails, super-new can only be called once per class
   [tc-err (class object% (super-new) (super-new))
           #:ret (ret (-class))
           #:msg #rx"`super-new' a single time"]
   ;; test passing an init arg to super-new
   [tc-e (let ()
           (define c% (class (class object% (super-new)
                               (: x Integer)
                               (init x))
                        (: x Integer)
                        (init x)
                        (super-new [x x])))
           (new c% [x 5])
           (void))
         -Void]
   ;; fails, bad argument type to super-new
   [tc-err (class (class object% (super-new)
                    (: x Integer)
                    (init x))
             (: x String)
             (init x)
             (super-new [x x]))
           #:ret (ret (-class #:init ([x -String #f])))]
   ;; fails, superclass does not accept this init arg
   [tc-err (class object% (super-new [x 3]))
           #:ret (ret (-class))
           #:msg "not accepted by superclass"]
   ;; test inherit method
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (Integer -> Integer))
                    (define/public (m x) (add1 x)))
             (super-new)
             (inherit m)
             (m 5))
           (void))
         -Void]
   ;; test inherit method in another method (next 3)
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (-> String String))
                    (define/public (m x) (string-append x "m")))
             (super-new)
             (inherit m)
             (: n (-> String))
             (define/public (n) (m "foo")))
           (void))
         -Void]
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (-> String String))
                    (define/public (m x) (string-append x "m")))
             (super-new)
             (inherit m)
             (define/public (n) (m "foo")))
           (void))
         -Void]
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (-> String String))
                    (define/public (m x) (string-append x "m")))
             (super-new)
             (inherit m)
             (define/private (n) (m "foo")))
           (void))
         -Void]
   ;; test internal name with inherit
   [tc-e (let ()
           (class (class object% (super-new)
                    (: m (Integer -> Integer))
                    (define/public (m x) (add1 x)))
             (super-new)
             (inherit [n m])
             (n 5))
           (void))
         -Void]
   ;; test inherit field
   [tc-e (let ()
           (class (class object% (super-new)
                    (field [x : Integer 0]))
             (super-new)
             (inherit-field x))
           (void))
         -Void]
   ;; test internal name with inherit-field
   [tc-e (let ()
           (class (class object% (super-new)
                    (field [x : String "b"]))
             (super-new)
             (inherit-field [y x])
             (set! y "a"))
           (void))
         -Void]
   ;; fails, superclass missing inherited field
   [tc-err (class (class object% (super-new))
             (super-new)
             (inherit-field [y x]))
           #:ret (ret (-class))
           #:msg #rx"superclass is missing a required field"]
   ;; fails, missing super method for inherit
   [tc-err (class (class object% (super-new)) (super-new) (inherit z))
           #:ret (ret (-class))]
   ;; fails, bad argument type to inherited method
   [tc-err (class (class object% (super-new)
                    (: m (Integer -> Integer))
                    (define/public (m x) (add1 x)))
             (super-new)
             (inherit m)
             (m "foo"))
           #:ret (ret (-class #:method ([m (t:-> -Integer -Integer)])))]
   ;; test that keyword methods type-check
   ;; FIXME: send with keywords does not work yet
   [tc-e (let ()
           (: c% (Class [n (Integer #:foo Integer -> Integer)]))
           (define c%
             (class object%
               (super-new)
               (define/public (n x #:foo foo)
                 (+ foo x))))
           (void))
         -Void]
   ;; test instance subtyping
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: x (U False Number))
               (field [x 0])))
           (: x (Instance (Class)))
           (define x (new c%))
           (void))
         -Void]
   ;; failing instance subtyping
   [tc-err (let ()
             (define x (new (class object% (super-new) (define/public (m) "m"))))
             (ann x (Object [n (-> String)]))
             (error "foo"))
           #:msg #rx"expected: .*n.*given:.*m.*"]
   ;; test use of `this` in field default
   [tc-e (let ()
           (class object%
             (super-new)
             (: x Integer)
             (field [x 0])
             (: y Integer)
             (field [y (get-field x this)]))
           (void))
         -Void]
   ;; test super calls
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m (String -> String))
               (define/public (m x) "a")))
           (define d%
             (class c%
               (super-new)
               (define/override (m x)
                 (string-append "x" (super m "b")))))
           (send (new d%) m "c"))
         -String]
   ;; test super calls at top-level
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m (Integer -> Integer))
               (define/public (m x) 0)))
           (define d%
             (class c%
               (super-new)
               (super m 5)
               (define/override (m x) 5)))
           (void))
         -Void]
   ;; fails, bad super call argument
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Integer -> Integer))
                 (define/public (m x) 0)))
             (define d%
               (class c%
                 (super-new)
                 (super m "foo")
                 (define/override (m x) 5))))]

   ;; test different internal/external names
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (public [m n])
                        (define m (lambda () 0))))
           (send (new c%) n)
           (void))
         -Void]
   ;; test local calls with internal/external
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: m (-> String))
                        (public [m n])
                        (define m (lambda () "a"))
                        (: z (-> String))
                        (define/public (z) (m))))
           (send (new c%) z))
         -String]
   ;; internal/external the same is ok
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (public [m m])
                        (define m (lambda () "a"))))
           (send (new c%) m))
         #:ret (ret -String -true-filter)]
   ;; fails, internal name not accessible
   [tc-err (let ()
             (define c% (class object% (super-new)
                          (public [m n])
                          (define m (lambda () 0))))
             (send (new c%) m))]
   ;; test internal/external with expected
   [tc-e (let ()
           (: c% (Class [n (-> String)]))
           (define c% (class object% (super-new)
                        (public [m n])
                        (define m (lambda () "a"))))
           (send (new c%) n))
         -String]
   ;; test internal/external field
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: f String)
                        (field ([f g] "a"))))
           (get-field g (new c%)))
         -String]
   ;; fail, internal name not accessible
   [tc-err (let ()
             (define c% (class object% (super-new)
                          (: f String)
                          (field ([f g] "a"))))
             (get-field f (new c%)))]
   ;; test internal/external init
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: i String)
                        (init ([i j]))))
           (new c% [j "a"])
           (void))
         -Void]
   ;; fails, internal name not accessible
   [tc-err (let ()
             (define c% (class object% (super-new)
                          (: i Integer)
                          (init ([i j]))))
             (new c% [i 5]))
           #:ret (ret (-object #:init ([j -Integer #f])))]
   ;; test that different internal names can map to the same external name
   ;; and that the internal-external name mapping is set correctly.
   [tc-e (class object%
           (super-new)
           (: x* String)
           (init [(x* x)])
           (field [x "x"]))
         (-class #:init ([x -String #f]) #:field ([x Univ]))]
   ;; test init default values
   [tc-e (let ()
           (class object% (super-new)
             (: z Integer)
             (init [z 0]))
           (void))
         -Void]
   ;; fails, bad default init value
   [tc-err (class object% (super-new)
             (: z Integer)
             (init [z "foo"]))
           #:ret (ret (-class #:init ([z -Integer #t])))
           #:msg #rx"expected: Integer.*given: String"]
   ;; test init field default value
   [tc-e (let ()
           (define c% (class object% (super-new)
                 (: x Integer)
                 (init-field ([x y] 0))))
           (void))
         -Void]
   ;; fails, wrong init-field default
   [tc-err (class object% (super-new)
             (: x Integer)
             (init-field ([x y] "foo")))
           #:ret (ret (-class #:init ([y -Integer #t]) #:field ([y -Integer])))]
   ;; test type-checking method with internal/external
   [tc-err (let ()
             (: c% (Class [n (Integer -> Integer)]))
             (define c% (class object% (super-new)
                          (public [m n])
                          (define m (lambda () 0)))))]
   ;; test type-checking without expected class type
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: m (String -> String))
                        (define/public (m x) "a")))
           (send (new c%) m "b"))
         -String]
   ;; fails, because the local call type is unknown
   ;; and is assumed to be Any
   [tc-err (class object% (super-new)
             (define/public (m) (n))
             (define/public (n x) 0))
           #:ret (ret (-class #:method ([m (t:-> -Bottom)] [n (t:-> Univ -Zero : -true-filter)])))
           #:msg #rx"since it is not a function type"]
   ;; test type-checking for classes without any
   ;; internal type annotations on methods
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (define/public (m) "a")))
           (send (new c%) m))
         #:ret (ret -String -true-filter)]
   ;; test inheritance without expected
   [tc-e (let ()
           (define c% (class (class object% (super-new)
                               (: m (-> String))
                               (define/public (m) "a"))
                        (super-new)
                        (: n (-> String))
                        (define/public (n) "b")))
           (send (new c%) m)
           (send (new c%) n))
         -String]
   ;; test fields without expected class type
   [tc-e (let ()
           (define c% (class object% (super-new)
                        (: x String)
                        (field [x "a"])))
           (get-field x (new c%)))
         -String]
   ;; row polymorphism, basic example with instantiation
   [tc-e (let ()
           (: f (All (A #:row (field x))
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (inst f #:row (field [y Integer]))
           (void))
         -Void]
   ;; fails, because the instantiation uses a field that
   ;; is supposed to be absent via the row constraint
   [tc-err (let ()
             (: f (All (A #:row (field x))
                    ((Class #:row-var A)
                     ->
                     (Class #:row-var A (field [x Integer])))))
             (define (f cls)
               (class cls (super-new)
                 (field [x 5])))
             (inst f #:row (field [x Integer])))
           #:ret (ret (t:-> (-class 
                              #:row (make-Row null `([x ,-Integer]) null null #f))
                            (-class
                              #:row (make-Row null `([x ,-Integer]) null null #f)
                              #:field ([x -Integer])))
                      -true-filter)]
   ;; fails, mixin argument is missing required field
   [tc-err (let ()
             (: f (All (A #:row (field x))
                    ((Class #:row-var A)
                     ->
                     (Class #:row-var A (field [x Integer])))))
             (define (f cls)
               (class cls (super-new)
                 (field [x 5])))
             (define instantiated
               (inst f #:row (field [y Integer])))
             (instantiated
              (class object% (super-new))))
           #:ret (ret (-class
                        #:row (make-Row null (list (list 'y -Integer)) null null #f)
                        #:field ([x -Integer])))]
   ;; fails, the argument object lacks required fields (with inference)
   [tc-err (let ()
             (: mixin (All (r #:row)
                         (-> (Class (field [x Any]) #:row-var r)
                             (Class (field [x Any]) #:row-var r))))
             (define (mixin cls) cls)
             (mixin object%))
           #:ret (ret (-class #:row (make-Row null null null null #f)
                              #:field ([x Univ])))
           #:msg (regexp-quote "expected: (Class (field (x Any)))")]
   ;; mixin application succeeds
   [tc-e (let ()
           (: f (All (A #:row (field x))
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (define instantiated
             (inst f #:row (field [y Integer])))
           (instantiated
            (class object% (super-new)
              (: y Integer)
              (field [y 0])))
           (void))
         -Void]
   ;; Basic row constraint inference
   [tc-e (let ()
           (: f (All (A #:row) ; inferred
                  ((Class #:row-var A)
                   ->
                   (Class #:row-var A (field [x Integer])))))
           (define (f cls)
             (class cls (super-new)
               (field [x 5])))
           (inst f #:row (field [y Integer]))
           (void))
         -Void]
   ;; fails, inferred constraint and instantiation don't match
   [tc-err (let ()
             (: f (All (A #:row)
                    ((Class #:row-var A)
                     ->
                     (Class #:row-var A (field [x Integer])))))
             (define (f cls)
               (class cls (super-new)
                 (field [x 5])))
             (inst f #:row (field [x Integer])))
           #:ret (ret (t:-> (-class 
                              #:row (make-Row null `([x ,-Integer]) null null #f))
                            (-class
                              #:row (make-Row null `([x ,-Integer]) null null #f)
                              #:field ([x -Integer])))
                      -true-filter)]
   ;; Check simple use of pubment
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x) "a")))
           (send (new c%) m "b"))
         -String]
   ;; Local calls to pubment method
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x) "a")
               (: n (-> String))
               (define/public (n) (m "b"))))
           (send (new c%) n))
         -String]
   ;; Inheritance with augment
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x) "a")))
           (define d%
             (class c%
               (super-new)
               (define/augment (m x) (string-append x "b"))))
           (send (new c%) m "c"))
         -String]
   ;; Pubment with inner
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m : String -> String)
               (define/pubment (m x)
                 (inner "a" m x))))
           (define d%
             (class c%
               (super-new)
               (define/augment (m x)
                 (string-append "foo" x))))
           (send (new c%) m "b"))
         -String]
   ;; make sure augment type is reflected in class type
   [tc-e (let ()
           (: c% (Class (augment [m (String -> Integer)])
                        [m (Integer -> Integer)]))
           (define c%
             (class object% (super-new)
               (: m (Integer -> Integer)
                  #:augment (String -> Integer))
               (define/pubment (m x) x)))
           (void))
         -Void]
   ;; pubment with different augment type
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: m (Symbol -> Symbol)
                  #:augment (String -> String))
               (define/pubment (m x)
                 (inner "" m "foo") 'a)))
           (define d%
             (class c%
               (super-new)
               (define/augment (m x)
                 (string-append x "bar"))))
           (send (new c%) m 'b))
         -Symbol]
   ;; fail, bad inner argument
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Symbol -> Symbol)
                    #:augment (String -> String))
                 (define/pubment (m x)
                   (inner "" m x) 'a)))
             (define d%
               (class c%
                 (super-new)
                 (define/augment (m x)
                   (string-append x "bar"))))
             (send (new c%) m 'b))
           #:ret (ret -Symbol)
           #:msg #rx"expected: String.*given: Symbol"]
   ;; Fail, bad inner default
   [tc-err (class object%
             (super-new)
             (: m (Symbol -> Symbol))
             (define/pubment (m x)
               (inner "foo" m x)))
           #:ret (ret (-class #:method ([m (t:-> -Symbol -Symbol)])
                              #:augment ([m (t:-> -Symbol -Symbol)])))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; Fail, wrong number of arguments to inner
   [tc-err (class object%
             (super-new)
             (: m (Integer -> Integer))
             (define/pubment (m x)
               (inner 3 m)))
           #:ret (ret (-class #:method ([m (t:-> -Integer -Integer)])
                              #:augment ([m (t:-> -Integer -Integer)])))
           #:msg #rx"wrong number of arguments provided.*expected: 2"]
   ;; Fail, bad augment type
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Symbol -> Symbol))
                 (define/pubment (m x)
                   (inner 'a m x))))
             (define d%
               (class c%
                 (super-new)
                 (define/augment (m x) "bad type")))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: Symbol.*given: String"]
   ;; Fail, cannot augment non-augmentable method
   [tc-err (let ()
             (define c%
               (class object%
                 (super-new)
                 (: m (Integer -> Integer))
                 (define/public (m x) 0)))
             (define d%
               (class c%
                 (super-new)
                 (define/augment (m x) 1)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"superclass is missing a required augmentable method"]
   ;; Pubment with separate internal/external names
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (: n (Symbol -> Symbol))
               (pubment [n m])
               (define n (λ (x) 'a))))
           (send (new c%) m 'b))
         -Symbol]
   ;; Pubment with expected class type
   [tc-e (let ()
           (: c% (Class [m (String -> String)]
                        (augment [m (String -> String)])))
           (define c%
             (class object%
               (super-new)
               (define/pubment (m x) "a")))
           (send (new c%) m "b"))
         -String]
   ;; fails, expected type not a class
   [tc-err (let ()
             (: c% String)
             (define c%
               (class object%
                 (super-new)
                 (: x Symbol)
                 (init-field x)))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: String"]
   ;; test polymorphic class
   [tc-e (let ()
           (: c% (All (A) (Class (init-field [x A]))))
           (define c%
             (class object%
               (super-new)
               (init-field x)))
           (new (inst c% Integer) [x 0])
           (void))
         -Void]
   ;; fails due to ill-typed polymorphic class body
   [tc-err (let ()
             (: c% (All (A) (Class (init-field [x A]))))
             (define c%
               (class object%
                 (super-new)
                 (init-field x)
                 (set! x "a")))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: A.*given: String"]
   ;; test polymorphism with keyword
   [tc-e (let ()
           (define point%
             (class object%
               #:∀ (X)
               (super-new)
               (init-field [x : X] [y : X])))
           (new (inst point% Integer) [x 0] [y 5])
           (new (inst point% String) [x "foo"] [y "bar"])
           (void))
         -Void]
   ;; test polymorphism with two type parameters
   [tc-e (let ()
           (define point%
             (class object%
               #:forall (X Y)
               (super-new)
               (init-field [x : X] [y : Y])))
           (new (inst point% Integer String) [x 0] [y "foo"])
           (new (inst point% String Integer) [x "foo"] [y 3])
           (void))
         -Void]
   ;; test class polymorphism with method
   [tc-e (let ()
           (define id%
             (class object%
               #:forall (X)
               (super-new)
               (: m (X -> X))
               (define/public (m x) x)))
           (send (new (inst id% String)) m "a"))
         -String]
   ;; fails because m is not parametric
   [tc-err (class object%
             #:forall (X)
             (super-new)
             (: m (X -> X))
             (define/public (m x) "a"))
           #:ret (ret (-poly (X) (-class #:method ([m (t:-> X X)]))))
           #:msg #rx"expected: X.*given: String"]
   ;; fails because default init value cannot be polymorphic
   [tc-err (class object%
             #:forall (Z)
             (super-new)
             (init-field [x : Z] [y : Z 0]))
           #:ret (ret (-poly (Z) (-class #:init-field ([x Z #f] [y Z #t]))))
           #:msg #rx"expected: Z.*given: Zero"]
   ;; fails because default field value cannot be polymorphic
   [tc-err (class object%
             #:forall (Z)
             (super-new)
             (field [x : Z "a"]))
           #:ret (ret (-poly (Z) (-class #:field ([x Z]))))
           #:msg #rx"expected: Z.*given: String"]
   ;; test in-clause type annotations (next several tests)
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (field [x : String "a"])))
           (string-append "b" (get-field x (new c%))))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (init-field [x : String "a"])))
           (string-append "c" (get-field x (new c% [x "b"]))))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (public [m : (String -> String)])
               (define (m x) (string-append x "foo"))))
           (send (new c%) m "bar"))
         -String]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (private [m : (String -> String)])
               (define (m x) (string-append x "foo"))))
           (void))
         -Void]
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (field [(x y) : String "a"])))
           (string-append "foo" (get-field y (new c%))))
         -String]
   ;; fails, duplicate type annotation
   [tc-err (class object%
             (super-new)
             (: x String)
             (field [x : Symbol 0]))
           #:ret (ret (-class #:field ([x -Symbol])))
           #:msg #rx"duplicate type annotation.*new type: String"]
   ;; fails, expected type and annotation don't match
   [tc-err (let ()
             (: c% (Class (field [x String])))
             (define c% (class object% (super-new)
                          (field [x : Symbol 'a])))
             (void))
           #:ret (ret -Void)
           #:msg #rx"expected: \\(Class \\(field \\(x String"]
   ;; fails, but make sure it's not an internal error
   [tc-err (class object% (super-new)
             (define/pubment (foo x) 0)
             (define/public (g x) (foo 3)))
           #:ret (ret (-class #:method ([g (t:-> Univ -Bottom)]
                                        [foo (t:-> Univ -Zero : -true-filter)])
                              #:augment ([foo top-func])))
           #:msg #rx"Cannot apply expression of type Any"]
   ;; the next several tests are for positional init arguments
   [tc-e (let ()
           (define c% (class object% (super-new) (init a b)))
           (new c% [a "a"] [b "b"])
           (make-object c% "a" "b")
           (instantiate c% ("a") [b "b"])
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object% (super-new) (init a [b 'b])))
           (new c% [a "a"] [b "b"])
           (new c% [a "a"])
           (make-object c% "a")
           (make-object c% "a" "b")
           (instantiate c% () [a "a"] [b "b"])
           (instantiate c% ("a") [b "b"])
           (instantiate c% ("a" "b"))
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class (class object%
                               (super-new)
                               (init [b 'b]))
                        (super-new) (init [a 'a])))
           (new c% [a "a"] [b "b"])
           (new c% [b "b"])
           (new c% [a "a"])
           (make-object c% "a")
           (make-object c% "a" "b")
           (instantiate c% () [a "a"] [b "b"])
           (instantiate c% ("a") [b "b"])
           (instantiate c% ("a" "b"))
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object%
                        (super-new)
                        (init-rest [rst : (List String String)])))
           (make-object c% "a" "b")
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object%
                        (super-new)
                        (init [a : Symbol])
                        (init-rest [rst : (List String String)])))
           (make-object c% 'a "b" "c")
           (void))
         -Void]
   [tc-e (let ()
           (define c% (class object%
                        (super-new)
                        (init-rest [rst : (U (List Symbol)
                                             (List String String))])))
           (make-object c% "b" "c")
           (make-object c% 'a)
           (void))
         -Void]
   [tc-err (let ()
             (define c% (class object%
                          (super-new)
                          (init-rest [rst : (List Symbol)])))
             (make-object c% "wrong"))
           #:ret (ret (make-Instance (make-Class #f null null null null (-Tuple (list -Symbol)))))
           #:msg #rx"expected: Symbol.*given: String"]
   ;; PR 14408, test init-field order
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (init-field [x : String] [y : Symbol])))
           (make-object c% "str" 'sym)
           (void))
         -Void]
   ;; a variant of the last, but testing that init and init-field
   ;; interleave correctly in the class type
   [tc-e (let ()
           (define c%
             (class object%
               (super-new)
               (init [a : 'a]) (init-field [x : 'x] [y : 'y]) (init [b 'b])))
           (make-object c% 'a 'x 'y 'b)
           (void))
         -Void]
   ;; fail, too many positional arguments to superclass
   [tc-err (class object% (super-make-object "foo"))
           #:ret (ret (-class))
           #:msg #rx"too many positional init arguments"]
   ;; check that case-lambda methods work
   [tc-e (let ()
           (class object%
             (super-new)
             (field [y : Integer 0])
             (: m (case-> (Any -> Integer)))
             (public m)
             (define m (case-lambda [(x) y])))
           (define c%
             (class object%
               (super-new)
               (: m (case-> (Any -> Void)))
               (public m)
               (define m (case-lambda [(x) (void)]))))
           (send (new c%) m 'anything))
         -Void]
   ;; fails, test that case-lambda bodies are checked
   [tc-err (class object%
             (super-new)
             (: m (case-> (Any -> Integer)))
             (public m)
             (define m (case-lambda [(x) "bad"])))
           #:ret (ret (-class #:method [(m (t:-> Univ -Integer))]))
           #:msg #rx"expected: Integer.*given: String"]
   ;; test that rest args work
   [tc-e (let ()
           (class object% (super-new)
             (: m (-> Integer * Integer))
             (define/public (m . xs) (apply + xs)))
           (void))
         -Void]
   ;; test that Name types are ok with get-field and as an
   ;; expected type for class type-checking
   [tc-e (let ()
           (define-type-alias Foo%
             (Class (init-field [x String])
                                [m (-> (Instance Foo%) String)]))
           (: foo% Foo%)
           (define foo%
             (class object% (super-new)
               (init-field x)
               (define/public (m a-foo) (get-field x a-foo))))
           (void))
         -Void]
   ;; test that filters are correctly handled for polymorphic classes
   [tc-e (let ()
           (class object%
             (super-new)
             (init x)))
         #:ret (ret (-poly (A) (-class #:init ([x A #f]))))
         #:expected (ret (-poly (A) (-class #:init ([x A #f]))) -no-filter -no-obj)]
   ;; test uses of a macro in the body of the class
   [tc-e
    (let ()
      (define c%
        (class object%
          (super-new)
          (define-syntax-rule (my-meth (m arg ...) . body)
            (define/public (m arg ...) . body))
          (my-meth (hello) (displayln "hello world"))))
      (send (new c%) hello))
    -Void]
   ;; the next few tests check failing class instantiation
   [tc-err (make-object object% 1)
           #:msg #rx"expected: 0 arguments.*given: 1 arguments"]
   [tc-err (make-object (ann object% ClassTop))
           #:msg #rx"cannot instantiate.*ClassTop"]
   [tc-err (make-object 3)
           #:msg #rx"value of a non-class type"]
   ;; PR 14726
   ;; test opt-arg but non-keyword method
   [tc-e (let ()
           (define-type-alias A%
             (Class [foo (->* [Integer] Void)]))
           (: a% A%)
           (define a%
             (class object%
               (super-new)
               (define/public (foo [i #f]) (void))))
           (new a%))
         (-object #:method ([foo (t:-> -Integer -Void)]))]
   [tc-e (let ()
           (define-type-alias A%
             (Class [foo (->* [] [Integer] Void)]))
           (: a% A%)
           (define a%
             (class object%
               (super-new)
               (define/public (foo [i #f]) (void))))
           (new a%))
         (-object #:method ([foo (cl->* (t:-> -Void) (t:-> -Integer -Void))]))]))
