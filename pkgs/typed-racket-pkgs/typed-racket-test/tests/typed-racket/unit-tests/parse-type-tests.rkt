#lang racket/base
(require "test-utils.rkt"
         "evaluator.rkt"
         (for-syntax
           racket/base
           racket/dict
           racket/set
           (base-env base-structs)
           (env tvar-env type-alias-env)
           (utils tc-utils)
           (private parse-type)
           (rep type-rep)

           (submod typed-racket/base-env/base-types initialize)
           (rename-in (types union abbrev numeric-tower resolve)
                      [Un t:Un] [-> t:->] [->* t:->*]))
         (only-in typed-racket/typed-racket do-standard-inits)
         (base-env base-types base-types-extra colon)
         ;; needed for parsing case-lambda/case-> types
         (only-in (base-env case-lambda) case-lambda)
         (only-in racket/class init init-field field augment)

         rackunit)

(provide tests)
(gen-test-main)

(begin-for-syntax
  (do-standard-inits))

(define-syntax (pt-test stx)
  (syntax-case stx (FAIL)
    [(_ FAIL ty-stx)
     (syntax/loc stx (pt-test FAIL ty-stx initial-tvar-env))]
    [(_ FAIL ty-stx tvar-env)
     (quasisyntax/loc stx
       (test-case #,(format "~a" (syntax->datum #'ty-stx))
         (unless
           (phase1-phase0-eval
             (with-handlers ([exn:fail:syntax? (lambda (exn) #'#t)])
               (parameterize ([current-tvars tvar-env]
                              [delay-errors? #f])
                 (parse-type (quote-syntax ty-stx)))
               #'#f))
           (fail-check "No syntax error when parsing type."))))]
    [(_ ts tv) (syntax/loc stx (pt-test ts tv initial-tvar-env))]
    [(_ ty-stx ty-val tvar-env)
     (quasisyntax/loc
       stx
       (test-case #,(format "~a" (syntax->datum #'ty-stx))
         (define-values (expected actual same?)
           (phase1-phase0-eval
             (parameterize ([current-tvars tvar-env]
                            [delay-errors? #f])
                (define expected ty-val)
                (define actual (parse-type (quote-syntax ty-stx)))
                #`(values #,expected #,actual #,(type-equal? actual expected)))))
          (unless same?
            (with-check-info (['expected expected] ['actual actual])
              (fail-check "Unequal types")))))]))

(define-syntax pt-tests
  (syntax-rules ()
    [(_ nm [elems ...] ...)
     (test-suite nm
                 (pt-test elems ...) ...)]))

(define-for-syntax N -Number)
(define-for-syntax B -Boolean)
(define-for-syntax Sym -Symbol)

(define tests
  (pt-tests
   "parse-type tests"
   [FAIL UNBOUND]
   [FAIL List]
   [FAIL (All (A) (List -> Boolean))]
   [Number N]
   [Any Univ]
   [(List Number String) (-Tuple (list N -String))]
   [(All (Number) Number) (-poly (a) a)]
   [(Number . Number) (-pair N N)]
   [(Listof Boolean) (make-Listof  B)]
   [(Vectorof (Listof Symbol)) (make-Vector (make-Listof Sym))]
   [(pred Number) (make-pred-ty N)]
   [(-> (values Number Boolean Number)) (t:-> (-values (list N B N)))]
   [(Number -> Number) (t:-> N N)]
   [(All (A) Number -> Number) (-poly (a) (t:-> N N))]
   [FAIL (All (A) -> Number Number)]
   [(All (A) (Number -> Number)) (-poly (a) (t:-> N N))]
   [(All (A) (-> Number Number)) (-poly (a) (t:-> N N))]
   [(All (A) A -> A) (-poly (a) (t:-> a a))]
   [(All (A) A → A) (-poly (a) (t:-> a a))]
   [FAIL (All (A) → A A)]
   [(All (A) (A -> A)) (-poly (a) (t:-> a a))]
   [(All (A) (-> A A)) (-poly (a) (t:-> a a))]
   [FAIL (All (A) -> Integer -> Integer -> Integer)]
   ;; requires transformer time stuff that doesn't work
   #;[(Refinement even?) (make-Refinement #'even?)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
   [(-> Number Number Number Boolean Number) (N N N B . t:-> . N)]
   [(Number Number Number * -> Boolean) ((list N N) N . t:->* . B)]
   [(-> Number Number Number * Boolean) ((list N N) N . t:->* . B)]
   ;[((. Number) -> Number) (->* (list) N N)] ;; not legal syntax
   [(U Number Boolean) (t:Un N B)]
   [(U Number Boolean Number) (t:Un N B)]
   [(U Number Boolean 1) (t:Un N B)]
   [(All (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(All (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(All (a ...) (-> a ... a Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(∀ (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a ...) (-> a ... a Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   [(All (a ...) (-> a ... Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   [(All (a ...) (-> (values a ...)))
    (-polydots (a) (t:-> (make-ValuesDots (list) a 'a)))]
   [(case-lambda (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                      [(N N) N])]
   [(case-> (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                 [(N N) N])]
   [(case-> (Number -> Boolean) (-> Number Number Number)) (cl-> [(N) B]
                                                                 [(N N) N])]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   ['(1 2 3) (-Tuple (map -val '(1 2 3)))]

   [(Listof Number) (make-Listof  N)]

   [a (-v a) (dict-set initial-tvar-env 'a (-v a))]

   [(Any -> Boolean : Number) (make-pred-ty -Number)]
   [(-> Any Boolean : Number) (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (Number @ 0) #:- (! Number @ 0))
    (make-pred-ty -Number)]
   [(-> Any Boolean : #:+ (Number @ 0) #:- (! Number @ 0))
    (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (! Number @ 0) #:- (Number @ 0))
    (t:->* (list Univ) -Boolean : (-FS (-not-filter -Number 0 null) (-filter -Number 0 null)))]
   [(-> Any Boolean : #:+ (! Number @ 0) #:- (Number @ 0))
    (t:->* (list Univ) -Boolean : (-FS (-not-filter -Number 0 null) (-filter -Number 0 null)))]
   [(All (a b) (-> (-> a Any : #:+ b) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-FS (-filter b 0) -top)) (-lst a) (-lst b)))]
   [(All (a b) (-> (-> a Any : #:+ (! b)) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-FS (-not-filter b 0) -top)) (-lst a) (-lst b)))]
   [(All (a b) (-> (-> a Any : #:- b) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-FS -top (-filter b 0))) (-lst a) (-lst b)))]
   [(All (a b) (-> (-> a Any : #:- (! b)) (Listof a) (Listof b)))
    (-poly (a b) (t:-> (asym-pred a Univ (-FS -top (-not-filter b 0))) (-lst a) (-lst b)))]
   [(Number -> Number -> Number)
    (t:-> -Number (t:-> -Number -Number))]
   [(-> Number (-> Number Number))
    (t:-> -Number (t:-> -Number -Number))]
   [(Integer -> (All (X) (X -> X)))
    (t:-> -Integer (-poly (x) (t:-> x x)))]
   [(-> Integer (All (X) (-> X X)))
    (t:-> -Integer (-poly (x) (t:-> x x)))]

   ;; ->* types
   [(->* (String Symbol) Void) (t:-> -String -Symbol -Void)]
   [(->* (String Symbol) (String) Void)
    (->opt -String -Symbol [-String] -Void)]
   [(->* (String Symbol) (String Symbol) Void)
    (->opt -String -Symbol [-String -Symbol] -Void)]
   [(->* (String Symbol) (String) (values Void String))
    (->opt -String -Symbol [-String] (-values (list -Void -String)))]
   [(->* (String Symbol) (String) #:rest Symbol Void)
    (->optkey -String -Symbol [-String] #:rest -Symbol -Void)]
   [(All (a) (->* (a Symbol) (String) #:rest Symbol Void))
    (-poly (a) (->optkey a -Symbol [-String] #:rest -Symbol -Void))]
   [(->* (Integer) (String #:foo Integer) Void)
    (->optkey -Integer [-String] #:foo -Integer #f -Void)]
   [(->* (Integer #:bar Integer) (String) Void)
    (->optkey -Integer [-String] #:bar -Integer #t -Void)]
   [(->* (Integer #:bar Integer) (String #:foo Integer) Void)
    (->optkey -Integer [-String] #:bar -Integer #t #:foo -Integer #f -Void)]

   [(Opaque foo?) (make-Opaque #'foo?)]
   ;; PR 14122
   [FAIL (Opaque 3)]

   ;; struct types
   [(Struct-Type arity-at-least) (make-StructType (resolve -Arity-At-Least))]
   [FAIL (Struct-Type Integer)]
   [FAIL (Struct-Type foo)]
   [Struct-TypeTop (make-StructTypeTop)]

   ;; keyword function types
   [(#:a String -> String)
    (->optkey [] #:a -String #t -String)]
   [([#:a String] -> String)
    (->optkey [] #:a -String #f -String)]
   [(#:a String #:b String -> String)
    (->optkey [] #:a -String #t #:b -String #t -String)]
   [([#:a String] #:b String -> String)
    (->optkey [] #:a -String #f #:b -String #t -String)]
   [(#:a String [#:b String] -> String)
    (->optkey [] #:a -String #t #:b -String #f -String)]
   [(String #:a String -> String)
    (->optkey -String [] #:a -String #t -String)]
   [(String #:a String String * -> String)
    (->optkey -String [] #:rest -String #:a -String #t -String)]
   [(String [#:a String] String * -> String)
    (->optkey -String [] #:rest -String #:a -String #f -String)]

   ;;; Classes
   [(Class) (-class)]
   [(Class (init [x Number] [y Number]))
    (-class #:init ([x -Number #f] [y -Number #f]))]
   [(Class (init [x Number] [y Number #:optional]))
    (-class #:init ([x -Number #f] [y -Number #t]))]
   [(Class (init [x Number]) (init-field [y Number]))
    (-class #:init ([x -Number #f]) #:init-field ([y -Number #f]))]
   [(Class [m (Number -> Number)])
    (-class #:method ([m (t:-> N N)]))]
   [(Class [m (Number -> Number)] (init [x Number]))
    (-class #:init ([x -Number #f]) #:method ([m (t:-> N N)]))]
   [(Class [m (Number -> Number)] (field [x Number]))
    (-class #:field ([x -Number]) #:method ([m (t:-> N N)]))]
   [(Class (augment [m (Number -> Number)]))
    (-class #:augment ([m (t:-> N N)]))]
   [(Class (augment [m (Number -> Number)]) (field [x Number]))
    (-class #:augment ([m (t:-> N N)]) #:field ([x -Number]))]
   [(Class (augment [m (-> Number)]) [m (-> Number)])
    (-class #:method ([m (t:-> N)]) #:augment ([m (t:-> N)]))]
   [FAIL (Class foobar)]
   [FAIL (Class [x UNBOUND])]
   [FAIL (Class [x Number #:random-keyword])]
   [FAIL (Class (random-clause [x Number]))]
   [FAIL (Class [m Number])]
   [FAIL (Class (augment [m Number]))]
   ;; test duplicates
   [FAIL (Class [x Number] [x Number])]
   [FAIL (Class (init [x Number]) (init [x Number]))]
   [FAIL (Class (init [x Number]) (init-field [x Number]))]
   [FAIL (Class (field [x Number]) (init-field [x Number]))]
   [FAIL (Class (augment [m (-> Number)] [m (-> Number)]))]
   [FAIL (Class (augment [m (-> Number)]) (augment [m (-> Number)]))]
   [FAIL (Class [m (-> Number)] [m (-> Number)])]
   ;; test #:row-var
   [(All (r #:row) (Class #:row-var r))
    (make-PolyRow (list 'r)
                  (list null null null null)
                  (-class #:row (make-F 'r)))]
   [FAIL (All (r #:row) (Class #:implements (Class #:row-var r)))]
   [FAIL (All (r #:row) (Class #:implements (Class) #:row-var r))]
   [FAIL (Class #:row-var 5)]
   [FAIL (Class #:row-var (list 3))]
   [FAIL (Class #:row-var x)]
   [FAIL (Class #:implements (Class #:row-var r) #:row-var x)]
   [FAIL (Class #:implements (Class #:row-var r) #:row-var r)]
   [FAIL (All (r #:row)
           (All (x #:row)
            (Class #:implements (Class #:row-var r) #:row-var x)))]
   [FAIL (All (r #:row) (Class #:implements (Class #:row-var r) #:row-var r))]
   ;; Test #:implements, some of these used to work but now they have to
   ;; refer to type aliases. Testing actual type aliases is hard here though.
   [FAIL (Class #:implements (Class [m (Number -> Number)]) (field [x Number]))]
   [FAIL (Class #:implements (Class [m (Number -> Number)])
                #:implements (Class [n (Number -> Number)])
                (field [x Number]))]
   [FAIL (Class #:implements (Class [m (Number -> Number)])
                #:implements (Class [m (Number -> Number)])
                (field [x Number]))]
   [FAIL (Class #:implements (Class (init [x Integer]) [m (Number -> Number)])
                (field [x Number]))]
   [FAIL (Class #:implements Number)]
   [FAIL (Class #:implements Number [m (Number -> Number)])]
   [FAIL (Class #:implements (Class [m (Number -> Number)]) [m String])]
   [FAIL (Class #:implements (Class [m (Number -> Number)])
                #:implements (Class [m (String -> String)])
                (field [x Number]))]
   [FAIL (Class #:implements (Class (augment [m (Number -> Number)]))
                #:implements (Class (augment [m (String -> String)]))
                (field [x Number]))]
   [FAIL (Class #:implements (Class (augment [m (Number -> Number)]))
                (augment [m (-> Number)]))]
   ;; Test Object types
   [(Object) (-object)]
   [(Object [m (Number -> Number)])
    (-object #:method ([m (t:-> N N)]))]
   [(Object [m (Number -> Number)] (field [f Number]))
    (-object #:method ([m (t:-> N N)]) #:field ([f N]))]
   [FAIL (Object foobar)]
   [FAIL (Object [x UNBOUND])]
   [FAIL (Object [x Number #:random-keyword])]
   [FAIL (Object (random-clause [x Number]))]
   [FAIL (Object [x Number] [x Number])]
   [FAIL (Object (field [x Number]) (field [x Number]))]
   [FAIL (Object [x Number] [x Number])]
   [FAIL (Object [m Number])]
   ;; Test row polymorphic types
   [(All (r #:row) ((Class #:row-var r) -> (Class #:row-var r)))
    (-polyrow (r) (list null null null null)
      (t:-> (-class #:row r) (-class #:row r)))]
   [(All (r #:row (init x y z) (field f) m n)
      ((Class #:row-var r) -> (Class #:row-var r)))
    (-polyrow (r) (list '(x y z) '(f) '(m n) '())
      (t:-> (-class #:row r) (-class #:row r)))]
   ;; Class types cannot use a row variable that doesn't constrain
   ;; all of its members to be absent in the row
   [FAIL (All (r #:row (init x))
           ((Class #:row-var r (init y)) -> (Class #:row-var r)))]
   [FAIL (All (r #:row (init x y z) (field f) m n)
           ((Class #:row-var r a b c) -> (Class #:row-var r)))]
   ))

;; FIXME - add tests for parse-values-type, parse-tc-results
