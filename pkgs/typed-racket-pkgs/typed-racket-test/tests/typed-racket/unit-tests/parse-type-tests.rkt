#lang racket/base
(require "test-utils.rkt"
         "evaluator.rkt"
         (for-syntax
           racket/base
           racket/dict
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
         (only-in racket/class init init-field field)

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
   [(Class) (make-Class #f null null null)]
   [(Class (init [x Number] [y Number]))
    (make-Class #f `((x ,-Number #f) (y ,-Number #f)) null null)]
   [(Class (init [x Number] [y Number #:optional]))
    (make-Class #f `((x ,-Number #f) (y ,-Number #t)) null null)]
   [(Class (init [x Number]) (init-field [y Number]))
    (make-Class #f `((x ,-Number #f) (y ,-Number #f)) `((y ,-Number))
                null)]
   [(Class [m (Number -> Number)])
    (make-Class #f null null `((m ,(t:-> N N))))]
   [(Class [m (Number -> Number)] (init [x Number]))
    (make-Class #f `((x ,-Number #f)) null `((m ,(t:-> N N))))]
   [(Class [m (Number -> Number)] (field [x Number]))
    (make-Class #f null `((x ,-Number)) `((m ,(t:-> N N))))]
   [FAIL (Class foobar)]
   [FAIL (Class [x UNBOUND])]
   [FAIL (Class [x Number #:random-keyword])]
   [FAIL (Class (random-clause [x Number]))]
   ;; test duplicates
   [FAIL (Class [x Number] [x Number])]
   [FAIL (Class (init [x Number]) (init [x Number]))]
   [FAIL (Class (init [x Number]) (init-field [x Number]))]
   [FAIL (Class (field [x Number]) (init-field [x Number]))]
   ;; test #:self
   [(Class #:self This% [m ((Instance This%) -> Number)])
    (-mu This%
      (make-Class
       #f null null `((m ,(t:-> (make-Instance This%) N)))))]
   ;; test #:extends
   [(Class #:extends (Class [m (Number -> Number)]) (field [x Number]))
    (make-Class #f null `((x ,-Number)) `((m ,(t:-> N N))))]
   [(Class #:extends (Class [m (Number -> Number)])
           #:extends (Class [n (Number -> Number)])
           (field [x Number]))
    (make-Class #f null `((x ,-Number)) `((n ,(t:-> N N)) (m ,(t:-> N N))))]
   [(Class #:extends (Class [m (Number -> Number)])
           #:extends (Class [m (Number -> Number)])
           (field [x Number]))
    (make-Class #f null `((x ,-Number)) `((m ,(t:-> N N))))]
   [(Class #:extends (Class (init [x Integer]) [m (Number -> Number)])
           (field [x Number]))
    (make-Class #f null `((x ,-Number)) `((m ,(t:-> N N))))]
   [FAIL (Class #:extends Number)]
   [FAIL (Class #:extends Number [m (Number -> Number)])]
   [FAIL (Class #:extends (Class [m (Number -> Number)]) [m String])]
   [FAIL (Class #:extends (Class [m (Number -> Number)])
                #:extends (Class [m (String -> String)])
                (field [x Number]))]
   ;; Test Object types
   [(Object) (make-Instance (make-Class #f null null null))]
   [(Object [m (Number -> Number)])
    (make-Instance (make-Class #f null null `((m ,(t:-> N N)))))]
   [(Object [m (Number -> Number)] (field [f Number]))
    (make-Instance (make-Class #f null `((f ,N)) `((m ,(t:-> N N)))))]
   [FAIL (Object foobar)]
   [FAIL (Object [x UNBOUND])]
   [FAIL (Object [x Number #:random-keyword])]
   [FAIL (Object (random-clause [x Number]))]
   [FAIL (Object [x Number] [x Number])]
   [FAIL (Object (field [x Number]) (field [x Number]))]
   [FAIL (Object [x Number] [x Number])]
   ))

;; FIXME - add tests for parse-values-type, parse-tc-results
