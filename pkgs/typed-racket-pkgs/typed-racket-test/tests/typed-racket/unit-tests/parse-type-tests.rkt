#lang racket/base
(require "test-utils.rkt"
         "evaluator.rkt"
         (for-syntax
           racket/base
           racket/dict
           (env tvar-env type-alias-env)
           (utils tc-utils)
           (private parse-type)
           (rep type-rep)

           (submod typed-racket/base-env/base-types initialize)
           (rename-in (types union abbrev numeric-tower)
                      [Un t:Un] [-> t:->] [->* t:->*]))
         (only-in typed-racket/typed-racket do-standard-inits)
         (base-env base-types base-types-extra colon)

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
         (unless
           (phase1-phase0-eval
             (parameterize ([current-tvars tvar-env]
                            [delay-errors? #f])
                #`#,(type-equal? (parse-type (quote-syntax ty-stx)) ty-val)))
           (fail-check "Unequal types"))))]))

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
   [(Number -> Number) (t:-> N N)]
   [(All (A) Number -> Number) (-poly (a) (t:-> N N))]
   [(All (A) (Number -> Number)) (-poly (a) (t:-> N N))]
   [(All (A) A -> A) (-poly (a) (t:-> a a))]
   [(All (A) A → A) (-poly (a) (t:-> a a))]
   [(All (A) (A -> A)) (-poly (a) (t:-> a a))]
   ;; requires transformer time stuff that doesn't work
   #;[(Refinement even?) (make-Refinement #'even?)]
   [(Number Number Number Boolean -> Number) (N N N B . t:-> . N)]
   [(Number Number Number * -> Boolean) ((list N N) N . t:->* . B)]
   ;[((. Number) -> Number) (->* (list) N N)] ;; not legal syntax
   [(U Number Boolean) (t:Un N B)]
   [(U Number Boolean Number) (t:Un N B)]
   [(U Number Boolean 1) (t:Un N B)]
   [(All (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(All (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(∀ (a) (Listof a)) (-poly (a) (make-Listof  a))]
   [(∀ (a ...) (a ... a -> Integer)) (-polydots (a) ( (list) (a a) . ->... . -Integer))]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]
   [(All (a ...) (-> (values a ...)))
    (-polydots (a) (t:-> (make-ValuesDots (list) a 'a)))]
   [(case-lambda (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                      [(N N) N])]
   [(case-> (Number -> Boolean) (Number Number -> Number)) (cl-> [(N) B]
                                                                 [(N N) N])]
   [1 (-val 1)]
   [#t (-val #t)]
   [#f (-val #f)]
   ["foo" (-val "foo")]
   ['(1 2 3) (-Tuple (map -val '(1 2 3)))]

   [(Listof Number) (make-Listof  N)]

   [a (-v a) (dict-set initial-tvar-env 'a (-v a))]
   [(All (a ...) (a ... -> Number))
    (-polydots (a) ((list) [a a] . ->... . N))]

   [(Any -> Boolean : Number) (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (Number @ 0) #:- (! Number @ 0))
    (make-pred-ty -Number)]
   [(Any -> Boolean : #:+ (! Number @ 0) #:- (Number @ 0))
    (t:->* (list Univ) -Boolean : (-FS (-not-filter -Number 0 null) (-filter -Number 0 null)))]
   [(Number -> Number -> Number)
    (t:-> -Number (t:-> -Number -Number))]
   [(Integer -> (All (X) (X -> X)))
    (t:-> -Integer (-poly (x) (t:-> x x)))]

   [(Opaque foo?) (make-Opaque #'foo?)]
   ;; PR 14122
   [FAIL (Opaque 3)]

   ;; struct types
   [(Struct-Type arity-at-least) (make-StructType (resolve -Arity-At-Least))]
   [FAIL (Struct-Type Integer)]
   [FAIL (Struct-Type foo)]
   [Struct-TypeTop (make-StructTypeTop)]
   ))

;; FIXME - add tests for parse-values-type, parse-tc-results
