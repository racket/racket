#lang racket/base

(module cross-phase-failure racket/base
  (require
    rackunit
    racket/contract)

  (provide
    (contract-out
      [rename cross-phase-failure* cross-phase-failure
        (->* (string?) (#:actual any/c #:expected any/c) cross-phase-failure?)]
      [cross-phase-failure? predicate/c]
      [cross-phase-failure-message (-> cross-phase-failure? string?)]
      [rename cross-phase-failure-check-infos* cross-phase-failure-check-infos
        (-> cross-phase-failure? (listof check-info?))]))

  (struct cross-phase-failure (message check-infos) #:prefab)

  (define no-arg (gensym 'no-arg))

  (define (cross-phase-failure* message #:actual [actual no-arg] #:expected [expected no-arg])
    (cross-phase-failure
      message
      (append
        (if (eq? actual no-arg) null (list (list 'actual actual)))
        (if (eq? expected no-arg) null (list (list 'expected expected))))))

  (define (cross-phase-failure-check-infos* cpf)
    (map (λ (args) (apply check-info args)) (cross-phase-failure-check-infos cpf))))

;; Functions for testing correct behavior of typechecking
(module tester racket/base
  (require
    (submod ".." cross-phase-failure)
    typed-racket/utils/utils
    racket/base racket/match
    (types tc-result printer)
    syntax/parse
    (for-template (only-in typed-racket/typed-racket do-standard-inits))
    (typecheck typechecker check-below)
    (utils mutated-vars tc-utils)
    (env mvar-env))
  (provide
    test-literal test-literal/fail
    test test/proc test/fail)


  (do-standard-inits)
  (print-complex-filters? #t)
  
  ;; tr-expand: syntax? -> syntax?
  ;; Expands out a form and annotates it with necesarry TR machinery.
  (define (tr-expand stx)
    (define expanded-stx (local-expand stx 'expression '()))
    (find-mutated-vars expanded-stx mvar-env)
    expanded-stx)


  ;; tc: syntax? (option/c tc-results?) -> tc-results?
  ;; Typechecks the expression using the expected information if provided.
  (define (tc expr expected)
    (if expected
        (tc-expr/check expr expected)
        (tc-expr expr)))


  (define (check-tc-results result golden #:name name)
    (unless (equal? golden result)
      (define base-message (format "~a did not return the expected value." name))

      (define extra-message1
        (if (parameterize ([delay-errors? #f])
              (with-handlers ([exn:fail? (lambda (_) #f)])
                (check-below result golden)
                #t))
            " It returned a more precise value."
            ""))

      (define extra-message2
        (match* (result golden)
          [((tc-result1: rt rf ro) (tc-result1: gt gf go))
           (cond
             [(not (equal? rt gt))
              " The types don't match."]
             [(not (equal? rf gf))
              " The filters don't match."]
             [(not (equal? ro go))
              " They objects don't match."])]
          [(_ _) ""]))

      (raise (cross-phase-failure
               #:actual result
               #:expected golden
               (string-append base-message extra-message1 extra-message2)))))

  ;; test: syntax? tc-results? [(option/c tc-results?)] -> void?
  ;; Checks that the expression typechecks using the expected type to the golden result.
  (define (test expr golden (expected #f))
    (test/proc expr (lambda (_) golden) expected))

  ;; test/proc: syntax? (syntax? -> tc-results?) [(option/c tc-results?)] -> void?
  ;; Checks that the expression typechecks to golden result. The golden result is computed by applying
  ;; the golden function to the expanded syntax of the expression.
  (define (test/proc expr golden-fun (expected #f))
    (define expanded-expr (tr-expand expr))
    (define result (tc expanded-expr expected))
    (define golden (golden-fun expanded-expr))
    (check-tc-results result golden #:name "tc-expr"))


  ;; test/fail syntax? tc-results? (or/c string? regexp?) (option/c tc-results?) -> void?
  ;; Checks that the expression doesn't typecheck using the expected type, returns the golden type,
  ;; and raises an error message matching the golden message
  (define (test/fail code golden message expected)
    (dynamic-wind
      void
      (λ ()
        (with-handlers ([exn:fail:syntax?
                         (lambda (exn)
                           (when message
                             (unless (regexp-match? message (exn-message exn))
                               (raise (cross-phase-failure
                                        #:actual (exn-message exn)
                                        #:expected message
                                        "tc-expr raised the wrong error message")))))])
          (define result
            (parameterize ([delay-errors? #t])
              (tc (tr-expand code) expected)))
          (check-tc-results result golden #:name "tc-expr")
          (report-first-error)
          (raise (cross-phase-failure
                   #:actual result
                   "tc-expr did not raise an error"))))
      (λ () (reset-errors!))))


  ;; test-literal syntax? tc-results? (option/c tc-results?) -> void?
  ;; Checks that the literal typechecks using the expected type to the golden result.
  (define (test-literal literal golden expected)
    (define result (tc-literal literal expected))
    (check-tc-results result golden #:name "tc-literal"))

  ;; test-literal/fail syntax? (or/c string? regexp?) (option/c tc-results?) -> void?
  ;; Checks that the literal doesn't typecheck using the expected type and the golden message
  (define (test-literal/fail literal message expected)
    (with-handlers ([exn:fail:syntax?
                     (lambda (exn)
                       (unless (regexp-match? message (exn-message exn))
                         (raise (cross-phase-failure
                                  #:actual (exn-message exn)
                                  #:expected message
                                  "tc-literal raised the wrong error message"))))])
      (define result (tc-literal literal expected))
      (raise (cross-phase-failure
               #:actual result
               "tc-literal did not raise an error")))))


(require
  'cross-phase-failure
  "evaluator.rkt"
  (except-in "test-utils.rkt" private)
  syntax/location syntax/srcloc
  (for-syntax
    racket/base
    syntax/parse
    'tester))

(provide tests)
(gen-test-main)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helpers for actual checks

(module+ test-helpers
  (provide tc-e
           tc-e/t
           tc-err
           tc-l
           tc-l/err))

(begin-for-syntax
  (define-splicing-syntax-class return
    (pattern ty:expr #:attr v #'(ret ty))
    (pattern (~seq #:ret r:expr) #:attr v #'r))

  (define-splicing-syntax-class err-return
    (pattern (~seq #:ret r:expr) #:attr v #'r)
    (pattern (~seq) #:attr v #'(ret -Bottom)))

  (define-splicing-syntax-class expected
    (pattern (~seq #:expected v:expr))
    (pattern (~seq) #:attr v #'#f))

  ;; for specifying the error message in a test
  (define-splicing-syntax-class expected-msg
    (pattern (~seq #:msg v:expr))
    (pattern (~seq) #:attr v #'#f)))

(define-syntax (test-phase1 stx)
  (syntax-parse stx
    ([_ name:expr body:expr ...]
     (quasisyntax/loc stx
       (test-case (format "~a ~a" (quote-line-number name) 'name)
         (with-check-info (['location (build-source-location-list (quote-srcloc #,stx))])
           (with-handlers ([cross-phase-failure?
                            (λ (tf)
                              (with-check-info*
                                (cross-phase-failure-check-infos tf)
                                (lambda ()
                                  (fail-check (cross-phase-failure-message tf)))))])
             (phase1-eval body ...))))))))


;;Constructs the syntax that calls eval and returns the answer to the user
(define-syntax (tc-e stx)
  (syntax-parse stx
    [(_ code:expr #:proc p)
     (quasisyntax/loc stx
       (test-phase1 code
         (test/proc (quote-syntax code) p)))]
    [(_ code:expr return:return ex:expected)
     (quasisyntax/loc stx
       (test-phase1 code
         (test (quote-syntax code) return.v ex.v)))]))

(define-syntax (tc-e/t stx)
  (syntax-parse stx
    [(_ e t) (syntax/loc stx (tc-e e #:ret (ret t -true-filter)))]))

;; check that a literal typechecks correctly
(define-syntax (tc-l stx)
  (syntax-parse stx
    [(_ lit ty exp:expected)
     (quasisyntax/loc stx
       (test-phase1 #,(syntax/loc #'lit (LITERAL lit))
         (test-literal (quote-syntax lit) ty exp.v)))]))


;; check that typechecking this expression fails
(define-syntax (tc-err stx)
  (syntax-parse stx
    [(_ code:expr ret:err-return ex:expected msg:expected-msg)
     (quasisyntax/loc stx
        (test-phase1 #,(syntax/loc #'code (FAIL code))
           (test/fail (quote-syntax code) ret.v msg.v ex.v)))]))

(define-syntax (tc-l/err stx)
  (syntax-parse stx
    [(_ lit:expr ex:expected msg:expected-msg)
     (quasisyntax/loc stx
       (test-phase1 #,(syntax/loc #'lit (LITERAL/FAIL lit))
         (test-literal/fail (quote-syntax lit) msg.v ex.v)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require
  rackunit

  ;; Needed for bindings of identifiers in expressions
  racket/bool
  (except-in racket/class class)
  racket/file
  racket/fixnum
  racket/flonum
  racket/extflonum
  racket/function
  racket/future
  racket/list
  racket/math
  racket/path
  racket/place
  racket/port
  racket/sequence
  racket/set
  racket/string
  racket/system
  racket/tcp
  racket/udp
  racket/vector
  syntax/stx

  (except-in typed-racket/utils/utils private)
  ;; Needed for bindings of types and TR primitives in expressions
  (except-in (base-env extra-procs prims base-types base-types-extra)
    define lambda λ case-lambda)
  ;; For tests that rely on kw/opt properties
  (prefix-in tr: (only-in (base-env prims) define lambda λ case-lambda))
  ;; Needed for the `let-name` syntax class before
  (prefix-in r: (only-in racket/base let-values))
  ;; Needed for constructing TR types in expected values
  (for-syntax
    (rep type-rep filter-rep object-rep)
    (base-env base-structs)
    (rename-in (types abbrev union numeric-tower filter-ops utils resolve)
               [Un t:Un]
               [-> t:->])))

(begin-for-syntax
  ;; More tests need to be written to use these macros.
  (define-syntax-class (let-name n)
    #:literals (r:let-values)
    (pattern (r:let-values ([(i:id) _] ...) . _)
             #:with x (list-ref (syntax->list #'(i ...)) n)))

  (define-syntax-rule (get-let-name id n e)
    (syntax-parser
     [p #:declare p (let-name n)
        #:with id #'p.x
        e]))

  (define (-path t var)
    (ret t
         (-FS (-not-filter (-val #f) var)
              (-filter (-val #f) var))
         (make-Path null var))))


(define tests
  (test-suite
    "Typechecker tests"
    #reader typed-racket/typed-reader
      (test-suite
        "tc-expr tests"

        [tc-e
         (let: ([x : (U Number (cons Number Number)) (cons 3 4)])
               (if (pair? x)
                   (+ 1 (car x))
                   5))
         -Number]
        (tc-e/t 0 -Zero)
        (tc-e/t 1 -One)
        (tc-e/t (if (let ([y 12]) y) 3 4) -PosByte)
        (tc-e/t 2 -PosByte)
        (tc-e/t 3 -PosByte)
        (tc-e/t 100 -PosByte)
        (tc-e/t 255 -PosByte)
        (tc-e/t 256 -PosIndex)
        (tc-e/t -1 -NegFixnum)
        (tc-e/t -100 -NegFixnum)
        (tc-e/t 1000 -PosIndex)
        (tc-e/t 268435455 -PosIndex)
        (tc-e/t -268435456 -NegFixnum)
        (tc-e/t 268435456 -PosFixnum)
        (tc-e/t -268435457 -NegFixnum)
        (tc-e/t 1073741823 -PosFixnum)
        (tc-e/t -1073741824 -NegFixnum)
        (tc-e/t 1073741824 -PosInt)
        (tc-e/t -1073741825 -NegInt)
        (tc-e/t "foo" -String)
        (tc-e (+ 3 4) -PosIndex)
        (tc-e (- 1) -NegFixnum)
        (tc-e (- 1073741823) -NegFixnum)
        (tc-e (- -4) -PosInt)
        (tc-e (- (ann -5 Nonpositive-Fixnum)) -Nat)
        (tc-e/t 1152921504606846975 -PosInt)
        (tc-e/t -1152921504606846975 -NegInt)
        (tc-e (- 3253463567262345623) -NegInt)
        (tc-e (- -23524623547234734568) -PosInt)
        (tc-e (- 241.3) -NegFlonum)
        (tc-e (- -24.3) -PosFlonum)
        (tc-e/t 34.2f0 -PosSingleFlonum)
        (tc-e/t -34.2f0 -NegSingleFlonum)

        (tc-e (- (ann 1000 Index) 1) -Fixnum)
        (tc-e (- (ann 1000 Positive-Index) 1) -Index)
        (tc-e (- (ann 1000 Fixnum) 1) -Int)
        (tc-e (- (ann 1000 Nonnegative-Fixnum) 1) -Fixnum)
        (tc-e (- (ann 1000 Positive-Fixnum) 1) -NonNegFixnum)
        (tc-e (- (ann 1000 Exact-Positive-Integer) 1) -Nat)

        (tc-e (fx- (ann 1000 Index) 1) -Fixnum)
        (tc-e (fx- (ann 1000 Positive-Index) 1) -Index)
        (tc-e (fx- (ann 1000 Fixnum) 1) -Fixnum)
        (tc-e (fx- (ann 1000 Nonnegative-Fixnum) 1) -Fixnum)
        (tc-e (fx- (ann 1000 Positive-Fixnum) 1) -NonNegFixnum)
        (tc-e (fx- (ann 1000 Exact-Positive-Integer) 1) -NonNegFixnum)


        (tc-e (*) -One)

        (tc-e (gcd 1/2) -PosRat)
        (tc-e (gcd 3 1/2) -PosRat)
        (tc-e (gcd (ann 3 Integer) 1/2) -NonNegRat)
        (tc-e (gcd (ann 3 Integer) -1/2) -NonNegRat)
        (tc-e (lcm 1/2) -PosRat)
        (tc-e (lcm 3 1/2) -PosRat)
        (tc-e (lcm (ann 3 Integer) 1/2) -NonNegRat)
        (tc-e (lcm (ann 3 Integer) -1/2) -NonNegRat)
        (tc-e (expt 0.5 0.3) -PosFlonum)
        (tc-e (expt 0.5 2) -PosFlonum)
        (tc-e (expt 0.5 0) -One)
        (tc-e (expt -1/2 -1/2) -Number)
        (tc-e (expt (ann 0.5 Float) (ann 2 Integer)) (t:Un -Flonum -One))
        (tc-e (expt (ann 0.5f0 Single-Flonum) (ann 2 Integer)) (t:Un -SingleFlonum -One))
        (tc-e (flexpt 0.5 0.3) -NonNegFlonum)
        (tc-e (flexpt 0.00000000001 100000000000.0) -NonNegFlonum)
        (tc-e (flexpt -2.0 -0.5) -Flonum) ; NaN
        (tc-e (extflexpt 0.5t0 0.3t0) -NonNegExtFlonum)
        (tc-e (extflexpt 0.00000000001t0 100000000000.0t0) -NonNegExtFlonum)
        (tc-e (extflexpt -2.0t0 -0.5t0) -ExtFlonum) ; NaN
        (tc-e (tanh (ann 0 Nonnegative-Integer)) -NonNegReal)
        (tc-e (sinh (ann 0 Nonpositive-Integer)) -NonPosReal)
        (tc-e (angle -1) (t:Un -InexactReal -Zero))
        (tc-e (angle 2.3) -Zero)
        (tc-e (magnitude 3/4) -PosRat)
        (tc-e (magnitude 3+2i) -NonNegReal)
        (tc-e (min (ann 3 Fixnum) (ann 3 Fixnum)) -Fixnum)
        (tc-e (min (ann -2 Negative-Fixnum) (ann 3 Fixnum)) -NegFixnum)
        (tc-e (min (ann 3 Fixnum) (ann -2 Negative-Fixnum)) -NegFixnum)
        (tc-e (exact->inexact (ann 3 Number)) (t:Un -InexactReal -InexactComplex))
        (tc-e (real->double-flonum #e1e-500) -NonNegFlonum)
        (tc-e (real->double-flonum #e-1e-500) -NonPosFlonum)
        (tc-e (real->single-flonum #e1e-500) -NonNegSingleFlonum)
        (tc-e (real->single-flonum #e-1e-500) -NonPosSingleFlonum)
        (tc-e (real->single-flonum 1e-300) -NonNegSingleFlonum)
        (tc-e (real->single-flonum -1e-300) -NonPosSingleFlonum)
        (tc-e (extfl->inexact 1t-500) -NonNegFlonum)
        (tc-e (extfl->inexact -1t-500) -NonPosFlonum)
        (tc-e (real->extfl #e1e-8192) -NonNegExtFlonum)
        (tc-e (real->extfl #e-1e-8192) -NonPosExtFlonum)
        (tc-err (let: ([z : 10000000000000 10000000000000]) z)) ; unsafe
        (tc-err (let: ([z : -4611686018427387904 -4611686018427387904]) z)) ; unsafe
        (tc-e/t (let: ([z : -4611686018427387905 -4611686018427387905]) z)
                (-val -4611686018427387905))
        (tc-err (let: ([z : -1073741825 -1073741825]) z)) ; unsafe
        (tc-e/t (let: ([z : -1073741824 -1073741824]) z) (-val -1073741824))
        (tc-e/t (let: ([z : 268435455 268435455]) z) (-val 268435455))
        (tc-err (let: ([z : 268435456 268435456]) z)) ; unsafe
        (tc-err (let: ([z : 4611686018427387903 4611686018427387903]) z)) ; unsafe
        (tc-e/t (let: ([z : 4611686018427387904 4611686018427387904]) z) (-val 4611686018427387904))

        [tc-e/t (lambda: () 3) (t:-> -PosByte : -true-filter)]
        [tc-e/t (lambda: ([x : Number]) 3) (t:-> -Number -PosByte : -true-filter)]
        [tc-e/t (lambda: ([x : Number] [y : Boolean]) 3)
                (t:-> -Number -Boolean -PosByte : -true-filter)]
        [tc-e/t (lambda () 3) (t:-> -PosByte : -true-filter)]
        [tc-e (values 3 4) #:ret (ret (list -PosByte -PosByte) (list -true-filter -true-filter))]
        [tc-e (cons 3 4) (-pair -PosByte -PosByte)]
        [tc-e (cons 3 (ann '() : (Listof Integer))) (make-Listof -Integer)]
        [tc-e (void) -Void]
        [tc-e (void 3 4) -Void]
        [tc-e (void #t #f '(1 2 3)) -Void]
        [tc-e/t #() (make-HeterogeneousVector (list))]
        [tc-err #(3)
           #:ret (ret (make-HeterogeneousVector (list -Integer -Integer)))
           #:expected (ret (make-HeterogeneousVector (list -Integer -Integer)))]
        [tc-err #(3 4 5)
           #:ret (ret (make-HeterogeneousVector (list -Integer -Integer)))
           #:expected (ret (make-HeterogeneousVector (list -Integer -Integer)))]
        [tc-e/t #(3 4 5) (make-HeterogeneousVector (list -Integer -Integer -Integer))]
        [tc-e/t '(2 3 4) (-lst* -PosByte -PosByte -PosByte)]
        [tc-e/t '(2 3 #t) (-lst* -PosByte -PosByte (-val #t))]
        [tc-e/t #(2 3 #t) (make-HeterogeneousVector (list -Integer -Integer -Boolean))]
        [tc-e (vector 2 "3" #t) (make-HeterogeneousVector (list -Integer -String -Boolean))]
        [tc-e (vector) (make-HeterogeneousVector (list))]
        [tc-e (vector) #:ret (tc-any-results -top) #:expected (tc-any-results -no-filter)]
        [tc-err (vector)
           #:ret (ret -Integer)
           #:expected (ret -Integer)]
        [tc-e (vector-immutable 2 "3" #t) (make-HeterogeneousVector (list -Integer -String -Boolean))]
        [tc-e (make-vector 4 1) (-vec -Integer)]
        [tc-e (build-vector 4 (lambda (x) 1)) (-vec -Integer)]
        [tc-e (range 0) -Null]
        [tc-e (range 1) (-lst* -Zero)]
        [tc-e (range 4) (-lst -Byte)]
        [tc-e (range (ann 10 : Index)) (-lst -Index)]
        [tc-e (range (ann 10 : Fixnum)) (-lst -NonNegFixnum)]
        [tc-e (range (ann 10.0 : Real)) (-lst -Nat)]
        [tc-e (range 2 4 1) (-lst -PosByte)]
        [tc-e (range 0 4 1) (-lst -Byte)]
        [tc-e (range 0.0 4/2 0.5) (-lst -Flonum)]
        [tc-e/t '(#t #f) (-lst* (-val #t) (-val #f))]
        [tc-e/t (plambda: (a) ([l : (Listof a)]) (car l))
                (make-Poly '(a) (t:-> (make-Listof (-v a)) (-v a)))]
        [tc-e/t (plambda: (a) ([l : (Listof a)]) (car l))
                (make-Poly '(a) (t:-> (make-Listof (-v a)) (-v a)))]
        [tc-e/t (case-lambda: [([a : Number] [b : Number]) (+ a b)]) (t:-> -Number -Number -Number)]
        [tc-e/t (tr:case-lambda [([a : Number] [b : Number]) (+ a b)])
                (t:-> -Number -Number -Number)]
        [tc-e/t (let: ([x : Number 5]) x) -Number]
        [tc-e (let-values ([(x) 4]) (+ x 1)) -PosIndex]
        [tc-e (let-values ([(x y) (values 3 #t)]) (and (= x 1) (not y)))
              #:ret (ret -Boolean -false-filter)]
        [tc-e/t (values 3) -PosByte]
        [tc-e (values) #:ret (ret null)]
        [tc-e (values 3 #f) #:ret (ret (list -PosByte (-val #f)) (list -true-filter -false-filter))]
        [tc-e (map #{values @ Symbol} '(a b c)) (-pair -Symbol (make-Listof -Symbol))]
        [tc-e (andmap add1 (ann '() (Listof Number))) (t:Un (-val #t) -Number)]
        [tc-e (ormap add1 (ann '() (Listof Number))) (t:Un (-val #f) -Number)]
        [tc-e (letrec: ([fact : (Number -> Number)
                              (lambda: ([n : Number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                       (fact 20))
              -Number]
        [tc-e (let: fact : Number ([n : Number 20])
                    (if (zero? n) 1 (* n (fact (- n 1)))))
              -Number]
        [tc-e (let: ([v : Any 5])
                    (if (number? v) (+ v 1) 3))
              -Number]
        [tc-e (let: ([v : Any #f])
                    (if (number? v) (+ v 1) 3))
              -Number]
        [tc-e (let: ([v : (Un Number Boolean) #f])
                    (if (boolean? v) 5 (+ v 1)))
              -Number]
        [tc-e (let: ([f : (Number Number -> Number) +]) (f 3 4)) -Number]
        [tc-e (let: ([+ : (Boolean -> Number) (lambda: ([x : Boolean]) 3)]) (+ #f)) -Number]
        [tc-e (when #f #t) -Void]
        [tc-e (when (number? #f) (+ 4 5)) -Void]
        [tc-e (let: ([x : (Un #f Number) 7])
                    (if x (+ x 1) 3))
              -Number]
        [tc-e (let: ([x : Number 1])
                    (if (and (number? x) #t)
                        (+ x 4)
                        'bc))
              -Number]
        [tc-e/t (let: ((x : Number 3)) (if (boolean? x) (not x) #t)) (-val #t)]
        [tc-e/t (begin 3) -PosByte]
        [tc-e/t (begin #f 3) -PosByte]
        [tc-e/t (begin #t) (-val #t)]
        [tc-e/t (begin0 #t) (-val #t)]
        [tc-e/t (begin0 #t 3) (-val #t)]
        [tc-e/t #t (-val #t)]
        [tc-e #f #:ret (ret (-val #f) -false-filter)]
        [tc-e/t '#t (-val #t)]
        [tc-e '#f #:ret (ret (-val #f) -false-filter)]
        [tc-e/t (if #f 'a 3) -PosByte]
        [tc-e/t (if #f #f #t) (t:Un (-val #t))]
        [tc-e (when #f 3) -Void]
        [tc-e/t '() -Null]
        [tc-e/t (let: ([x : (Listof Number) '(1)])
                      (cond [(pair? x) 1]
                            [(null? x) 1]))
              -One]
        [tc-e/t (lambda: ([x : Number] . [y : Number *]) (car y))
                (->* (list -Number) -Number -Number)]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3) -Number]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3 4 5) -Number]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3 4) -Number]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '(4)) -Number]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '(4 6 7)) -Number]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '()) -Number]

        [tc-e/t (lambda: ([x : Number] . [y : Boolean *]) (car y))
                (->* (list -Number) -Boolean -Boolean)]
        [tc-e ((lambda: ([x : Number] . [y : Boolean *]) (car y)) 3) -Boolean]
        [tc-e (apply (lambda: ([x : Number] . [y : Boolean *]) (car y)) 3 '(#f)) -Boolean]
        [tc-e (lambda args (void)) #:ret (ret (t:-> -String -Void) -true-filter)
                                   #:expected (ret (t:-> -String -Void) -true-filter)]
        [tc-e (lambda (x y . z)
                (+ x y (+ (length z))))
              #:ret (ret (t:-> -Byte -Index -Number) -true-filter)
              #:expected (ret (t:-> -Byte -Index -Number) -true-filter)]

        [tc-e/t (let: ([x : Number 3])
                      (when (number? x) #t))
              (-val #t)]
        [tc-e (let: ([x : Number 3])
                    (when (boolean? x) #t))
              -Void]

        [tc-e (integer-bytes->integer '#"abcd" #t) -Nat]
        [tc-e (integer-bytes->integer '#"abcd" #f) -Int]

        [tc-e/t (let: ([x : Any 3])
                    (if (list? x)
                        (begin (car x) 1)
                        2))
              -PosByte]


        [tc-e (let: ([x : (U Number Boolean) 3])
                    (if (not (boolean? x))
                        (add1 x)
                        3))
              -Number]

        [tc-e/t (let ([x 1]) x) -One]
        [tc-e (let ([x 1]) (boolean? x)) #:ret (ret -Boolean -false-filter)]
        [tc-e (boolean? number?) #:ret (ret -Boolean -false-filter)]

        [tc-e (let: ([x : (Option Number) #f]) x) (t:Un -Number (-val #f))]
        [tc-e (let: ([x : Any 12]) (not (not x))) -Boolean]

        [tc-e (let: ([x : (Option Number) #f])
                    (if (let ([z 1]) x)
                        (add1 x)
                        12))
              -Number]
        [tc-err (5 4) #:msg "Cannot apply expression of type"]
        [tc-err (apply 5 '(2))]
        [tc-err (map (lambda: ([x : Any] [y : Any]) 1) '(1))]
        [tc-e (map add1 '(1)) (-pair -PosByte (-lst -PosByte))]

        [tc-e/t (let ([x 5])
                (if (eq? x 1)
                    12
                    14))
              -PosByte]

        [tc-e (car (append (list 1 2) (list 3 4))) -PosByte]

        [tc-e
         (let-syntax ([a
                       (syntax-rules ()
                         [(_ e) (let ([v 1]) e)])])
           (let: ([v : String "a"])
                 (string-append "foo" (a v))))
         -String]
        [tc-e (string-join '("hello" "world") " ") -String]
        [tc-e (string-join '("hello" "world")) -String]
        [tc-e (string-join '("hello" "world") #:before-first "a") -String]
        [tc-e (add-between '(1 2 3) 0) (-lst -Byte)]
        [tc-e (add-between '(1 2 3) 'a) (-lst (t:Un -PosByte (-val 'a)))]
        [tc-e ((inst add-between Positive-Byte Symbol) '(1 2 3) 'a #:splice? #t #:before-first '(b))
              (-lst (t:Un -PosByte -Symbol))]

        [tc-e (apply (plambda: (a) [x : a *] x) '(5)) (-lst -PosByte)]
        [tc-e (apply append (list '(1 2 3) '(4 5 6))) (-lst -PosByte)]

        [tc-err ((case-lambda: [([x : Number]) x]
                               [([y : Number] [x : Number]) x])
                 1 2 3)]
        [tc-err ((case-lambda: [([x : Number]) x]
                               [([y : Number] [x : Number]) x])
                 1 'foo)]

        [tc-err (apply
                 (case-lambda: [([x : Number]) x]
                               [([y : Number] [x : Number]) x])
                 '(1 2 3))]
        [tc-err (apply
                 (case-lambda: [([x : Number]) x]
                               [([y : Number] [x : Number]) x])
                 '(1 foo))]
        [tc-err ((tr:case-lambda [([x : Number]) x]
                                 [([y : Number] [x : Number]) x])
                 1 2 3)]
        [tc-err ((tr:case-lambda [([x : Number]) x]
                                 [([y : Number] [x : Number]) x])
                 1 'foo)]

        [tc-err (apply
                 (tr:case-lambda [([x : Number]) x]
                                 [([y : Number] [x : Number]) x])
                 '(1 2 3))]
        [tc-err (apply
                 (tr:case-lambda [([x : Number]) x]
                                 [([y : Number] [x : Number]) x])
                 '(1 foo))]

        [tc-e (let: ([x : Any #f])
                    (if (number? (let ([z 1]) x))
                        (add1 x)
                        12))
              -Number]

        [tc-e (let: ([x : (Option Number) #f])
                    (if x
                        (add1 x)
                        12))
              -Number]


        [tc-e null #:ret (ret (-val null) -true-filter (-id-path #'null))]

        [tc-e/t (let* ([sym 'squarf]
                       [x (if (= 1 2) 3 sym)])
                  x)
                (t:Un (-val 'squarf) -PosByte)]

        [tc-e/t (if #t 1 2) -One]


        ;; eq? as predicate
        [tc-e/t (let: ([x : (Un 'foo Number) 'foo])
                      (if (eq? x 'foo) 3 x))
                -Number]
        [tc-e/t (let: ([x : (Un 'foo Number) 'foo])
                      (if (eq? 'foo x) 3 x))
                -Number]

        [tc-err (let: ([x : (U String 'foo) 'foo])
                      (if (string=? x 'foo)
                          "foo"
                          x))
          #:ret (ret (t:Un -String (-val 'foo)) -true-filter)]

        [tc-e/t (let: ([x : (U String 5) 5])
                        (if (eq? x 5)
                            "foo"
                            x))
                  (t:Un -String (-val 5))]

        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? x sym) 3 x))
              #:ret (ret -PosByte -true-filter)]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? sym x) 3 x))
              #:ret (ret -PosByte -true-filter)]
        ;; equal? as predicate for symbols
        [tc-e/t (let: ([x : (Un 'foo Number) 'foo])
                      (if (equal? x 'foo) 3 x))
                -Number]
        [tc-e/t (let: ([x : (Un 'foo Number) 'foo])
                      (if (equal? 'foo x) 3 x))
                -Number]

        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? x sym) 3 x))
              #:ret (ret -PosByte -true-filter)]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? sym x) 3 x))
              #:ret (ret -PosByte -true-filter)]

        [tc-e (let: ([x : (Listof Symbol)'(a b c)])
                    (cond [(memq 'a x) => car]
                          [else 'foo]))
              -Symbol]

        [tc-e (list 2 3 4) (-lst* -PosByte -PosByte -PosByte)]
        [tc-e (list 2 3 4 'a) (-lst* -PosByte -PosByte -PosByte (-val 'a))]

        [tc-e `(1 2 ,(+ 3 4)) (-lst* -One -PosByte -PosIndex)]

        [tc-e (let: ([x : Any 1])
                    (when (and (list? x) (not (null? x)))
                      (car x)))
              Univ]

        [tc-err (let: ([x : Any 3])
                      (car x))]
        [tc-err (car 3)]
        [tc-err (map 3 12)]
        [tc-err (car 3)]

        [tc-e/t (let: ([x : Any 1])
                  (if (and (list? x) (not (null? x)))
                      x
                      'foo))
                (t:Un (-val 'foo) (-pair Univ (-lst Univ)))]

        [tc-e (cadr (cadr (list 1 (list 1 2 3) 3))) -PosByte]



        ;;; tests for and
        [tc-e (let: ([x : Any 1]) (and (number? x) (boolean? x)))
              #:ret (ret -Boolean -false-filter)]
        [tc-e (let: ([x : Any 1]) (and (number? x) x))
              (t:Un -Number (-val #f))]
        [tc-e (let: ([x : Any 1]) (and x (boolean? x)))
              -Boolean]

        [tc-e/t (let: ([x : Any 3])
                      (if (and (list? x) (not (null? x)))
                          (begin (car x) 1) 2))
              -PosByte]

        ;; set! tests
        [tc-e (let: ([x : Any 3])
                    (set! x '(1 2 3))
                    (if (number? x) x 2))
              Univ]

        ;; or tests - doesn't do anything good yet

        #| FIXME: This should pass, but the filter is different for some reason
        [tc-e (let: ([x : Any 3])
                    (if (or (boolean? x) (number? x))
                        (if (boolean? x) 12 x)
                        47))
              -Number]
        |#

        ;; test for fake or
        [tc-e (let: ([x : Any 1])
                    (if (if (number? x)
                            #t
                            (boolean? x))
                        (if (boolean? x) 1 (+ 1 x))
                        4))
              -Number]
        ;; these don't invoke the or rule
        [tc-e (let: ([x : Any 1]
                     [y : Any 12])
                    (if (if (number? x)
                            #t
                            (boolean? y))
                        (if (boolean? x) 1 x)
                        4))
              #:ret (ret Univ -true-filter)]
        [tc-e (let: ([x : Any 1])
                    (if (if ((lambda: ([x : Any]) x) 12)
                            #t
                            (boolean? x))
                        (if (boolean? x) 1 x)
                        4))
              #:ret (ret Univ -true-filter)]
        ;; T-AbsPred
        [tc-e/t (let ([p? (lambda: ([x : Any]) (number? x))])
                  (lambda: ([x : Any]) (if (p? x) (add1 x) (add1 12))))
                (t:-> Univ -Number)]
        [tc-e/t (let ([p? (lambda: ([x : Any]) (not (number? x)))])
                  (lambda: ([x : Any]) (if (p? x) 12 (add1 x))))
                (t:-> Univ -Number : (-FS -top (-filter -Number 0)))]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) (number? z))])
                  (lambda: ([x : Any]) (if (p? x) 11 12)))
                (t:-> Univ -PosByte : -true-filter)]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) (number? z))])
                  (lambda: ([x : Any]) (if (p? x) x 12)))
                (t:-> Univ Univ : (-FS  (-not-filter (-val #f) 0) (-filter (-val #f) 0))
                                : (make-Path null '(0 0)))]
        [tc-e/t (let* ([z (ann 1 : Any)]
                       [p? (lambda: ([x : Any]) (not (number? z)))])
                  (lambda: ([x : Any]) (if (p? x) (ann (add1 7) Any) 12)))
                (t:-> Univ Univ)]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) (not (number? z)))])
                  (lambda: ([x : Any]) (if (p? x) x 12)))
                (t:-> Univ -PosByte : -true-filter)]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) z)])
                  (lambda: ([x : Any]) (if (p? x) x 12)))
                (t:-> Univ Univ : (-FS (-not-filter (-val #f) 0) (-filter (-val #f) 0))
                                : (make-Path null '(0 0)))]

        [tc-e (not 1)
          #:ret (ret -Boolean -false-filter)]

        [tc-err ((lambda () 1) 2)
          #:ret (ret (-val 1) -true-filter)]
        [tc-err (apply (lambda () 1) '(2))]
        [tc-err ((lambda: ([x : Any] [y : Any]) 1) 2)
          #:ret (ret (-val 1) -true-filter)]
        [tc-err (map map '(2))]
        [tc-err ((plambda: (a) ([x : (a -> a)] [y : a]) (x y)) 5)]
        [tc-err ((plambda: (a) ([x : a] [y : a]) x) 5)]
        [tc-err (ann 5 : String)
          #:ret (ret -String -true-filter)]

        ;; these don't work because the type annotation gets lost in marshalling
        #|
        [tc-e (letrec-syntaxes+values () ([(#{x : Number}) (values 1)]) (add1 x)) -Number]
        [tc-e (letrec-values ([(#{x : Number}) (values 1)]) (add1 x)) -Number]
        [tc-e (letrec ([#{x : Number} (values 1)]) (add1 x)) -Number]
        |#

        [tc-e (letrec: ([x : Number (values 1)]) (add1 x)) -Number]

        ;; This test case used to test Undefined, but the `x` is now Bottom because
        ;; cyclic definitions can be given *any* type. This is ok since cyclic definitions
        ;; are just errors that TR does not handle (because #<undefined> no longer leaks out
        ;; in such cases).
        [tc-e (let ()
                 (: complicated Boolean)
                 (define complicated #f)
                 (: undefined (Un))
                 (define undefined (letrec: ((x : (Un) x)) x))
                 (letrec: ((x : (Un) (if complicated undefined undefined))
                           (y : (Un) (if complicated x undefined)))
                   y))
          -Bottom]

        ;; This case fails, unlike the last one, since the types in the
        ;; cycle are inconsistent. Note that this will error (if it typechecked)
        ;; because of the uninitialized variables.
        [tc-err (let ()
                 (: x String)
                 (define x y)
                 (: y Symbol)
                 (define y x)
                 y)
                #:ret (ret -Symbol -true-filter)
                #:msg #rx"expected: String|expected: Symbol"]

        ;; Test ill-typed code in letrec RHS
        [tc-err (let () (: x String) (define x 'foo) x)
                #:ret (ret -String -true-filter)
                #:msg #rx"expected: String.*given: 'foo"]

        [tc-err (let ([x (add1 5)])
                  (set! x "foo")
                  x)
          #:ret (ret -Integer -true-filter)]
        ;; w-c-m
        [tc-e/t (with-continuation-mark
                  ((inst make-continuation-mark-key Symbol)) 'mark
                  3)
              -PosByte]
        [tc-err (with-continuation-mark (5 4) 1
                  3)]
        [tc-err (with-continuation-mark 1 (5 4)
                  3)]
        [tc-err (with-continuation-mark 1 2 (5 4))]

        [tc-err (with-continuation-mark 'x 'y 'z)
          #:ret (ret (-val 'z) -bot-filter)
          #:expected (ret (-val 'z) -no-filter -no-obj)]


        ;; call-with-values

        [tc-e (call-with-values (lambda () (values 1 2))
                                (lambda: ([x : Number] [y : Number]) (+ x y)))
              -Number]
        [tc-e (call-with-values (lambda () 1)
                                (lambda: ([x : Number]) (+ x 1)))
              -Number]
        [tc-err (call-with-values (lambda () 1)
                                  (lambda: () 2))
          #:ret (ret -PosByte -true-filter)]

        [tc-err (call-with-values (lambda () (values 2))
                                  (lambda: ([x : Number] [y : Number]) (+ x y)))
          #:ret (ret -Number)]
        [tc-err (call-with-values 5
                                  (lambda: ([x : Number] [y : Number]) (+ x y)))
          #:ret (ret -Number)]
        [tc-err (call-with-values (lambda () (values 2))
                                  5)]
        [tc-err (call-with-values (lambda () (values 2 1))
                                  (lambda: ([x : String] [y : Number]) (+ x y)))]
        ;; quote-syntax
        [tc-e/t #'3 (-Syntax -PosByte)]
        [tc-e/t #'(2 3 4) (-Syntax (-lst* (-Syntax -PosByte) (-Syntax -PosByte) (-Syntax -PosByte)))]
        [tc-e/t #'id (-Syntax (-val 'id))]
        [tc-e/t #'#(1 2 3) (-Syntax (make-HeterogeneousVector (list (-Syntax -One) (-Syntax -PosByte) (-Syntax -PosByte))))]
        [tc-e/t (ann #'#(1 2 3) (Syntaxof (Vectorof (Syntaxof (U 1 2 3 'foo)))))
                (-Syntax (-vec (-Syntax (t:Un (-val 1) (-val 2) (-val 3) (-val 'foo)))))]
        [tc-e/t (ann #'#(1 2 3) (Syntaxof (Vector (Syntaxof (U 1 'foo))
                                                  (Syntaxof (U 2 'bar))
                                                  (Syntaxof (U 3 'baz)))))
                (-Syntax (make-HeterogeneousVector (list (-Syntax (t:Un (-val 1) (-val 'foo)))
                                                         (-Syntax (t:Un (-val 2) (-val 'bar)))
                                                         (-Syntax (t:Un (-val 3) (-val 'baz))))))]
        [tc-e/t #'#&2 (-Syntax (-box (-Syntax -PosByte)))]
        [tc-e/t (ann #'#&2 (Syntaxof (Boxof (Syntaxof (U 2 'foo)))))
                (-Syntax (-box (-Syntax (t:Un (-val 2) (-val 'foo)))))]
        [tc-e/t #'#hash([1 . 1] [2 . 2]) (-Syntax (make-Hashtable -Int (-Syntax -PosByte)))]
        [tc-e/t (ann #'#hash([1 . 1] [2 . 2]) (Syntaxof (HashTable (U 1 2 'foo)
                                                                   (Syntaxof (U 1 2 'bar)))))
                (-Syntax (make-Hashtable (t:Un (-val 1) (-val 2) (-val 'foo))
                                         (-Syntax (t:Un (-val 1) (-val 2) (-val 'bar)))))]

        ;; testing some primitives
        [tc-e (let ([app apply]
                    [f (lambda: [x : Number *] 3)])
                (app f (list 1 2 3)))
              -PosByte]
        [tc-e ((lambda () (call/cc (lambda: ([k : (Number -> (U))]) (if (read) 5 (k 10))))))
              -Number]

        [tc-e (number->string 5) -String]

        [tc-e (let-values ([(a b) (quotient/remainder 5 12)]
                           [(a*) (quotient 5 12)]
                           [(b*) (remainder 5 12)])
                (+ a b a* b*))
              -Nat]

        [tc-e (raise-type-error 'foo "bar" 5) (t:Un)]
        [tc-e (raise-type-error 'foo "bar" 7 (list 5)) (t:Un)]

        #| FIXME: this typechecks, but the equality check seems broken
        [tc-e
         (let ((x '(1 3 5 7 9)))
           (do: : Number ((x : (Listof Number) x (cdr x))
                          (sum : Number 0 (+ sum (car x))))
                ((null? x) sum)))
         -Number]
        |#

        ;; inference with internal define
        [tc-e (let ()
                (define x 1)
                (define y 2)
                (define z (+ x y))
                (* x z))
              -PosIndex]

        [tc-e/t (let ()
                  (define: (f [x : Number]) : Number
                    (define: (g [y : Number]) : Number
                      (let*-values ([(z w) (values (g (f x)) 5)])
                        (+ z w)))
                    (g 4))
                  5)
                -PosByte]

        [tc-err (let ()
                  (define x x)
                  1)]
        [tc-err (let ()
                  (define (x) (y))
                  (define (y) (x))
                  1)]

        [tc-e ((case-lambda:
                [[x : Number *] (+ 1 (car x))])
               5)
              -Number]
        [tc-e ((tr:case-lambda
                [[x : Number *] (+ 1 (car x))])
               5)
              -Number]

        [tc-e `(4 ,@'(3)) (-pair -PosByte (-lst* -PosByte))]

        [tc-e
         (let ((x '(1 3 5 7 9)))
           (do: : Number ((x : (Listof Number) x (cdr x))
                          (sum : Number 0 (+ sum (car x))))
                ((null? x) sum)))
         #:ret (ret -Number -top-filter -empty-obj)]

        [tc-e/t (if #f 1 'foo) (-val 'foo)]

        [tc-e (list* 1 2 3) (-pair -One (-pair -PosByte -PosByte))]
        [tc-err (list*)]

        [tc-err (apply append (list 1) (list 2) (list 3) (list (list 1) "foo"))]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list 1))) (-lst -PosByte)]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list "foo")))
              (-lst (t:Un -String -PosByte))]
        [tc-e (plambda: (b ...) [y : b ... b] (apply append (map list y)))
         #:ret (ret (-polydots (b) (->... (list) (b b) (-lst Univ))) -true-filter)]
        [tc-e/t (plambda: (b ...) [y : (Listof Integer) ... b] (apply append y))
                (-polydots (b) (->... (list) ((-lst -Integer) b) (-lst -Integer)))]

        [tc-err (plambda: (a ...) ([z : String] . [w : Number ... a])
                          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                                 1 1 1 1 w))
         #:ret (ret (-polydots (a) (->... (list -String) (-Number a) -Bottom)) -true-filter)]

        [tc-err (plambda: (a ...) ([z : String] . [w : Number])
                          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                                 1 w))]

        [tc-e/t (plambda: (a ...) ([z : String] . [w : Number ... a])
                        (apply (plambda: (b ...) ([x : Number] . [y : Number ... b]) x)
                               1 w))
              (-polydots (a) ((list -String) (-Number a) . ->... . -Number))]
        [tc-e/t (let ([f (plambda: (a ...) [w : a ... a] w)])
                  (f 1 "hello" #\c))
                (-lst* -One -String -Char)]
        ;; instantiating non-dotted terms
        [tc-e/t (inst (plambda: (a) ([x : a]) x) Integer)
                (make-Function (list (make-arr* (list -Integer) -Integer
                                                #:filters (-FS (-not-filter (-val #f) (list 0 0))
                                                               (-filter (-val #f) (list 0 0)))
                                                #:object (make-Path null (list 0 0)))))]
        [tc-e/t (inst (plambda: (a) [x : a *] (apply list x)) Integer)
                ((list) -Integer . ->* . (-lst -Integer))]

        ;; instantiating dotted terms
        [tc-e/t (inst (plambda: (a ...) [xs : a ... a] 3) Integer Boolean Integer)
                (-Integer -Boolean -Integer . t:-> . -PosByte : -true-filter)]
        [tc-e/t (inst (plambda: (a ...) [xs : (a ... a -> Integer) ... a] 3) Integer Boolean Integer)
                ((-Integer -Boolean -Integer . t:-> . -Integer)
                 (-Integer -Boolean -Integer . t:-> . -Integer)
                 (-Integer -Boolean -Integer . t:-> . -Integer)
                 . t:-> . -PosByte : -true-filter)]

        [tc-e/t (plambda: (z x y ...) () (inst map z x y ... y))
              (-polydots (z x y)
                         (t:-> (cl->*
                                ((t:-> x z) (-pair x (-lst x)) . t:-> . (-pair z (-lst z)))
                                ((list ((list x) (y y) . ->... . z) (-lst x)) 
                                 ((-lst y) y) 
                                 . ->... . (-lst z)))
                               : -true-filter 
                               : (-id-path #'map)))]

        ;; error tests
        [tc-err (+ 3 #f)]
        [tc-err (let: ([x : Number #f]) x)
          #:ret (ret -Number -true-filter)]
        [tc-err (let: ([x : Number #f]) (+ 1 x))
          #:ret (ret -Number)]

        [tc-err
         (let: ([x : Any '(foo)])
               (if (null? x) 1
                   (if (list? x)
                       (add1 x)
                       12)))
         #:ret (ret -PosByte -true-filter)]

        [tc-err (let*: ([x : Any 1]
                        [f : (-> Void) (lambda () (set! x 'foo))])
                       (if (number? x)
                           (begin (f) (add1 x))
                           12))
          #:ret (ret -PosByte -true-filter)]

        [tc-err (ann 3 (Rec a a))]
        [tc-err (ann 3 (Rec a (U a 3)))]
        [tc-err (ann 3 (Rec a (Rec b a)))]

        #| FIXME: why should this error?
        [tc-err (lambda: ([x : Any])
                         (if (number? (not (not x)))
                             (add1 x)
                             12))]
        |#

        [tc-e (filter exact-integer? (list 1 2 3 'foo))
              (-lst -Integer)]

        [tc-e (filter even? (filter exact-integer? (list 1 2 3 'foo)))
              (-lst -Integer)]

        [tc-e (filter (ann (λ: ((x : (U Symbol String)))
                               (if (symbol? x) 'x #f)) ((U Symbol String) -> (U Symbol #f) : Symbol))
                      (list "three" 'four 'five "six"))
              (-lst -Symbol)]


        [tc-e (vector-filter path-string? (ann (vector "a" 4 5 "b") (Vectorof Any)))
              (-vec (t:Un -Path -String))]

        [tc-e (sequence-filter module-path? (ann (vector "a" 4 5 "b") (Vectorof Any)))
              (-seq (t:Un -Module-Path))]


        #|
        [tc-err (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : Char] . [xs : a ... a]) c)
                                 3 (list #\c) as))]
        [tc-err (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : String] . [xs : a ... a]) c)
                                 3 (list #\c) (map list as)))]
        [tc-err (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : Char] . [xs : a ... a]) c)
                                 3 (list #\c) (map list (map list as))))]

        [tc-e/t (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : Char] . [xs : a ... a]) c)
                                 3 (list #\c) (map list as)))
                (-polydots (a) ((list) (a a) . ->... . -Integer))]
        |#

        ;; First is same as second, but with map explicitly instantiated.
        [tc-e/t (plambda: (a ...) [ys : (a ... a -> Number) *]
                (lambda: [zs : a ... a]
                  ((inst map Number (a ... a -> Number))
                   (lambda: ([y : (a ... a -> Number)])
                     (apply y zs))
                   ys)))
              (-polydots (a) ((list) ((list) (a a) . ->... . -Number) . ->* .
                                     ((list) (a a) . ->... . (-lst -Number)) : -true-filter))]
        [tc-e/t (plambda: (a ...) [ys : (a ... a -> Number) *]
                (lambda: [zs : a ... a]
                  (map (lambda: ([y : (a ... a -> Number)])
                         (apply y zs))
                       ys)))
              (-polydots (a) ((list) ((list) (a a) . ->... . -Number) . ->* .
                                     ((list) (a a) . ->... . (-lst -Number)) : -true-filter))]

        [tc-e/t (lambda: ((x : (All (t) t)))
                       ((inst (inst x (All (t) (t -> t)))
                              (All (t) t))
                        x))
              ((-poly (a) a)  . t:-> . (-poly (a) a))]

        ;; We need to make sure that even if a isn't free in the dotted type, that it gets replicated
        ;; appropriately.
        [tc-e/t (inst (plambda: (a ...) [ys : Number ... a]
                                (apply + ys))
                      Boolean String Number)
                (-Number -Number -Number . t:-> . -Number)]

        [tc-e (assq 'foo #{'((a b) (foo bar)) :: (Listof (List Symbol Symbol))})
              (t:Un (-val #f) (-lst* -Symbol -Symbol))]

        [tc-e/t (ann (lambda (x) x) (All (a) (a -> a)))
                (-poly (a) (a . t:-> . a))]
        [tc-e (apply values (list 1 2 3)) #:ret (ret (list -One -PosByte -PosByte))]

        [tc-e/t (ann (if #t 3 "foo") Integer) -Integer]

        [tc-e/t (plambda: (a ...) ([x : Number] . [y : a ... a])
                          (andmap null? (map list y)))
                (-polydots (a) ((list -Number) (a a) . ->... . -Boolean))]
        [tc-e (ann (error 'foo) (values Number Number)) #:ret (ret (list -Bottom -Bottom))]

        [tc-e (string->number "123")
              (t:Un (-val #f) -Number)]

        [tc-e #{(make-hash) :: (HashTable Number Number)}
              (-HT -Number -Number)]
        [tc-e #{(make-immutable-hash) :: (HashTable String Symbol)}
              (-HT -String -Symbol)]
        [tc-e (hash-has-key? (make-hash '((1 . 2))) 1) -Boolean]

        [tc-err (let: ([fact : (Number -> Number)
                             (lambda: ([n : Number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                        (fact 20))
          #:ret (ret -Number)]

        [tc-err (ann (lambda: ([x : Any]) #f) (Any -> Boolean : String))
          #:ret (ret (make-pred-ty -String) -true-filter)]


        [tc-e (time (+ 3 4)) -PosIndex]



        [tc-e
         (call-with-values (lambda () ((inst time-apply Number Number Number) + (list 1 2)))
                           (lambda: ([v : (Listof Number)]
                                     [cpu : Number]
                                     [user : Number]
                                     [gc : Number])
                             'whatever))
         #:ret (ret (-val 'whatever) -true-filter)]
        [tc-e
         (call-with-values (lambda () 
                             ((inst time-apply Number Number Number Number Number Number Number)
                              + (list 1 2 3 4 5 6)))
                           (lambda: ([v : (Listof Number)]
                                     [cpu : Number]
                                     [user : Number]
                                     [gc : Number])
                             'whatever))
         #:ret (ret (-val 'whatever) -true-filter)]
        [tc-e (let: ([l : (Listof Any) (list 1 2 3)])
                (if (andmap number? l)
                    (+ 1 (car l))
                    7))
              -Number]
        (tc-e (or (string->number "7") 7)
              #:ret (ret -Number -true-filter))
        [tc-e (let ([x 1]) (if x x (add1 x)))
              #:ret (ret -One -true-filter)]
        [tc-e (let: ([x : (U (Vectorof Integer) String) (vector 1 2 3)])
                (if (vector? x) (vector-ref x 0) (string-length x)))
         -Integer]
        [tc-e (let ()
                (define: foo : (Integer * -> Integer) +)
                (foo 1 2 3 4 5))
              -Integer]
        [tc-e (let ()
                (define: x : Any 7)
                (if (box? x) (unbox x) (+ 1)))
              Univ]
        [tc-e (floor 1/2) -Nat]
        [tc-e (ceiling 1/2) -PosInt]
        [tc-e (truncate 0.5) -NonNegFlonum]
        [tc-e (truncate -0.5) -NonPosFlonum]
        [tc-e/t (ann (lambda (x) (lambda (x) x))
                     (Integer -> (All (X) (X -> X))))
                (t:-> -Integer (-poly (x) (t:-> x x)))]
        [tc-e/t (lambda: ([x : Any])
                         (or (eq? 'q x)
                             (eq? 'r x)
                             (eq? 's x)))
                (make-pred-ty (t:Un (-val 'q) (-val 'r) (-val 's)))]
        [tc-e (let: ([x : Exact-Positive-Integer 1])
                (vector-ref #("a" "b") x)
                (vector-ref #("a" "b") (sub1 x))
                (vector-ref #("a" "b") (- x 1)))
              -String]
        [tc-err (string-append "bar" (if (zero? (ann 0.0 Float)) #f "foo"))
          #:ret (ret -String)]
        [tc-err (do: : Void
                     ([j : Natural (+ i 'a) (+ j i)])
                     ((>= j 10))
                     #f)
          #:ret (ret -Void -top-filter -empty-obj)]
        [tc-err (apply +)]
        [tc-e/t
         (let ([x eof])
           (if (procedure? x)
               x
               (lambda (z) (eq? x z))))
         (make-pred-ty (-val eof))]
        [tc-e ((inst map Number (Pairof Number Number))
               car
               (ann (list (cons 1 2) (cons 2 3) (cons 4 5)) (Listof (Pairof Number Number))))
              (-lst -Number)]
        [tc-err (list (values 1 2))
          #:ret (ret (-Tuple (list -Bottom)))]


        ;;Path tests
        (tc-e (path-string? "foo") -Boolean)
        (tc-e (path-string? (string->path "foo")) #:ret (ret -Boolean -true-filter))
        (tc-e (bytes->path #"foo" 'unix) -SomeSystemPath)
        (tc-e (bytes->path #"foo") -Path)
        (tc-e (bytes->path-element #"foo") -Path)
        (tc-e (bytes->path-element #"foo" 'windows) -SomeSystemPath)

        (tc-e (cleanse-path "foo") -Path)
        (tc-e (cleanse-path (string->some-system-path "foo" 'unix)) -SomeSystemPath)
        (tc-e (simplify-path "foo") -Path)
        (tc-e (simplify-path "foo" #t) -Path)
        (tc-e (simplify-path (string->some-system-path "foo" 'unix) #f) -SomeSystemPath)
        (tc-e (path->directory-path "foo") -Path)
        (tc-e (path->directory-path (string->some-system-path "foo" 'unix)) -SomeSystemPath)

        (tc-e (resolve-path "foo") -Path)
        (tc-e (expand-user-path "foo") -Path)

        ;;String Tests
        (tc-e (string? "a") #:ret (ret -Boolean -true-filter))
        (tc-e (string? 2) #:ret (ret -Boolean -false-filter))

        (tc-e (string->immutable-string (string #\a #\b)) -String)
        (tc-e (string-length (make-string 5 #\z)) -Index)
        (tc-e (string-copy (build-string 26 integer->char)) -String)
        (tc-e (string-copy! (make-string 4) 0 "foob") -Void)
        (tc-e (string-copy! (make-string 4) 1 "bark" 1) -Void)
        (tc-e (string-copy! (make-string 4) 1 "zend" 1 2) -Void)

        (tc-e (string-fill! (make-string 5) #\Z) -Void)

        (tc-e (string-append) -String)
        (tc-e (string-append "a" "b") -String)
        (tc-e (string-append "c" "d" "f") -String)

        (tc-e (string->list "abcde") (-lst -Char))
        (tc-e (list->string (list #\a #\d #\d)) -String)

        (tc-e (string=? "a" "a") -Boolean)
        (tc-e (string<? "a" "a") -Boolean)
        (tc-e (string>? "a" "a") -Boolean)
        (tc-e (string<=? "a" "a") -Boolean)
        (tc-e (string>=? "a" "a") -Boolean)

        (tc-e (string-upcase "a") -String)
        (tc-e (string-downcase "a") -String)
        (tc-e (string-titlecase "a") -String)
        (tc-e (string-foldcase "a") -String)


        (tc-e (string-ci=? "a" "A") -Boolean)
        (tc-e (string-ci<? "a" "A") -Boolean)
        (tc-e (string-ci>? "a" "A") -Boolean)
        (tc-e (string-ci<=? "a" "A") -Boolean)
        (tc-e (string-ci>=? "a" "A") -Boolean)


        (tc-e (string-normalize-nfd "a") -String)
        (tc-e (string-normalize-nfkd "a") -String)
        (tc-e (string-normalize-nfc "a") -String)
        (tc-e (string-normalize-nfkc "a") -String)



        (tc-e (string-locale=? "a" "a") -Boolean)
        (tc-e (string-locale<? "a" "a") -Boolean)
        (tc-e (string-locale>? "a" "a") -Boolean)

        (tc-e (string-locale-upcase "a") -String)
        (tc-e (string-locale-downcase "a") -String)


        (tc-e (string-locale-ci=? "a" "A") -Boolean)
        (tc-e (string-locale-ci<? "a" "A") -Boolean)
        (tc-e (string-locale-ci>? "a" "A") -Boolean)

        ;Symbols

        (tc-e (symbol? 'foo) #:ret (ret -Boolean -true-filter))
        (tc-e (symbol? 2) #:ret (ret -Boolean -false-filter))

        (tc-e (symbol-interned? 'foo) -Boolean)
        (tc-e (symbol-interned? (string->unreadable-symbol "bar")) -Boolean)
        (tc-e (symbol-interned? (string->uninterned-symbol "bar")) -Boolean)
        (tc-e (symbol-interned? (gensym 'foo)) -Boolean)
        (tc-e (symbol-interned? (gensym "foo")) -Boolean)

        (tc-e (symbol-unreadable? (gensym)) -Boolean)
        (tc-e (symbol-unreadable? 'foo) -Boolean)
        (tc-e (string->unreadable-symbol "bar") -Symbol)
        (tc-e (string->uninterned-symbol "bar") -Symbol)

        (tc-e (symbol->string 'foo) -String)
        (tc-e (string->symbol (symbol->string 'foo)) -Symbol)

        ;Booleans
        (tc-e (not #f) #:ret (ret -Boolean -true-filter))
        (tc-e (false? #f) #:ret (ret -Boolean -true-filter))
        (tc-e (not #t) #:ret (ret -Boolean -false-filter))
        ;; It's not clear why the following test doesn't work,
        ;; but it works fine in the real typechecker
        ;(tc-e (false? #t) #:ret (ret -Boolean -false-filter))


        (tc-e (boolean? true) #:ret (ret -Boolean -true-filter))
        (tc-e (boolean? 6) #:ret (ret -Boolean -false-filter))
        (tc-e (immutable? (cons 3 4)) -Boolean)

        (tc-e (boolean=? #t false) -Boolean)
        (tc-e (symbol=? 'foo 'foo) -Boolean)

        (tc-e (equal? 1 2) -Boolean)
        (tc-e (eqv? 1 2) -Boolean)
        (tc-e (eq? 1 2) -Boolean)
        (tc-e (equal?/recur 'foo 'bar eq?) -Boolean)



        (tc-e (shuffle '("a" "b")) (-lst -String))


        ;Regexps
        (tc-e (regexp-match "foo" "foobar") (-opt (-pair -String (-lst (-opt -String)))))
        (tc-e (regexp-match #"foo" #"foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #rx"foo" "foobar") (-opt (-pair -String (-lst (-opt -String)))))
        (tc-e (regexp-match #rx#"foo" #"foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #px"foo" "foobar") (-opt (-pair -String (-lst (-opt -String)))))
        (tc-e (regexp-match #px#"foo" #"foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-e (regexp-match "foo" #"foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #"foo" "foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #rx"foo" #"foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #rx#"foo" "foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #px"foo" #"foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #px#"foo" "foobar") (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-e (regexp-match "foo" (string->path "tmp")) (-opt (-pair -String (-lst (-opt -String)))))
        (tc-e (regexp-match #"foo" (string->path "tmp")) (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match "foo" (open-input-string "tmp"))
              (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #"foo" (open-input-string "tmp"))
              (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-err (regexp-try-match "foo" "foobar")
          #:ret (ret (t:Un (-val #f) (-pair -Bytes (-lst (t:Un (-val #f) -Bytes))))))
        (tc-e (regexp-try-match "foo" (open-input-string "foobar"))
              (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-err (regexp-match-peek "foo" "foobar")
          #:ret (ret (t:Un (-val #f) (-pair -Bytes (-lst (t:Un (-val #f) -Bytes))))))
        (tc-e (regexp-match-peek "foo" (open-input-string "foobar"))
              (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-err (regexp-match-peek-immediate "foo" "foobar")
          #:ret (ret (t:Un (-val #f) (-pair -Bytes (-lst (t:Un (-val #f) -Bytes))))))
        (tc-e (regexp-match-peek-immediate "foo" (open-input-string "foobar"))
              (-opt (-pair -Bytes (-lst (-opt -Bytes)))))



        [tc-e (regexp-match/end "foo" "foobar")
              #:ret (ret (list (-opt (-pair -String (-lst (-opt -String)))) (-opt -Bytes)))]

        (tc-e (regexp-split "foo" "foobar") (-pair -String (-lst -String)))
        (tc-e (regexp-split "foo" #"foobar") (-pair -Bytes (-lst -Bytes)))
        (tc-e (regexp-split #"foo" "foobar") (-pair -Bytes (-lst -Bytes)))
        (tc-e (regexp-split #"foo" #"foobar") (-pair -Bytes (-lst -Bytes)))

        (tc-err (regexp-split "foo" (path->string "foobar"))
          #:ret (ret (-pair -String (-lst -String))))

        (tc-e (regexp-replace "foo" "foobar" "rep") -String)
        (tc-e (regexp-replace #"foo" "foobar" "rep") -Bytes)
        (tc-e (regexp-replace "foo" #"foobar" "rep") -Bytes)
        (tc-e (regexp-replace "foo" #"foobar" #"rep") -Bytes)

        (tc-err (regexp-replace "foo" "foobar" #"rep"))
        (tc-e (regexp-replace "foo" "foobar" (lambda: (args : String *) "foo")) -String)
        (tc-e (regexp-replace "foo" #"foobar" (lambda: (args : Bytes *) #"foo")) -Bytes)
        (tc-err (regexp-replace "foo" "foobar" (lambda: (args : Bytes *) #"foo")))
        (tc-err (regexp-replace #"foo" "foobar" (lambda: (args : String *) "foo")))

        ;File System
        (tc-e (find-system-path 'home-dir) -Path)
        (tc-e (path-list-string->path-list "/bin:/sbin:/usr/bin" null) (-lst -Path))
        (tc-e (find-executable-path "racket" "collects" #t) (-opt -Path))

        (tc-e (file-exists? "/usr") -Boolean)
        (tc-e (link-exists? "/usr") -Boolean)
        (tc-e (delete-file "does-not-exist") -Void)

        (tc-e (rename-file-or-directory "old" "new") -Void)
        (tc-e (rename-file-or-directory "old" "new" #t) -Void)

        (tc-e (file-or-directory-modify-seconds "dir") -NonNegFixnum)
        (tc-e (file-or-directory-modify-seconds "dir" #f) -NonNegFixnum)
        (tc-e (file-or-directory-modify-seconds "dir" 20) -Void)
        (tc-e (file-or-directory-modify-seconds "dir" #f (lambda () "error"))
              (t:Un -NonNegFixnum -String))
        (tc-e (file-or-directory-modify-seconds "dir" 20 (lambda () "error")) (t:Un -Void -String))

        (tc-e (file-or-directory-permissions "tmp") (-lst (one-of/c 'read 'write 'execute)))
        (tc-e (file-or-directory-permissions "tmp" #f) (-lst (one-of/c 'read 'write 'execute)))
        (tc-e (file-or-directory-permissions "tmp" 'bits) -NonNegFixnum)
        (tc-e (file-or-directory-permissions "tmp" 4) -Void)

        (tc-e (file-or-directory-identity "tmp") -PosInt)
        (tc-e (file-or-directory-identity "tmp" 3) -PosInt)

        (tc-e (file-size "tmp") -Nat)

        (tc-e (copy-file "tmp/src" "tmp/dest") -Void)
        (tc-e (make-file-or-directory-link "tmp/src" "tmp/dest") -Void)

        (tc-e (current-drive) -Path)

        (tc-e (directory-exists? "e") -Boolean)
        (tc-e (make-directory "e") -Void)

        (tc-e (delete-directory "e") -Void)

        (tc-e (directory-list) (-lst -Path))
        (tc-e (directory-list "tmp") (-lst -Path))
        (tc-e (filesystem-root-list) (-lst -Path))




        (tc-e (copy-directory/files "tmp/src" "tmp/dest") -Void)
        (tc-e (delete-directory/files "tmp/src") -Void)

        (tc-e (find-files (lambda (p) #t)) (-lst -Path))
        (tc-e (find-files (lambda (p) #t) #f) (-lst -Path))
        (tc-e (find-files (lambda (p) #t) "start") (-lst -Path))

        (tc-e (pathlist-closure (list "thpm" "htmp")) (-lst -Path))
        (tc-e (fold-files (lambda: ((p : Path) (type : Symbol) (res : 'res))
                            (if (eq? type 'dir) (values res #t) (values res 'ignored))) 'res)
              (-val 'res))
        (tc-e (fold-files (lambda: ((p : Path) (type : Symbol) (res : 'res)) res) 'res "tmp" #f)
              (-val 'res))

        (tc-e (make-directory* "tmp/a/b/c") -Void)


        (tc-e (put-preferences (list 'sym 'sym2) (list 'v1 'v2)) -Void)

        (tc-e (preferences-lock-file-mode) (one-of/c 'exists 'file-lock))

        (tc-e (make-lock-file-name "tmp.file") -Pathlike)
        (tc-e (make-lock-file-name "tmp.dir" "tmp.file") -Pathlike)


        ;New set operations
        (tc-e (set-union (set 'one) (set 'two)) (-set (one-of/c 'one 'two)))
        (tc-e (set-intersect (set 'one) (set 'two)) (-set (-val 'one)))
        (tc-e (set-subtract (set 'one) (set 'two)) (-set (-val 'one)))
        (tc-e (set-symmetric-difference (set 'one) (set 'two)) (-set (one-of/c 'one 'two)))

        (tc-e (list->set (list 'one 'two)) (-set (one-of/c 'one 'two)))
        (tc-e (list->seteq (list 'one 'two)) (-set (one-of/c 'one 'two)))
        (tc-e (list->seteqv (list 'one 'two)) (-set (one-of/c 'one 'two)))
        (tc-e (set->list (set 'one 'two)) (-lst (one-of/c 'one 'two)))


        ;Syntax

        (tc-e (syntax? #'id) #:ret (ret -Boolean -true-filter))
        (tc-e (syntax? 2) #:ret (ret -Boolean -false-filter))

        (tc-e (syntax-source #'here) Univ)
        (tc-e (syntax-line #'here) (-opt -PosInt))
        (tc-e (syntax-column #'here) (-opt -Nat))
        (tc-e (syntax-position #'here) (-opt -PosInt))
        (tc-e (syntax-span #'here) (-opt -Nat))


        ;Parameters
        (tc-e (parameter-procedure=? current-input-port current-output-port) -Boolean)

        ;Namespaces
        (tc-e (namespace? 2) #:ret (ret -Boolean -false-filter))
        (tc-e (namespace? (make-empty-namespace)) #:ret (ret -Boolean -true-filter))

        (tc-e (namespace-anchor? 3) #:ret (ret -Boolean -false-filter))
        (tc-e/t (lambda: ((x : Namespace-Anchor)) (namespace-anchor? x))
                (t:-> -Namespace-Anchor -Boolean : -true-filter))


        (tc-e (variable-reference? 3) #:ret (ret -Boolean -false-filter))
        (tc-e/t (lambda: ((x : Variable-Reference)) (variable-reference? x))
                (t:-> -Variable-Reference  -Boolean : -true-filter))

        (tc-e (system-type 'os) (one-of/c 'unix 'windows 'macosx))
        (tc-e (system-type 'gc) (one-of/c 'cgc '3m))
        (tc-e (system-type 'link) (one-of/c 'static 'shared 'dll 'framework))
        (tc-e (system-type 'so-suffix) -Bytes)
        (tc-e (system-type 'machine) -String)
        (tc-err (system-type 'other))

        (tc-e (tcp-listen 49 45) -TCP-Listener)
        (tc-e (tcp-connect "google.com" 80) (list -Input-Port -Output-Port))


        (tc-e (udp-open-socket) -UDP-Socket)
        (tc-e (udp-close (udp-open-socket)) -Void)

        (tc-e (udp-addresses (udp-open-socket)) (list -String -String))
        (tc-e (udp-addresses (udp-open-socket) #f) (list -String -String))
        (tc-e (udp-addresses (udp-open-socket) #t)
              (list -String -NonNegFixnum -String -NonNegFixnum))

        ;Byte converters
        (tc-e (bytes-open-converter "UTF-8" "UTF-8") (-opt -Bytes-Converter))
        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert c #"abcde"))
              (list -Bytes -Nat (one-of/c 'complete 'continues 'aborts 'error)))
        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert c #"abcde" 0 5 (make-bytes 10)))
              (list -Nat -Nat (one-of/c 'complete 'continues 'aborts 'error)))

        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert-end c))  (list -Bytes (one-of/c 'complete 'continues)))

        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert-end c (make-bytes 10)))  (list -Nat (one-of/c 'complete 'continues)))

        ;Subprocess
        (tc-e (subprocess #f #f #f (string->path "/usr/bin/echo")
                          "string" (string->path "path") #"bytes")
              (list
                -Subprocess
                -Input-Port
                -Output-Port
                -Input-Port))

        (tc-e (subprocess (current-output-port) (current-input-port) (current-error-port)
                          (string->path "/usr/bin/echo") 'exact "arg")
              (list
                -Subprocess
                (-val #f)
                (-val #f)
                (-val #f)))

        (tc-e (let ()
                (: p Subprocess)
                (: std-out (Option Input-Port))
                (: std-in  (Option Output-Port))
                (: std-err (Option Input-Port))
                (define-values (p std-out std-in std-err)
                 (subprocess #f #f #f (string->path "/bin/bash")))
                (subprocess? p))
              #:ret (ret -Boolean -true-filter))

        (tc-e (car (process "hello"))
              -Input-Port)
        (tc-e (car (process* "hello"))
              -Input-Port)

        #;
        (tc-e (let ()
                (: std-out Input-Port)
                (: std-in  Output-Port)
                (: std-err Input-Port)
                (: proc-id Natural)
                (: f Any)
                (define-values (std-out std-in proc-id std-err f)
                 (apply values (process/ports #f #f #f "/bin/bash")))
                proc-id)
              -Nat)


        #;
        (tc-e (let ()
                (: std-out #f)
                (: std-in  #f)
                (: std-err #f)
                (: proc-id Natural)
                (: f Any)
                (define-values (std-out std-in proc-id std-err f)
                 (apply values (process*/ports (current-output-port)
                                               (current-input-port)
                                               (current-error-port)
                                               "/bin/bash"
                                               "arg1"
                                               #"arg2")))
                proc-id)
              -Nat)

        ;Compilation
        (tc-e (compile-syntax #'(+ 1 2)) -Compiled-Expression)
        (tc-e (let: ((e : Compiled-Expression (compile #'(+ 1 2))))
                (compiled-expression? e))
              #:ret (ret -Boolean -true-filter))
        (tc-e (let: ((e : Compiled-Expression (compile #'(module + racket 2))))
                (compiled-module-expression? e)) -Boolean)

        ;Impersonator Property
        (tc-e (make-impersonator-property 'prop)
              (list -Impersonator-Property (t:-> Univ -Boolean) (t:-> Univ Univ)))
        (tc-e (let-values: ((((prop : Impersonator-Property)
                              (pred : (Any -> Any))
                              (acc : (Any -> Any)))
                             (make-impersonator-property 'prop)))
               (impersonator-property? prop))
              #:ret (ret -Boolean -true-filter))

        ;Security Guards
        (tc-e (make-security-guard (current-security-guard)
                                   (lambda args (void))
                                   (lambda args (void)))
              -Security-Guard)
        (tc-e (let: ((s : Security-Guard (current-security-guard)))
                (security-guard? s))
              #:ret (ret -Boolean -true-filter))


        ;Custodians
        (tc-e (make-custodian) -Custodian)
        (tc-e (let: ((c : Custodian (current-custodian)))
                (custodian? c))
              #:ret (ret -Boolean -true-filter))
        (tc-e (let: ((c : (Custodian-Boxof Integer) (make-custodian-box (current-custodian) 1)))
                (custodian-box-value c)) -Int)

        ;Thread Groups
        (tc-e (make-thread-group) -Thread-Group)
        (tc-e (let: ((tg : Thread-Group (current-thread-group)))
                (thread-group? tg))
              #:ret (ret -Boolean -true-filter))


        ;Inspector
        (tc-e (make-inspector) -Inspector)
        (tc-e (let: ((i : Inspector (current-inspector)))
                (inspector? i))
              #:ret (ret -Boolean -true-filter))

        ;Continuation Prompt Tags ang Continuation Mark Sets
        ;; TODO: supporting default-continuation-prompt-tag means we need to
        ;;       specially handle abort-current-continuation in the type system
        ;(tc-e (default-continuation-prompt-tag) -Prompt-Tag)
        (tc-e (let: ((pt : (Prompt-Tagof Integer Integer) (make-continuation-prompt-tag)))
                   (continuation-marks #f pt)) -Cont-Mark-Set)
        (tc-e (let: ((set : Continuation-Mark-Set (current-continuation-marks)))
                   (continuation-mark-set? set)) #:ret (ret -Boolean -true-filter))

        ;Logging
        (tc-e (make-logger 'name) -Logger)
        (tc-e (let: ((l : Logger (make-logger)))
                (let: ((lr : Log-Receiver (make-log-receiver l 'error)))
                 (log-message l 'error "Message" 'value))) -Void)

        ;Events
        (tc-e (sync (make-semaphore)) -Semaphore)
        (tc-e (sync (tcp-listen 5555)) -TCP-Listener)
        (tc-e (sync (tcp-listen 5555) (make-semaphore))
              (make-Union (list -TCP-Listener -Semaphore)))
        (tc-e (sync (thread (λ () 0))) -Thread)
        (tc-e (sync (make-will-executor)) -Will-Executor)
        (tc-e (sync (make-custodian-box (current-custodian) 0))
              (make-CustodianBox (-val 0)))
        (tc-e (sync ((inst make-channel String))) -String)
        (tc-e (sync always-evt) (-mu x (make-Evt x)))
        (tc-e (sync never-evt) -Bottom)
        (tc-e (sync never-evt always-evt) (-mu x (make-Evt x)))
        (tc-e (sync (system-idle-evt)) -Void)
        (tc-e (sync (choice-evt (system-idle-evt))) -Void)
        (tc-e (sync (choice-evt (system-idle-evt)
                                ((inst make-channel String))))
              (make-Union (list -String -Void)))
        (tc-e (sync/timeout 100 (make-semaphore) (tcp-listen 5555))
              (make-Union (list (-val #f) -TCP-Listener -Semaphore)))
        (tc-e (handle-evt ((inst make-channel Number))
                          (λ: ([x : Number]) (number->string x)))
              (make-Evt -String))
        (tc-e (wrap-evt ((inst make-channel Number))
                        (λ: ([x : Number]) (number->string x)))
              (make-Evt -String))
        (tc-e (guard-evt (inst make-channel String))
              (make-Evt -String))
        (tc-e (sync (replace-evt always-evt (lambda (x) 3)))
              (-mu x (make-Evt x)))
        (tc-e (sync (replace-evt always-evt (lambda (x)
                                              (alarm-evt (+ (current-inexact-milliseconds) 1000)))))
              (-mu x (make-Evt x)))
        (tc-err (let: ([a : (U (Evtof Any) String) always-evt])
                  (if (handle-evt? a) a (string->symbol a)))
          #:ret (ret (t:Un -Symbol (make-Evt Univ))))
        (tc-err (let: ([a : (U (Evtof Any) String) always-evt])
                  (if (channel-put-evt? a) a (string->symbol a)))
          #:ret (ret (t:Un -Symbol (make-Evt (-mu x (make-Evt x))))))
        (tc-err (let: ([a : (U (Evtof Any) String) always-evt])
                  (if (semaphore-peek-evt? a) a (string->symbol a)))
          #:ret (ret (t:Un -Symbol (make-Evt (-mu x (make-Evt x))))))

        ;Semaphores
        (tc-e (make-semaphore) -Semaphore)
        (tc-e (let: ((s : Semaphore (make-semaphore 3)))
                      (semaphore-post s)) -Void)

        ;Random Numbers
        (tc-e (make-pseudo-random-generator) -Pseudo-Random-Generator)
        (tc-e (let: ((pg : Pseudo-Random-Generator (make-pseudo-random-generator)))
                (pseudo-random-generator->vector pg))
              (make-HeterogeneousVector (list -PosInt -PosInt -PosInt -PosInt -PosInt -PosInt)))

        ;Structure Type Properties
        (tc-e (make-struct-type-property 'prop)
              (list -Struct-Type-Property (t:-> Univ -Boolean) (t:-> Univ Univ)))
        (tc-e (let-values: ((((prop : Struct-Type-Property)
                              (pred : (Any -> Any)) (acc : (Any -> Any)))
                             (make-struct-type-property 'prop)))
               (struct-type-property? prop))
              #:ret (ret -Boolean -true-filter))

        ;Wills
        (tc-e (make-will-executor) -Will-Executor)
        ;; FIXME: Broken because ManyUniv doesn't have a corresponding tc-result
        #|
        (tc-e (let: ((w : Will-Executor (make-will-executor)))
                (will-register w 'a (lambda: ((s : Symbol)) (void)))
                (will-execute w)) #:ret tc-any-results)
        |#

        ;Promises
        ;For some reason they are failing in the test suite
        #|
        (tc-e (delay 's) (-Promise -Symbol))
        (tc-e (let: ((p : (Promise Symbol) (delay 's)))
                  (promise-running? p)) -Boolean)
        |#


        ;Kernel Structs, check that their hidden identifiers type
        (tc-e (void exn
                    exn:fail
                    exn:fail:contract
                    exn:fail:contract:arity
                    exn:fail:contract:divide-by-zero
                    exn:fail:contract:non-fixnum-result
                    exn:fail:contract:continuation
                    exn:fail:contract:variable
                    exn:fail:syntax
                    exn:fail:read
                    exn:fail:read:eof
                    exn:fail:read:non-char
                    exn:fail:filesystem
                    exn:fail:filesystem:exists
                    exn:fail:filesystem:version
                    exn:fail:network
                    exn:fail:out-of-memory
                    exn:fail:unsupported
                    exn:fail:user
                    exn:break
                    arity-at-least
                    date
                    srcloc)
              -Void)
        [tc-e (raise (exn:fail:contract "1" (current-continuation-marks))) (t:Un)]
        [tc-err (exn:fail:contract)
          #:ret (ret (resolve (make-Name #'exn:fail:contract null #f #t)))]
        [tc-e (#%variable-reference) -Variable-Reference]
        [tc-e (#%variable-reference x) -Variable-Reference]
        [tc-e (#%variable-reference +) -Variable-Reference]
        [tc-e (apply (λ: ([x : String] [y : String])
                       (string-append x y)) (list "foo" "bar")) -String]
        [tc-e (apply (plambda: (a) ([x : a] [y : a]) x) (list "foo" "bar")) -String]
        [tc-e (ann
               (case-lambda [(x) (add1 x)]
                            [(x y) (add1 x)])
               (case-> (Integer -> Integer)
                       (Integer Integer -> Integer)))
              #:ret (ret (cl->* (t:-> -Integer -Integer)
                                (t:-> -Integer -Integer -Integer))
                         -true-filter)]
        [tc-e (let ([my-pred (λ () #f)])
                (for/and: : Any ([i (in-range 4)])
                          (my-pred)))
              #:ret (ret Univ -top-filter -empty-obj)]
        [tc-e
         (let ()
           (define: long : (List 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 Integer)
             (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))

           (define-syntax-rule (go acc ...)
             (begin (ann (acc long) One) ...))

           (go first second third fourth fifth sixth seventh eighth ninth tenth))
         (-val 1)]

        [tc-e (vector-append #(1) #(2))
              (-vec -Integer)]
        [tc-e/t (ann #() (Vectorof Integer))
                (-vec -Integer)]

        [tc-e (let: ([x : Float 0.0])
                (= 0 x))
              -Boolean]
        [tc-e (let: ([x : Inexact-Real 0.0])
                (= 0 x))
              -Boolean]
        [tc-e (let: ([x : Real 0.0])
                (= 0 x))
              -Boolean]

        [tc-e/t (ann (lambda: ([x : Boolean])
                       (if x x #t))
                     (Boolean -> #t))
                (t:-> -Boolean (-val #t))]

        [tc-e (sequence? 'foo)
              -Boolean]
        [tc-err (stop-before (inst empty-sequence Symbol) zero?)]
        [tc-e (stop-before (inst empty-sequence Integer) zero?)
              (-seq -Int)]
        [tc-e (stop-after (inst empty-sequence Integer) zero?)
              (-seq -Int)]
        [tc-e (sequence->list (inst empty-sequence Symbol))
              (-lst -Symbol)]
        [tc-e (sequence-length (inst empty-sequence Symbol))
              -Nat]
        [tc-e (sequence-ref (inst empty-sequence Symbol) 0)
              -Symbol]
        [tc-e (sequence-tail (inst empty-sequence Symbol) 0)
              (-seq -Symbol)]
        [tc-e (sequence-append empty-sequence (inst empty-sequence Symbol))
              (-seq -Symbol)]
        [tc-e (sequence-append (inst empty-sequence Symbol) (inst empty-sequence Integer))
              (-seq (t:Un -Symbol -Int))]
        [tc-e (sequence-map add1 (inst empty-sequence Integer))
              (-seq -Int)]
        [tc-err (sequence-andmap zero? (inst empty-sequence Symbol))]
        [tc-e (sequence-andmap zero? (inst empty-sequence Integer))
              -Boolean]
        [tc-e (sequence-andmap add1 (inst empty-sequence Integer))
              (t:Un -Int (-val #t))]
        [tc-e (sequence-ormap zero? (inst empty-sequence Integer))
              -Boolean]
        [tc-e (sequence-ormap add1 (inst empty-sequence Integer))
              (t:Un -Int (-val #f))]
        [tc-e (sequence-fold (lambda: ([y : (Listof Symbol)] [x : Symbol]) (cons x y))
                             null empty-sequence)
              (-lst -Symbol)]
        [tc-e (sequence-count zero? (inst empty-sequence Integer))
              -Nat]
        [tc-e (sequence-filter zero? (inst empty-sequence Integer))
              (-seq (-val 0))]
        [tc-e (sequence-add-between (inst empty-sequence Integer) 'foo)
              (-seq (t:Un -Int (-val 'foo)))]
        [tc-e (let ()
                (define: x : Any (vector 1 2 3))
                (if (vector? x) (vector-ref x 0) #f))
              Univ]
        [tc-e ((inst vector Index) 0)
              (-vec -Index)]
        [tc-err ((inst list Void) 1 2 3)
          #:ret (ret (-lst -Void))]
        [tc-e ((inst list Any) 1 2 3)
              (-lst Univ)]

        [tc-e (let ()
                (define f
                  (lambda: ((x : Boolean) (y : String))
                    (if x y "false")))
                (apply f (list #f "2")))
              -String]
        [tc-err (let ()
                  (: f (All (i ...) Any -> (values Any ... i)))
                  (define (f x) (values 1 2)))]
        [tc-err (let ()
                  (: g (All (i ...) Any -> (values Any ... i)))
                  (define (g x) 2))]
        [tc-err
          (let ((s (ann (set 2) Any)))
            (if (set? s) (ann s (Setof String)) ((inst set String))))
          #:ret (ret (-set -String))]

        [tc-e (split-at (list 0 2 3 4 5 6) 3)
              (list (-lst -Byte) (-lst -Byte))]
        [tc-e (vector-split-at (vector 2 3 4 5 6) 3)
              (list (-vec -Integer) (-vec -Integer))]

        [tc-e/t (ann ((letrec ((x (lambda args 3))) x) 1 2) Byte) -Byte]
        [tc-e (vector-ref (ann (vector 'a 'b) (Vector Symbol Symbol)) 0)
              -Symbol]
        [tc-err (vector-ref (ann (vector 'a 'b) (Vector Symbol Symbol)) 4)]
        [tc-e (vector-ref (ann (vector 'a 'b) (Vector Symbol Symbol)) (+ -1 2))
              -Symbol]
        [tc-e (vector-set! (ann (vector 'a 'b) (Vector Symbol Symbol)) 0 'c)
              -Void]
        [tc-err (vector-set! (ann (vector 'a 'b) (Vector Symbol Symbol)) -4 'c)]
        [tc-e (vector-set! (ann (vector 'a 'b) (Vector Symbol Symbol)) (+ -1 2) 'c)
              -Void]
        [tc-err
          (ann
            ((letrec ((x (lambda (acc v) (if v (list v) acc)))) x) null (list 'bad 'prog))
            (Listof Symbol))
          #:ret (ret (-lst -Symbol) -top-filter -empty-obj)]
        [tc-e (filter values empty)
              (-lst -Bottom)]
        [tc-e (lambda lst (map (plambda: (b) ([x : b]) x) lst))
              (-polydots (a) (->... (list) (a a) (make-ListDots a 'a)))
              #:expected (ret (-polydots (a) (->... (list) (a a) (make-ListDots a 'a))))]

        [tc-e/t (ann (lambda (x) #t) (All (a) Any))
                (-poly (a) Univ)]
        [tc-e
           ((inst filter Any Symbol) symbol? null)
           (-lst -Symbol)]
        [tc-e/t (ann (plambda: (A -Boolean ...) ((a : A) b : B ... B)
                       (apply (inst values A B ... B) a b)) 
                     (All (A B ...) (A B ... -> (values A B ... B))))
              (-polydots (a b) ((list a) (b b) . ->... . (make-ValuesDots (list (-result a)) b 'b)))]
        [tc-e/t (ann (ann 'x Symbol) Symbol) -Symbol]

        [tc-err (lambda (x) x)
          #:ret (ret (-poly (a) (cl->* (t:-> a a) (t:-> a a a))))
          #:expected (ret (-poly (a) (cl->* (t:-> a a) (t:-> a a a))))]
        [tc-err (plambda: (A) ((x : A)) x)
          #:ret (ret (list -Symbol -Symbol))
          #:expected (ret (list -Symbol -Symbol))]

        [tc-e/t
          (case-lambda
            [w 'result]
            [(x) (add1 "hello")])
          (->* (list) Univ (-val 'result) : -true-filter)]

        [tc-e
           (opt-lambda: ((x : Symbol 'a)) x)
           #:ret (ret (t:-> -Symbol -Symbol) -true-filter)
           #:expected (ret (t:-> -Symbol -Symbol) -true-filter)]

        [tc-e/t (inst (ann (lambda (a) a) (All (a) (a -> a))) Symbol)
                (t:-> -Symbol -Symbol)]

        ;; This test makes sure that a user written filter
        ;; can reference an identifier object in addition to
        ;; an integer object.
        [tc-e/t
         (λ (x)
           (define f
             (ann (λ (y) (exact-integer? x))
                  ;; note the filters
                  (Any -> Boolean : #:+ (Integer @ x) #:- (! Integer @ x))))
           (if (f 'dummy) (add1 x) 2))
         (t:-> Univ -Integer : (-FS -top (-filter -Integer 0)))]

        ;; This test ensures that curried predicates have
        ;; the correct filters so that they can be used for
        ;; occurrence typing.
        [tc-e
         (let ()
           (define f (λ (x) (λ (y) (number? x))))
           (: b (U Number String))
           (define b 5)
           (define g (f b))
           ;; this doesn't type-check unless OT is working
           (if (g "foo") (add1 b) 3)
           (void))
         ;; type doesn't really matter, just make sure it typechecks
         -Void]

        ;; Tests the absence of a regression with curried functions
        ;; (should not trigger an error with free-identifier=?)
        [tc-e (lambda (x) (lambda (y) y))
              #:ret
              (ret (t:-> Univ (t:-> Univ Univ : (-FS (-not-filter (-val #f) (list 0 0))
                                                     (-filter (-val #f) (list 0 0)))
                                              : (make-Path null (list 0 0)))
                              : -true-filter)
                   -true-filter)]

       ;; The following ensures that the correct filter can be
       ;; written by the user
       [tc-e
        (let ()
          (: f (Any -> (Any -> Boolean : #:+ (Symbol @ 1 0)
                            #:- (! Symbol @ 1 0))
                    : #:+ Top #:- Bot))
          (define f (λ (x) (λ (y) (symbol? x))))
          (: b (U Symbol String))
          (define b 'sym)
          (define g (f b))
          (if (g "foo") (symbol->string b) "str")
          (void))
        ;; type doesn't really matter, just make sure it typechecks
        -Void]

       ;; Unit test for PR 13298. Should raise an unbound id error
       ;; instead of just allowing `x` to be undefined
       [tc-err (let () (: x Number) 3)]

       ;; Sets as sequences
       [tc-e (in-set (set 1 2 3)) (-seq -PosByte)]
       [tc-e
        (let ()
          (: lst (Listof Integer))
          (define lst
            (for/list: : (Listof Integer) ([i : Integer (set 1 2 3)]) i))
          (void))
        -Void]
       [tc-e
        (let ()
          (for: ([k : Symbol (in-set (set 'x 'y 'z))]) (displayln k))
          (void))
        -Void]

       ;; PR 14139
       [tc-e
        (let ()
          (: f : (U (Setof Integer) Integer) → (Setof Integer))
          (define (f s) (if (set? s) s (set)))
          (void))
        -Void]

       ;; negate
       [tc-e
        (let ()
          (: x (U Symbol Void))
          (define x 'foo)
          (if ((negate void?) x) (symbol->string x) "foo"))
        -String]
       [tc-e
        (let ()
          (: pos? (Real -> Boolean : #:+ (Positive-Real @ 0) #:- (Nonpositive-Real @ 0)))
          (define pos? (lambda: ([x : Real]) (positive? x)))
          (: x Real)
          (define x 3)
          (if ((negate pos?) x) x -5))
        #:ret (ret -NonPosReal -true-filter)]

       [tc-err
         (hash-ref! (ann (make-hash) (HashTable #f (-> #t))) #f (lambda () #t))]
       [tc-e
         (hash-ref (ann (make-hash) (HashTable #f #t)) #f #f)
         -Boolean]

       ;; regexp-replaces
       [tc-e
        (regexp-replaces "zero-or-more?" '([#rx"-" "_"] [#rx"(.*)\\?$" "is_\\1"]))
        (t:Un -String -Bytes)]
       [tc-e
        (regexp-replaces "zero-or-more?" '(["e" "o"] ["o" "oo"]))
        (t:Un -String -Bytes)]

       ;; racket/string
       [tc-e
        (string-append* "a" '("c" "d"))
        -String]
       [tc-e
        (string-append* (cdr (append* (map (λ: ([x : String]) (list ", " x))
                                           '("Alpha" "Beta" "Gamma")))))
        -String]
       [tc-e
        (string-join '("x" "y" "z") ", "
                     #:before-first "Todo: "
                     #:before-last " and "
                     #:after-last ".")
        -String]
       [tc-e
        (string-normalize-spaces "  foo bar  baz \r\n\t")
        -String]
       [tc-e
        (string-replace "foo bar baz" "bar" "blah")
        -String]
       [tc-e
        (string-split "  " #:trim? #f)
        (-lst -String)]
       [tc-e
        (string-trim "  foo bar  baz \r\n\t" " " #:repeat? #t)
        -String]

       ;; remove and friends
       [tc-e (remq #f '(1 2 3 4)) (-lst -PosByte)]
       [tc-e (remv #f '(1 2 3 4)) (-lst -PosByte)]
       [tc-e (remove #f '(1 2 3 4)) (-lst -PosByte)]
       [tc-e (remove* '(1 2) '(a b c d)) (-lst (one-of/c 'a 'b 'c 'd))]
       [tc-e (remq* '(1 2) '(a b c d)) (-lst (one-of/c 'a 'b 'c 'd))]
       [tc-e (remv* '(1 2) '(a b c d)) (-lst (one-of/c 'a 'b 'c 'd))]

       ;; functions from racket/list
       [tc-e (takef '(a b "x" "y") symbol?) (-lst -Symbol)]
       [tc-e (takef-right '(a b "x" "y") string?) (-lst -String)]
       [tc-e (dropf '("a" b "x" "y") string?)
             (-lst (t:Un -String (-val 'b)))]
       [tc-e (dropf-right '("a" b "x" "y") string?)
             (-lst (t:Un -String (-val 'b)))]
       [tc-e (splitf-at '("a" b "x" "y") string?)
             #:ret (ret (list (-lst -String)
                              (-lst (t:Un -String (-val 'b)))))]
       [tc-e (splitf-at-right '("a" b "x" "y") string?)
             #:ret (ret (list (-lst (t:Un -String (-val 'b)))
                              (-lst -String)))]
       [tc-e (permutations '(a b c d)) (-lst (-lst (one-of/c 'a 'b 'c 'd)))]
       [tc-e (in-permutations '(a b c d)) (-seq (-lst (one-of/c 'a 'b 'c 'd)))]

       ;; test functions which do lookup with the "wrong type", where the
       ;; result type shouldn't be widened to include that type
       [tc-e (memq 3 '(a b c)) (t:Un (-val #f) (-lst (one-of/c 'a 'b 'c)))]
       [tc-e (memv 3 '(a b c)) (t:Un (-val #f) (-lst (one-of/c 'a 'b 'c)))]
       [tc-e (member 3 '(a b c)) (t:Un (-val #f) (-lst (one-of/c 'a 'b 'c)))]
       [tc-e (member 3 '(a b c) equal?) (t:Un (-val #f) (-lst (one-of/c 'a 'b 'c)))]
       [tc-e (assq 3 '((a . 5) (b . 7))) (t:Un (-val #f) (-pair (one-of/c 'a 'b) -PosByte))]
       [tc-e (assv 3 '((a . 5) (b . 7))) (t:Un (-val #f) (-pair (one-of/c 'a 'b) -PosByte))]
       [tc-e (assoc 3 '((a . 5) (b . 7))) (t:Un (-val #f) (-pair (one-of/c 'a 'b) -PosByte))]
       [tc-e (set-remove (set 1 2 3) 'a) (-set -PosByte)]
       ;; don't return HashTableTop
       [tc-e (hash-remove #hash((a . 5) (b . 7)) 3) (-HT -Symbol -Integer)]
       [tc-e (hash-remove #hash((a . 5) (b . 7)) 3) (-HT -Symbol -Integer)]
       ;; these should actually work
       [tc-e (vector-memq 3 #(a b c)) (t:Un (-val #f) -Index)]
       [tc-e (vector-memv 3 #(a b c)) (t:Un (-val #f) -Index)]
       [tc-e (vector-member 3 #(a b c)) (t:Un (-val #f) -Index)]

       ;; tests for struct type types
       [tc-e (let-values ([(_1 _2 _3 _4 _5 _6 parent _7)
                           (struct-type-info
                            (let-values ([(type _1 _2 _3 _4)
                                          (make-struct-type 'foo #f 3 0)])
                              type))])
               parent)
             (-opt (make-StructTypeTop))]
       [tc-e (let-values ([(name _1 _2 getter setter _3 _4 _5)
                           (struct-type-info struct:arity-at-least)])
               (getter (arity-at-least 3) 0))
             Univ]
       [tc-e/t (assert (let-values ([(type _) (struct-info (arity-at-least 3))])
                       type))
               (make-StructTypeTop)]
       [tc-err (let-values ([(name _1 _2 getter setter _3 _4 _5)
                             (struct-type-info struct:arity-at-least)])
                 (getter 'bad 0))
         #:ret (ret Univ)]
       [tc-err (struct-type-make-constructor 'bad)
         #:ret (ret top-func)]
       [tc-err (struct-type-make-predicate 'bad)]

       [tc-e
         (call-with-values (lambda () (eval #'(+ 1 2))) (inst list Any))
         (-lst Univ)]

       ;; futures & places primitives
       [tc-e (future (λ () "foo")) (-future -String)]
       [tc-e (would-be-future (λ () "foo")) (-future -String)]
       [tc-e (touch (future (λ () "foo"))) -String]
       [tc-e (current-future) (-opt (-future Univ))]
       [tc-e (add1 (processor-count)) -PosInt]
       [tc-e (assert (current-future) future?)
             #:ret (ret (-future Univ) -true-filter)]
       [tc-e (futures-enabled?) -Boolean]
       [tc-e (place-enabled?) -Boolean]
       [tc-e (dynamic-place "a.rkt" 'a #:at #f) -Place]
       [tc-e (dynamic-place (string->path "a.rkt") 'a #:at #f) -Place]
       [tc-e/t (let-values
                 ([(p _1 _2 _3)
                   (dynamic-place* "a.rkt" 'a #:in (open-input-string "hi"))])
                 p)
               -Place]
       [tc-e (let ([p (dynamic-place "a.rkt" 'a)])
               (place-break p)
               (place-break p 'terminate)
               (place-kill p)
               (list (place-wait p)
                     (place-dead-evt p)))
             (-lst* -Int (-mu x (make-Evt x)))]
       [tc-e (let ()
               (define-values (c1 c2) (place-channel))
               (place-channel-get c2)
               (place-channel-get c2)
               (place-channel-put/get c2 "b")
               (place-channel-put c1 "a"))
             -Void]
       [tc-e (place-message-allowed? 'msg) -Boolean]

       ;; fxvectors & flvectors
       [tc-e (let ()
               (define fx1 (fxvector 0 500 34))
               (define fx2 (make-fxvector 20 3))
               (fx+ (fxvector-ref fx1 0) 2)
               (fxvector-set! fx2 1 5)
               (fxvector-length fx1)
               (fxvector-copy fx2 0 5))
             -FxVector]
       [tc-e (let ()
               (define s1 (in-flvector (flvector 1.0 2.0 3.0)))
               (define f1 (sequence-ref s1 0))
               (define s2 (in-fxvector (fxvector 1 2 3)))
               (define f2 (sequence-ref s2 2))
               (define s3 (in-extflvector (extflvector 1.0t0 2.0t0 3.0t0)))
               (define f3 (sequence-ref s3 1))
               (list f1 f2 f3))
             (-lst* -Flonum -Fixnum -ExtFlonum)]

       ;; for/hash, for*/hash - PR 14306
       [tc-e (for/hash: : (HashTable Symbol String)
               ([x (in-list '(x y z))]
                [y (in-list '("a" "b" "c"))]
                #:when (eq? x 'x))
               (values x y))
             #:ret (ret (-HT -Symbol -String)
                        (-FS -top -top)
                        -empty-obj)]
       [tc-e (for*/hash: : (HashTable Symbol String)
               ([k (in-list '(x y z))]
                [v (in-list '("a" "b"))]
                #:when (eq? k 'x))
               (values k v))
             #:ret (ret (-HT -Symbol -String)
                        (-FS -top -top)
                        -empty-obj)]

       ;; PR 13937
       [tc-e (let ()
               (: foo ((HashTable String Symbol) -> (HashTable Symbol String)))
               (define (foo map)
                 (for/hash : (HashTable Symbol String)
                             ([(str sym) map])
                   (values sym str)))
               (foo #hash(("foo" . foo))))
             (-HT -Symbol -String)]

       ;; for/hash doesn't always need a return annotation inside
       [tc-e/t (let ()
                 (tr:define h : (HashTable Any Any)
                   (for/hash ([(k v) (in-hash #hash(("a" . a)))])
                     (values v k)))
                 h)
               (-HT Univ Univ)]

       ;; call-with-input-string and friends - PR 14050
       [tc-e (call-with-input-string "abcd" (lambda: ([input : Input-Port]) (values 'a 'b)))
             #:ret (ret (list (-val 'a) (-val 'b)))]
       [tc-e (call-with-input-bytes #"abcd" (lambda: ([input : Input-Port]) (values 'a 'b)))
             #:ret (ret (list (-val 'a) (-val 'b)))]

       [tc-e (lambda: ([x : (U (Parameter Symbol) Symbol)])
               (if (parameter? x)
                   (x)
                   x))
             #:ret (ret (t:-> (t:Un (-Param -Symbol -Symbol) -Symbol) -Symbol))
             #:expected (ret (t:-> (t:Un (-Param -Symbol -Symbol) -Symbol) -Symbol))]

       ;; time-apply and similar functions (test improved inference)
       [tc-e (let ()
               (: f (All (b a ...) (-> (List a ... a) b b)))
               (define (f lst x) x)
               (f '(a b) "foo"))
             -String]
       [tc-e (time-apply (lambda: ([x : Symbol] [y : Symbol]) "foo") '(a b))
             #:ret (ret (list (-lst* -String) -Nat -Nat -Nat))]

       ;; test kw function without type annotation
       [tc-e (let () (tr:define (f x #:y y) y) (f 'a #:y 'b)) Univ]
       [tc-e (let () (tr:define (f x [a "a"] #:y y) y) (f 'a #:y 'b)) Univ]
       [tc-e (let () (tr:define (f x #:y y . args) y) (f 'a 'b #:y 'c)) Univ]
       [tc-e (let () (tr:define (f x [a "a"] #:y y . args) y) (f 'a 'b #:y 'c)) Univ]
       [tc-e (let () (tr:define (f x #:y y #:z z) y) (f 'a #:y 'c #:z 'd)) Univ]
       [tc-e (let () (tr:define (f x #:y [y "y"] #:z z) y)
                     (f 'a #:y 'y #:z 'd) (f 'a #:z 'd)) Univ]
       [tc-e (let () (tr:define (f x #:y [y "y"] #:z [z "z"]) y)
                     (f 'a) (f 'a #:z 'd) (f 'a #:y 'y #:z 'd)) Univ]
       [tc-e (let () (tr:define (f x #:y y #:z z) y) (f 'a #:y 'c #:z 'd)) Univ]
       [tc-e (let () (tr:define (f x [a "a"] #:y y #:z z) y) (f 'a #:y 'c #:z 'd)) Univ]
       [tc-e (let () (tr:define (f x #:y [y 'y]) y) (f "a" #:y "b")) Univ]
       [tc-e (let () (tr:define (f x [a "a"] #:y [y 'y]) y) (f "a" #:y "b")) Univ]
       [tc-e (let () (tr:define (f x #:y [y 'y] . args) y) (f "a" "b" #:y "b")) Univ]
       [tc-e (let () (tr:define (f x [a "a"] #:y [y 'y] . args) y) (f "a" "b" #:y "b")) Univ]
       ;; FIXME: these tests work in the REPL correctly, but not in the test harness
       ;;        probably due to the reader type annotations
       #|
       [tc-e (let () (tr:define (f #{x : Symbol} #:y y) x) (f 'a #:y 'b)) -Symbol]
       [tc-err (let () (tr:define (f #{x : Symbol} #:y y) y) (f "a" #:y 'b))
               #:msg #rx"expected: Symbol.*given: String"]
       |#
       [tc-e (tr:lambda (x #:y y) y) (->key Univ #:y Univ #t Univ)]
       [tc-e ((tr:lambda (x #:y y) y) 'a #:y 'b) Univ]
       [tc-e ((tr:lambda (x #:y y . args) y) 'a #:y 'b) Univ]
       [tc-e ((tr:lambda (x #:y [y 'y] . args) y) 'a #:y 'b) Univ]
       [tc-err (let () (tr:define (f x #:y y) (string-append x "foo")) (void))
         #:ret (ret -Void)
         #:msg #rx"expected: String.*given: Any"]
       [tc-err (let () (tr:define (f x #:y y) y) (f "a"))
         #:ret (ret Univ)
         #:msg #rx"required keyword was not supplied"]

       ;; test lambdas with mixed type expressions, typed keywords, typed
       ;; optional arguments
       [tc-e (tr:lambda (x [y : String]) (string-append y "b"))
             #:ret (ret (t:-> Univ -String -String) -true-filter)]
       [tc-e (tr:lambda (x [y : String] . z) (string-append y "b"))
             #:ret (ret (->* (list Univ -String) Univ -String) -true-filter)]
       [tc-e (tr:lambda (x [y : String] . [z : String *]) (string-append y "b"))
             #:ret (ret (->* (list Univ -String) -String -String) -true-filter)]
       [tc-e (tr:lambda (x [y : String]) : String (string-append y "b"))
             #:ret (ret (t:-> Univ -String -String) -true-filter)]
       [tc-e (tr:lambda (x z [y : String]) (string-append y "b"))
             #:ret (ret (t:-> Univ Univ -String -String) -true-filter)]
       [tc-e (tr:lambda (x z [y : String] . w) (string-append y "b"))
             #:ret (ret (->* (list Univ Univ -String) Univ -String) -true-filter)]
       [tc-e (tr:lambda (x z [y : String] . [w : String *]) (string-append y "b"))
             #:ret (ret (->* (list Univ Univ -String) -String -String) -true-filter)]
       [tc-e (tr:lambda (x z [y : String]) : String (string-append y "b"))
             #:ret (ret (t:-> Univ Univ -String -String) -true-filter)]
       [tc-err (tr:lambda (x [y : String]) : Symbol (string-append y "b"))
         #:ret (ret (t:-> Univ -String -Symbol) -true-filter)
         #:msg "expected: Symbol.*given: String"]
       [tc-err (tr:lambda (x [y : String "a"] z) (string-append y "b"))
               #:msg "expected optional lambda argument"]
       [tc-e (tr:lambda (x [y : String "a"]) (string-append y "b"))
             (->opt Univ [-String] -String)]
       [tc-e (tr:lambda (x [y : String "a"] . z) (string-append y "b"))
             (->optkey Univ [-String] #:rest Univ -String)]
       [tc-e (tr:lambda (x [y : String "a"] . [z : String *]) (string-append y "b"))
             (->optkey Univ [-String] #:rest -String -String)]
       [tc-e (tr:lambda (x y [z : String "a"]) (string-append z "b"))
             (->opt Univ Univ [-String] -String)]
       [tc-e (tr:lambda (w x [y : String "y"] [z : String "z"]) (string-append y z))
             (->opt Univ Univ [-String -String] -String)]
       [tc-e (tr:lambda (w [x : String] [y : String "y"] [z : String "z"])
               (string-append x z))
             (->opt Univ -String [-String -String] -String)]
       [tc-e (tr:lambda (x #:y [y : String]) (string-append y "b"))
             (->key Univ #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String] . z) (string-append y "b"))
             (->optkey Univ [] #:rest Univ #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String] . [z : String *]) (string-append y "b"))
             (->optkey Univ [] #:rest -String #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String]) : String (string-append y "b"))
             (->key Univ #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String "a"]) (string-append y "b"))
             (->key Univ #:y -String #f -String)]
       [tc-e (tr:lambda (x #:y [y : String "a"]) : String (string-append y "b"))
             (->key Univ #:y -String #f -String)]
       [tc-e (tr:lambda (x #:y [y : String] [z "z"]) (string-append y "b"))
             (->optkey Univ [Univ] #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String "a"] [z "z"]) (string-append y "b"))
             (->optkey Univ [Univ] #:y -String #f -String)]
       [tc-e (tr:lambda (x [z "z"] #:y [y : String]) (string-append y "b"))
             (->optkey Univ [Univ] #:y -String #t -String)]
       [tc-e (tr:lambda (x [z "z"] #:y [y : String "a"]) (string-append y "b"))
             (->optkey Univ [Univ] #:y -String #f -String)]
       [tc-e (tr:lambda (x [z "z"] #:y [y : String "a"]) : String (string-append y "b"))
             (->optkey Univ [Univ] #:y -String #f -String)]
       [tc-e (tr:lambda (x #:y [y : String] [z : String "z"]) (string-append y z))
             (->optkey Univ [-String] #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String] [z : String "z"] . w) (string-append y z))
             (->optkey Univ [-String] #:rest Univ #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String] [z : String "z"] . [w : String *]) (string-append y z))
             (->optkey Univ [-String] #:rest -String #:y -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String "y"] [z : String "z"]) (string-append y z))
             (->optkey Univ [-String] #:y -String #f -String)]
       [tc-e (tr:lambda (x [z : String "z"] #:y [y : String]) (string-append y z))
             (->optkey Univ [-String] #:y -String #t -String)]
       [tc-e (tr:lambda (x [z : String "z"] #:y [y : String "a"]) (string-append y z))
             (->optkey Univ [-String] #:y -String #f -String)]
       [tc-e (tr:lambda (x #:y [y : String] #:z [z : String]) (string-append y z))
             (->key Univ #:y -String #t #:z -String #t -String)]
       [tc-e (tr:lambda (x #:y [y : String] #:z [z : String "z"]) (string-append y z))
             (->key Univ #:y -String #t #:z -String #f -String)]
       [tc-e (tr:lambda (x #:y [y : String "y"] #:z [z : String "z"]) (string-append y z))
             (->key Univ #:y -String #f #:z -String #f -String)]
       ;; for these next three tests, test the application instead of the
       ;; type of the function because the precise filters are hard to
       ;; get right in the expected result type and polymorphic types are
       ;; harder to test for equality.
       [tc-e ((inst (tr:lambda #:forall (A) (x [y : A]) y) String) 'a "foo")
             #:ret (ret -String -true-filter)]
       [tc-e ((inst (tr:lambda #:∀ (A) (x [y : A]) y) String) 'a "foo")
             #:ret (ret -String -true-filter)]
       [tc-e ((inst (tr:lambda #:forall (A ...) (x . [rst : A ... A]) rst) String) 'a "foo")
             #:ret (ret (-lst* -String) -true-filter)]
       #| FIXME: does not work yet, TR thinks the type variable is unbound
       [tc-e (inst (tr:lambda #:forall (A) (x [y : A] [z : String "z"]) y) String)
             #:ret (ret (->opt Univ -String [-String] -String) -true-filter)]
       |#

       ;; test `define` with mixed type annotations
       [tc-e (let () (tr:define y "bar")
                     (tr:define x : String "foo")
                     (string-append x y))
             -String]
       [tc-e (let () (tr:define ((f [x : String]) y) x)
                     ;; FIXME: does not work due to a bug in
                     ;; lambda type-checking
                     ;(tr:define ((g x) [y : String]) y)
                     (string-append ((f "foo") 'y) "bar"))
             -String]
       [tc-e (let () (tr:define #:forall (A ...) (f x . [rst : A ... A]) rst)
                     (f 'a "b" "c"))
             #:ret (ret (-lst* -String -String) -true-filter)]

       ;; test new :-less forms that allow fewer annotations
       [tc-e/t (let ([x "foo"]) x) -String]
       [tc-e (let ([x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (let ([x : String "foo"] [y 'y]) (string-append x "bar"))
             -String]
       [tc-e (let ([y 'y] [x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (let ([y 'y] [x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (let #:forall (A) ([x : A "foo"]) x)
             #:ret (ret -String -true-filter)]
       [tc-e (let #:forall (A) ([y 'y] [x : A "foo"]) x)
             #:ret (ret -String -true-filter)]
       [tc-e/t (let* ([x "foo"]) x) -String]
       [tc-e (let* ([x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (let* ([x : String "foo"] [y 'y]) (string-append x "bar"))
             -String]
       [tc-e (let* ([y 'y] [x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (let* ([y 'y] [x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e/t (letrec ([x "foo"]) x) -String]
       [tc-e (letrec ([x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (letrec ([x : String "foo"] [y 'y]) (string-append x "bar"))
             -String]
       [tc-e (letrec ([y 'y] [x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e (letrec ([y 'y] [x : String "foo"]) (string-append x "bar"))
             -String]
       [tc-e/t (let-values ([(x y) (values "foo" "bar")]) x) -String]
       [tc-e (let-values ([(x y) (values "foo" "bar")]
                          [([z : String]) (values "baz")])
               (string-append x y z))
             -String]
       [tc-e (let-values ([([x : String] [y : String]) (values "foo" "bar")])
               (string-append x y))
             -String]
       [tc-e/t (letrec-values ([(x y) (values "foo" "bar")]) x)
               -String]
       [tc-e (letrec-values ([(x y) (values "foo" "bar")]
                             [([z : String]) (values "baz")])
               (string-append x y z))
             -String]
       [tc-e (letrec-values ([([x : String] [y : String]) (values "foo" "bar")])
               (string-append x y))
             -String]
       [tc-e (let loop ([x "x"]) x)
             #:ret (ret Univ -true-filter)]
       [tc-e (let loop ([x : String "x"]) x)
             #:ret (ret -String -true-filter)]
       [tc-e (let/cc k "foo") -String]
       [tc-e (let/ec k "foo") -String]
       [tc-e (let/cc k : String (k "foo")) -String]
       [tc-e (let/ec k : String (k "foo")) -String]
       [tc-e (ann (do ([x : Integer 0 (add1 x)]) ((> x 10) x) (displayln x))
                  Integer)
             #:ret (ret -Integer -top-filter -empty-obj)]
       [tc-e (do : Integer ([x : Integer 0 (add1 x)]) ((> x 10) x) (displayln x))
             #:ret (ret -Integer -top-filter -empty-obj)]
       [tc-e (tr:case-lambda [(x [y : String]) x])
             #:ret (ret (t:-> Univ -String Univ
                              : (-FS (-not-filter (-val #f) (list 0 0))
                                     (-filter (-val #f) (list 0 0)))
                              : (make-Path null (list 0 0)))
                        -true-filter)]
       [tc-e (tr:case-lambda [(x [y : String] . rst) x])
             #:ret (ret (->* (list Univ -String) Univ Univ
                             : (-FS (-not-filter (-val #f) (list 0 0))
                                    (-filter (-val #f) (list 0 0)))
                             : (make-Path null (list 0 0)))
                        -true-filter)]
       [tc-e (tr:case-lambda [(x [y : String] . [rst : String *]) x])
             #:ret (ret (->* (list Univ -String) -String Univ
                             : (-FS (-not-filter (-val #f) (list 0 0))
                                    (-filter (-val #f) (list 0 0)))
                             : (make-Path null (list 0 0)))
                        -true-filter)]
       [tc-e (tr:case-lambda #:forall (A) [([x : A]) x])
             #:ret (ret (-poly (A)
                          (t:-> A A
                                : (-FS (-not-filter (-val #f) (list 0 0))
                                       (-filter (-val #f) (list 0 0)))
                                : (make-Path null (list 0 0))))
                        -true-filter)]

       ;; PR 13651 and related
       [tc-e (tr:lambda #:forall (a ...) ([f : (-> String (values a ... a))])
               (f "foo"))
             #:ret (ret (-polydots (a)
                          (t:-> (t:-> -String (make-ValuesDots '() a 'a))
                                (make-ValuesDots '() a 'a)))
                        -true-filter)]
       [tc-e (inst (plambda: (A B ...) ((a : A) b : B ... B)
                     ((ann (lambda () (apply (inst values A B ... B) a b))
                           (-> (values A B ... B)))))
                   String String Symbol)
             #:ret (ret (t:-> -String -String -Symbol
                              (-values (list -String -String -Symbol)))
                        -true-filter)]

       ;; make-input-port, make-output-port (examples from Reference)
       [tc-e (let ()
               (define /dev/null-in
                 (make-input-port
                  'null
                  (lambda (s) eof)
                  (lambda (skip s progress-evt) eof)
                  void
                  (lambda () never-evt)
                  (lambda (k progress-evt done-evt)
                    (error "no successful peeks!"))))
               (read-char /dev/null-in))
             (t:Un -Char (-val eof))]
       [tc-e (let ()
               (define infinite-ones
                 (let ([one! (tr:lambda ([s : Bytes])
                               (bytes-set! s 0 (char->integer #\1)) 1)])
                   (make-input-port
                    'ones
                    one!
                    (tr:lambda ([s : Bytes] skip progress-evt) (one! s))
                    void)))
               (read-string 5 infinite-ones))
             (t:Un -String (-val eof))]
       [tc-e (let ()
               (define infinite-voids
                 (make-input-port
                  'voids
                  (lambda (s) (lambda args 'void))
                  (lambda (skip s evt) (lambda args 'void))
                  void))
               (read-char-or-special infinite-voids))
             Univ]
       [tc-e (let ()
               (define mod3-cycle/one-thread
                 (let* ([n 2]
                        [mod! (tr:lambda ([s : Bytes] [delta : Integer])
                                (bytes-set! s 0 (+ 48 (modulo (+ n delta) 3)))
                                1)])
                   (make-input-port
                    'mod3-cycle/not-thread-safe
                    (tr:lambda ([s : Bytes])
                      (set! n (modulo (add1 n) 3))
                      (mod! s 0))
                    (tr:lambda ([s : Bytes] [skip : Integer] evt)
                      (mod! s skip))
                    void)))
               (read-string 5 mod3-cycle/one-thread))
             (t:Un -String (-val eof))]
       [tc-e (let ()
               (define /dev/null-out
                 (make-output-port
                  'null
                  always-evt
                  (tr:lambda (s [start : Integer] [end : Integer] non-block? breakable?)
                    (- end start))
                  void
                  (lambda (special non-block? breakable?) #t)
                  (tr:lambda (s [start : Integer] [end : Integer])
                    (wrap-evt always-evt (lambda (x) (- end start))))
                  (lambda (special) always-evt)))
               (display "hello" /dev/null-out))
             -Void]
       [tc-e (let ()
               (: accum-list (Listof Char))
               (define accum-list null)
               (define accumulator/not-thread-safe
                 (make-output-port
                  'accum/not-thread-safe
                  always-evt
                  (tr:lambda ([s : Bytes] [start : Integer] [end : Integer] _1 _2)
                    (set! accum-list
                          (append accum-list
                                  (map integer->char
                                       (bytes->list (subbytes s start end)))))
                    (- end start))
                  void))
               (display "hello" accumulator/not-thread-safe))
             -Void]

       ;; Additional tests for recursive type aliases
       [tc-e ;; The types here are valid, but uninhabitable.
             (let () (define-type-alias A (Listof B))
                     (define-type-alias B (Listof A))
                     "dummy")
             #:ret (ret -String -true-filter)]
       [tc-e (let () (define-type-alias A (Listof B))
                     (define-type-alias B (U #f (Listof A)))
                     (: a A)
                     (define a (list #f (list (list #f))))
                     (void))
             -Void]
       [tc-err (let () (define-type-alias A (Class #:implements A)) "dummy")
               #:msg "Recursive #:implements clause not allowed"]
       [tc-err (let () (define-type-alias X (U X #f)) "dummy")
               #:msg "recursive types are not allowed directly inside"]
       [tc-err (let () (define-type-alias X (All (A #:row) X)) "dummy")
               #:msg "recursive types are not allowed directly inside"]

       ;; Check the more precise Tarjan's algorithm-based letrec-values type checking
       [tc-e ;; An example from Eric Dobson (see gh372) that shows that precisely
             ;; finding cycles is necessary for checking more letrecs.
             (let ()
               (: foo (-> String))
               (define (foo)
                 (: bn (Integer -> Integer))
                 (define (bn n) (if (= n 0) 1 (bn (- n 1))))
                 (define v (bn 0))
                 "foo")
               (foo))
             -String]
       [tc-e/t (letrec-values ([(a b) (values x y)]
                               [(x y) (values "x" "y")])
                 a)
               -String]
       [tc-e/t (letrec-values ([(a b) (values x "b")]
                               [(x y) (values "x" "y")])
                 a)
               -String]
       [tc-e/t (letrec-values ([(a b) (values "a" "b")]
                               [(x y) (values z z)]
                               [(z) a])
                 z)
               -String]
       [tc-err (letrec-values ([(a b) (values x "b")]
                               [(x y) (values a b)])
                 a)
               #:msg "type information"]
       [tc-err (letrec-values ([(a) (values x)]
                               [(x) (values z)]
                               [(z) (values a)])
                 a)
               #:msg "type information"]
       ;; make sure no-binding cases like the middle expression are checked
       [tc-err (let () (define r "r") (string-append r 'foo) (define x "x") "y")
         #:ret (ret -String -true-filter)
         #:msg "expected: String.*given: 'foo"]

       ;; Polydotted types are not checking equality correctly
       [tc-err (ann (lambda () (let ([my-values values]) (my-values)))
                 (All (A ...) (-> (Values Symbol ... A))))
         #:ret (ret (-polydots (A) (t:-> (-values-dots null -Symbol 'A))) -true-filter)]

       [tc-e (list 'x)
             #:ret (ret (-Tuple (list -Symbol)))
             #:expected (ret (-Tuple (list -Symbol)) -no-filter -no-obj)]
       [tc-e (list 'y)
             #:ret (ret (-lst -Symbol))
             #:expected (ret (-lst -Symbol) -no-filter -no-obj)]
       [tc-e (reverse (list 'x 'y))
             #:ret (ret (-Tuple (list (-val 'y) (-val 'x))))
             #:expected (ret (-Tuple (list (-val 'y) (-val 'x))) -no-filter -no-obj)]

       [tc-err (vector 1 2)
         #:ret (ret (make-HeterogeneousVector (list -Byte -Byte)) -false-filter -empty-obj)
         #:expected (ret (make-HeterogeneousVector (list -Byte -Byte)) -false-filter -no-obj)]

       [tc-err (values 'x)
         #:ret (ret (list -Symbol -Symbol)
                    (list -bot-filter -bot-filter)
                    (list -empty-obj -empty-obj))
         #:expected (ret (list -Symbol -Symbol) (list -no-filter -no-filter ) (list -no-obj -no-obj))]

       [tc-err (values 'x 'y 'z)
         #:ret (ret (list -Symbol -Symbol)
                    (list -bot-filter -bot-filter)
                    (list -empty-obj -empty-obj))
         #:expected (ret (list -Symbol -Symbol) (list -no-filter -no-filter ) (list -no-obj -no-obj))]

       [tc-err (values 'y)
         #:ret (ret (list -Symbol) (list -bot-filter ) (list -empty-obj) Univ 'B)
         #:expected (ret (list -Symbol) (list -no-filter ) (list -no-obj) Univ 'B)]

       [tc-err (values (values 'x 'y))
         #:ret (ret (-val 'x) -bot-filter)
         #:expected (ret (-val 'x) -no-filter -no-obj)]

       [tc-err (if (random) (values 1 2) 3)
         #:ret (ret (-val 3) -true-filter)
         #:expected (ret (-val 3) -no-filter -no-obj)]

       [tc-err
         (let* ([x 42]
                [n x])
           (set! n 43)
           (if #t
               (add1 "")
               0))
         #:ret (ret -Bottom)]

       [tc-e
         (let: ([x : Any 4])
           (if (let: ([y x]) (number? y))
               (add1 x)
               4))
         -Number]

       [tc-e
         (let ()
           (: g (-> Boolean))
           (define (g) (g))
           (: x Any)
           (define x 0)
           (number? x))
         -Boolean]

       [tc-e
         (let ()
           (: g (Any -> Boolean : #:+ (Number @ 0) #:- Bot))
           (define (g x)
             (or (number? x)
                 (g x)))
           (: x Any)
           (define x 0)
           (g x)
           (add1 x))
         -Number]

       [tc-e
         (let: ([x : Any 1])
           (unless (number? x)
             (error 'foo))
           (add1 x))
         -Number]

       [tc-e
         (let: ([x : Any 1])
           (let ()
             (unless (number? x)
               (error 'foo))
             #t)
           (add1 x))
         -Number]

       [tc-e/t
         (let ()
           (: f (Number -> Number))
           (define (f x)
             (cond
               ((zero? x) x)
               (else (add1 (f (sub1 x))))))

           (define y (f 7))
           4)
         -PosByte]

       [tc-err
         (let ()
           (: z (case-> (Symbol -> Symbol)
                        (Number -> Number)))
           (define z (lambda (a) a))
           (z "y"))
         #:ret (ret -String -bot-filter)
         #:expected (ret -String -no-filter -no-obj)]

       [tc-err
         (let ()
           (: z (case->
                  (-> Number #:b Symbol Number)
                  (-> Symbol #:b Symbol Symbol)))
           (define z (lambda (a #:b b) a))
           (z "y" #:b "y"))
         #:ret (ret -String -bot-filter)
         #:expected (ret -String -no-filter -no-obj)]

       [tc-e/t
         (lambda (x)
           (unless (number? x)
             (error 'foo)))
         (t:-> Univ -Void : (-FS (-filter -Number 0) (-filter -Number 0)))]

       [tc-e
         (let ([x : (U) (error 'fail)])
           (if (number? x) (add1 x) 0))
         -Bottom]

       [tc-err
         (let ([f (lambda (x y) y)])
           (f 1))
         #:ret (ret Univ -top-filter)]

       [tc-err
         (let ([f (lambda (x y) y)])
           (f 1 2 3))
         #:ret (ret Univ -true-filter)]

       [tc-err
         (case-lambda
           ((x y . z) 'x)
           ((x . y) 'x)
           (w (first w)))
         #:ret
           (ret (cl->* (->* (list -Symbol -Symbol) -Symbol -Symbol)
                       (->* (list) -String -String)))
         #:expected
           (ret (cl->* (->* (list -Symbol -Symbol) -Symbol -Symbol)
                       (->* (list) -String -String)))]

       [tc-e
         (case-lambda
           [() 1]
           [args 2])
         #:ret (ret (t:-> (-val 1)) -true-filter)
         #:expected (ret (t:-> (-val 1)) -no-filter)]

       [tc-e
         (case-lambda
           [(x . y) 2]
           [args 1])
         #:ret (ret (cl->* (t:-> (-val 1)) (t:-> Univ (-val 2))) -true-filter)
         #:expected (ret (cl->* (t:-> (-val 1)) (t:-> Univ (-val 2))) -no-filter)]

       [tc-e
         (case-lambda
           [(x) 2]
           [args 1])
         #:ret (ret (cl->* (t:-> (-val 1)) (t:-> Univ (-val 2))) -true-filter)
         #:expected (ret (cl->* (t:-> (-val 1)) (t:-> Univ (-val 2))) -no-filter)]

       [tc-err
         (case-lambda
           [(x . y) 1]
           [args 2])
         #:ret (ret (cl->* (t:-> (-val 1)) (t:-> Univ (-val 1))) -true-filter)
         #:expected (ret (cl->* (t:-> (-val 1)) (t:-> Univ (-val 1))) -no-filter)]

       ;; typecheck-fail should fail
       [tc-err (typecheck-fail #'stx "typecheck-fail")
               #:msg #rx"typecheck-fail"]
       [tc-err (string-append (typecheck-fail #'stx "typecheck-fail") "bar")
               #:ret (ret -String)
               #:msg #rx"typecheck-fail"]

       [tc-e
         (let: ([f : (All (b ...) (Any ... b -> Any)) (lambda x 'x)])
           (lambda xs (apply f xs)))
         #:ret (ret (->* (list) Univ Univ))
         #:expected (ret (->* (list) Univ Univ))]

       [tc-e
         (let: ([f : (All (b ...) (Any ... b -> Any)) (lambda x 'x)])
           (lambda xs (apply f (ann (cons 'y xs) (cons Symbol (Listof Any))))))
         #:ret (ret (->* (list) Univ Univ))
         #:expected (ret (->* (list) Univ Univ))]

       [tc-e
         (let: ([f : (All (b ...) (Any ... b -> Any)) (lambda x 'x)])
           (lambda xs (apply f 'y xs)))
         #:ret (ret (->* (list) Univ Univ))
         #:expected (ret (->* (list) Univ Univ))]

       [tc-err
         (let: ([f : (case->) (case-lambda)])
            (apply f empty))
         #:ret (ret -Bottom)
         #:msg #rx"has no cases"]
       [tc-err
         (let: ([f : (All (A) (case->)) (case-lambda)])
            (apply f empty))
         #:ret (ret -Bottom)
         #:msg #rx"has no cases"]
       [tc-err
         (let: ([f : (All (A ...) (case->)) (case-lambda)])
            (apply f empty))
         #:ret (ret -Bottom)
         #:msg #rx"has no cases"]

       [tc-err
         (let: ([f : (case->) (case-lambda)])
            (apply f empty))
         #:ret (ret -Bottom)
         #:msg #rx"has no cases"]
       [tc-err
         (let: ([f : (All (A) (case->)) (case-lambda)])
            (apply f empty))
         #:ret (ret -Bottom)
         #:msg #rx"has no cases"]
       [tc-err
         (let: ([f : (All (A ...) (case->)) (case-lambda)])
            (apply f empty))
         #:ret (ret -Bottom)
         #:msg #rx"has no cases"]

       [tc-e/t
         (let: ([f : (All (a) (a a * -> Void)) (λ _ (void))])
           (plambda: (A B ...) ([xs : (List Any A ... B)])
             (apply f xs)))
         (-polydots (a b) (t:-> (-pair Univ  (make-ListDots a 'b)) -Void))]
       [tc-e/t
         (let: ([f : (All (a) (a a * -> Void)) (λ _ (void))])
           (plambda: (A B ...) ([xs : (List A ... B)])
             (apply f (first xs) xs)))
         (-polydots (a b) (t:-> (make-ListDots a 'b) -Void))]

       [tc-e/t
         (let ()
           (: a Symbol)
           (define a b)
           (: b Symbol)
           (define b 'x)
           a)
         -Symbol]

       [tc-e
         (let ()
           (: x Integer)
           (define x
             (let ()
               (: f (Integer -> Integer))
               (define (f y)
                 (g (g y)))
               (f 4)))
           (: g (Integer -> Integer))
           (define (g x)
             (* x x))
           'x)
         -Symbol
         #:expected (ret -Symbol)]

       [tc-e
         (ann (for/list ([z #"foobar"]) (add1 z)) (Listof Integer))
         (-lst -Int)]

       [tc-e
         (lambda (a . b) (apply values a b))

         #:ret (ret (-polydots (A B ...) (->... (list A) (B B) (-values-dots (list A) B 'B))))
         #:expected (ret (-polydots (A B ...) (->... (list A) (B B) (-values-dots (list A) B 'B))))
         ]
       [tc-e
         (tr:lambda (x #:y [y 3]) x)
         #:ret (ret (->key Univ #:y Univ #f Univ) -true-filter)
         #:expected (ret (->key Univ #:y Univ #f Univ) -no-filter)]
       [tc-err
         (lambda xs (plambda: (b) ([x : Any]) 3))
         #:ret (ret (-polydots (a) (->... (list) (a a) (-values-dots (list) a 'a))))
         #:expected (ret (-polydots (a) (->... (list) (a a) (-values-dots (list) a 'a))))]


       [tc-e
         (tr:lambda xs (tr:lambda (x) 
                         (apply values (map (tr:lambda (z) (tr:lambda (y) (symbol? x))) xs))))
         #:ret
           (ret (-polydots (a ...)
                  (->... (list) (a a)
                         (-values (list
                                   (t:-> Univ
                                         (-values-dots 
                                          (list)
                                          (t:-> Univ -Boolean 
                                                : (-FS (-filter -Symbol (list 1 0)) -top)) 'a)))))))
           #:expected
           (ret (-polydots (a ...)
                  (->... 
                   (list) (a a) 
                   (-values 
                    (list
                     (t:-> Univ 
                           (-values-dots 
                            (list) 
                            (t:-> Univ -Boolean : (-FS (-filter -Symbol (list 1 0)) -top)) 'a)))))))]

       [tc-err
         (inst (eval '3) Any)
         #:ret (ret -Bottom)]
       [tc-err
         (lambda xs (inst (apply values (plambda: (b) ([x : b]) x) xs) Symbol))
         #:ret (ret (-polydots (a ...) (->... (list) (a a)
                                              (-values-dots (list (t:-> -Symbol -Symbol)) a 'a))))
         #:expected (ret (-polydots (a ...) 
                                    (->... (list) (a a)
                                           (-values-dots (list (t:-> -Symbol -Symbol)) a 'a))))]

       [tc-err
         (lambda xs (andmap (lambda: ([x : (Vectorof Any)]) x) xs))
         #:ret (ret (-polydots (a ...) (->... (list) ((-vec a) a) (t:Un (-val #f) (-vec Univ)))))
         #:expected (ret (-polydots (a ...) (->... (list) ((-vec a) a)
                                                   (t:Un (-val #f) (-vec Univ)))))]
       [tc-err
         (lambda xs (andmap (lambda: ([x : #f]) x) xs))
         #:ret (ret (-polydots (a ...) (->... (list) ((-val #f) a) (-val #f))))
         #:expected (ret (-polydots (a ...) (->... (list) ((-val #f) a) (-val #f))))]

       [tc-e
        ((letrec ([lp (lambda (x) lp)]) lp) 'y)
        #:ret (ret (t:-> -Symbol Univ) -true-filter)
        #:expected (ret (t:-> -Symbol Univ) -no-filter -no-obj)]

       [tc-e
         (list (vector 1 2 3))
        #:ret (ret (-seq (-vec Univ)))
        #:expected (ret (-seq (-vec Univ)))]

       ;; PR 14557 - apply union of functions with different return values
       [tc-err
        (let ()
          (: f (U (-> Void) (-> (values Void Void))))
          (define (f) (void))
          (f))
        #:msg #rx"Expected the same number of values.*1 and 2"]
       [tc-e
        (let ()
          (: f (U (-> (values String Symbol)) (-> (values Void Void))))
          (define (f) (values "foo" 'bar))
          (f))
        #:ret (ret (list (t:Un -String -Void) (t:Un -Symbol -Void)))]
       [tc-e (syntax->datum #`(#,(lambda (x) x)))
             #:ret (ret Univ)]
       [tc-e (stx->list #'(a . b))
             #:ret (ret (t:Un (-lst (-Syntax Univ)) (-val #f)))]

       [tc-e/t
         (lambda (x)
           (define (g y)
               (unless (string? y)
                 (error 'bad-input))
               (eval y))
           (g x)
           x)
         (t:-> Univ -String
               : (-FS (-and (-filter -String '(0 0)) (-not-filter (-val #f) '(0 0))) -bot)
               : (make-Path null '(0 0)))]

       ;; PR 14576
       [tc-e
         (let: ([foo : (All (b ...) ((List (b ... b -> b) ... b) -> Void)) (lambda (x) (void))])
           (foo (list (λ: ([x : String] [y : Symbol]) x) (λ: ([x : String] [y : Symbol]) y))))
         -Void]

       ;; PR 14579
       [tc-e
        (let: ([foo : (All (b ...) ((List (List b ... b) ... b) -> (List (List b ... b) ... b)))
                 (lambda (x) x)])
          (foo (list (list "string"))))
        (-lst* (-lst* -String))]

       ;; PR 14577
       [tc-err
        (let: ([foo : (All (b ...) ((List (List b ... b) ... b) -> (List (List b ... b) ... b)))
                 (lambda (x) x)])
          (foo (list (list))))
        #:ret (ret -Bottom)]

       ;; PR 14580
       [tc-err
        (let: ([foo : (All (b ...) ((List (List b ... b) ... b) -> (List (List b ... b) ... b)))
                 (lambda (x) x)])
          (foo (list (list "string" 'symbol))))
        #:ret (ret (-lst* (-lst* -String)))
        #:expected (ret (-lst* (-lst* -String)))]

       ;; PR 13898
       [tc-err
        (let ()
          (: f ([#:foo Any] -> (Option Natural)))
          (tr:define (f #:foo x) 0)
          (error "dummy"))
        #:msg #rx"missing keyword arguments.*#:foo"]

       [tc-err
        (let ()
          (: f (-> [#:foo Any] Zero))
          (tr:define (f #:foo [foo 'foo] #:bar bar) 0)
          (error "dummy"))
        #:msg #rx"too many mandatory keyword arguments.*#:foo"]

       ;; PR 14583
       [tc-e
        (let ()
          (: f (-> String))
          (tr:define (f #:foo [foo 'foo]) "foo")
          (f))
        -String]
       [tc-err
        (let ()
          (: f (-> String))
          (tr:define (f #:foo [foo 'foo]) foo)
          (error "dummy"))
        #:msg #rx"expected: String.*given: 'foo"]

       ;; Make sure -Bottom and multiple values play nice together.
       [tc-e
        (let ()
          (define-values (a b) (error 'nyi))
          b)
        -Bottom]

       [tc-e
         ((if (even? 4) add1 (inst values Integer)) 4)
         -Int]

       ;; PR 14601
       [tc-err
        (let ()
          (: f Procedure)
          (define f (lambda () 'hi))
          (f))
        #:msg "function with unknown arity"]

       ;; PR 13259
       [tc-err
        (let ()
          (: y String)
          (define y (for/fold: ((x : String null)) ((v : String null)) x))
          y)
        #:ret (ret -String -true-filter)
        #:msg #rx"expected: String.*given: (Null|'\\(\\))"]

       ;; PR 14493
       [tc-err
        (let ()
          (define f values)
          (: g (Any -> Boolean))
          (define (g x) (f x))
          (error "foo"))
        #:msg #rx"Polymorphic function `f' could not be applied"]

       ;; PR 14509
       [tc-e
        (let ()
         (: f (-> Any (-> Any Boolean : #:+ (Number @ 1 0))))
         (define f (λ (x) (λ (y) (number? x))))

         (: g (-> Any (-> Boolean : #:+ (Number @ 1 0))))
         (define g (λ (x) (λ () (number? x))))
         (void))
        -Void]

       ;; with-handlers
       [tc-e
         (with-handlers ([exn:fail? (λ (exn) 4)])
            5)
         #:ret (ret -Nat -true-filter)
         #:expected (ret -Nat -no-filter)]
       [tc-e
         (with-handlers ([exn:fail? (λ (exn) #f)])
            5)
         #:ret (ret Univ -top-filter)
         #:expected (ret Univ -no-filter)]
       [tc-e
        (with-handlers ([void (λ: ([x : Any]) #t)]) #f)
        -Boolean]
       [tc-err
        (with-handlers ([values (lambda: ([e : String]) (string-append e "bar"))])
          (raise "foo"))
        #:msg #rx"expected: \\(-> Any Any\\).*given: \\(-> String String\\)"]
       [tc-err
        (with-handlers (["foo" (lambda: ([e : String]) (string-append e "bar"))])
          (raise "foo"))
        #:msg #rx"expected: \\(-> Any Any\\).*given: String"]
       [tc-err
        (with-handlers ([string? (lambda (e) (string-append e "bar"))])
          (raise "foo"))
        #:ret (ret -String)
        #:msg #rx"expected: String.*given: Any"]
       [tc-err
        (with-handlers ([string? (lambda: ([e : String]) (string-append e "bar"))]
                        [symbol? (lambda (x) (symbol->string x))])
          (raise 'foo))
        #:ret (ret -String)
        #:msg #rx"expected: Symbol.*given: Any"]
       
       [tc-err
        (raise (λ ([x : Number]) (add1 x)))]
       
       [tc-err
        (raise (exn:fail:syntax "" (current-continuation-marks)
                                (list (datum->syntax #f add1))))]

       ;; PR 14218
       [tc-e (ann (values "foo" "bar") (Values String String))
             #:ret (ret (list -String -String))]
       [tc-e (let ()
               (tr:define (foo) : (Values String String) (values "foo" "bar"))
               (void))
             -Void]

       ;; Make sure unannotated definitions with the wrong number of values
       ;; don't produce an internal error
       [tc-err (let () (define x (values 1 2)) (error "dummy"))
               #:msg #rx"Expression should produce 1 values"]
        )

  (test-suite
   "tc-literal tests"
   (tc-l 5 -PosByte)
   (tc-l -5 -NegFixnum)
   (tc-l 0 -Zero)
   (tc-l 0.0 -FlonumPosZero)
   (tc-l -0.0 -FlonumNegZero)
   (tc-l 5# -PosFlonum)
   (tc-l 5.0 -PosFlonum)
   (tc-l 5.1 -PosFlonum)
   (tc-l -5# -NegFlonum)
   (tc-l -5.0 -NegFlonum)
   (tc-l -5.1 -NegFlonum)
   (tc-l 0.0t0 -ExtFlonumPosZero)
   (tc-l -0.0t0 -ExtFlonumNegZero)
   (tc-l 5#t0 -PosExtFlonum)
   (tc-l 5.0t0 -PosExtFlonum)
   (tc-l 5.1t0 -PosExtFlonum)
   (tc-l -5#t0 -NegExtFlonum)
   (tc-l -5.0t0 -NegExtFlonum)
   (tc-l -5.1t0 -NegExtFlonum)
   (tc-l 1+1i -ExactNumber)
   (tc-l 1+1.0i -FloatComplex)
   (tc-l 1.0+1i -FloatComplex)
   (tc-l 1.0+1.1i -FloatComplex)
   (tc-l #t (-val #t))
   (tc-l "foo" -String)
   (tc-l foo (-val 'foo))
   (tc-l #:foo (-val '#:foo))
   (tc-l #f (-val #f))
   (tc-l #"foo" -Bytes)
   [tc-l () -Null]
   [tc-l (3 . 4) (-pair -PosByte -PosByte)]
   [tc-l #hash() (make-Hashtable Univ Univ)]
   [tc-l #hash((1 . 2) (3 . 4)) (make-Hashtable -Integer -Integer)]
   [tc-l #hasheq((a . q) (b . w)) (make-Hashtable -Symbol -Symbol)]
   [tc-l #hash{[:a . :b]}
         (let ([rec-type (-mu X (make-Hashtable (t:Un -Symbol X) (t:Un -Symbol X)))])
           (make-Hashtable (t:Un -Symbol rec-type) (t:Un -Symbol rec-type)))
         #:expected (-mu X (make-Hashtable (t:Un -Symbol X) (t:Un -Symbol X)))]
   [tc-l #hash{[:a . :b]}
         (make-Hashtable (-val ':a) (-val ':b))
         #:expected (t:Un (-val #f) (make-Hashtable (-val ':a) (-val ':b)))]
   [tc-l #(:a :b)
         (-vec (t:Un (-val ':a) (-val ':b) (-mu X (-vec (t:Un (-val ':a) (-val ':b) X)))))
         #:expected (-mu X (-vec (t:Un (-val ':a) (-val ':b) X)))]
   [tc-l (#(:a) . :b)
         (-pair (-vec (t:Un (-val ':a)
                            (-mu X (-pair (-vec (t:Un (-val ':a) X)) (t:Un (-val ':b) X)))))
                (-val ':b))
         #:expected (-mu X (-pair (-vec (t:Un (-val ':a) X)) (t:Un (-val ':b) X)))]
   [tc-l/err #(1 2) #:expected (make-HeterogeneousVector (list -Number -Symbol))
                    #:msg #rx"expected: Symbol"]
   )
  ))
