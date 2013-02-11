#lang racket/base

(require (for-syntax typed-racket/env/global-env) typed-racket/env/global-env
         (for-template typed-racket/env/global-env)
         (for-meta 2 typed-racket/env/global-env))
(require "test-utils.rkt"
         (for-syntax racket/base)
         (for-template racket/base))
(require (private type-annotation parse-type)
         (except-in 
          (base-env prims
                    base-types-extra
                    base-env-indexing base-structs)
          define lambda λ)
         (typecheck typechecker)
         (rep type-rep filter-rep object-rep)
         (rename-in (types utils union numeric-tower abbrev filter-ops)
                    [Un t:Un]
                    [true-lfilter -true-lfilter]
                    [true-filter -true-filter]
                    [-> t:->])
         (utils tc-utils utils)
         (utils mutated-vars)
         (env type-name-env type-env-structs init-envs mvar-env)
         rackunit rackunit/text-ui
         syntax/parse
         (for-syntax (utils tc-utils) racket/file racket/port
                     (typecheck typechecker)
                     (env global-env)
                     (base-env base-env-indexing))
         racket/file racket/port racket/flonum
         (env global-env)
         (for-meta 2 (env global-env))
         (for-template
          racket/file racket/port
            (base-env base-types base-types-extra base-env-indexing))
         (for-syntax syntax/kerncase syntax/parse))

(require (prefix-in b: (base-env base-env))
         (prefix-in n: (base-env base-env-numeric)))

(provide typecheck-tests g tc-expr/expand)

(b:init) (n:init) (initialize-structs) (initialize-indexing) 
(dynamic-require '(submod typed-racket/base-env/base-types #%type-decl) #f)

(define N -Number)
(define B -Boolean)
(define Sym -Symbol)
(define -Pos -PosInt)
(define R -Real)

(define (g) (run typecheck-tests))

(define-namespace-anchor anch)

(define (-path t var [p null])
  (ret t
       (-FS (make-NotTypeFilter (-val #f) p var)
            (make-TypeFilter (-val #f) p var))
       (make-Path p var)))


;; check that a literal typechecks correctly
(define-syntax tc-l
  (syntax-rules ()
    [(_ lit ty)
     (check-type-equal? (format "~s" 'lit) (tc-literal #'lit) ty)]))

;; local-expand and then typecheck an expression
(define-syntax (tc-expr/expand/values stx)
  (syntax-case stx ()
    [(_ e)
     #`(parameterize ([delay-errors? #f]
                      [current-namespace (namespace-anchor->namespace anch)]
                      [orig-module-stx (quote-syntax e)])
         (let ([ex (expand 'e)])
	   (find-mutated-vars ex mvar-env)
           (values (lambda () (tc-expr ex)) ex)))]))

(define-syntax (tc-expr/expand stx)
  (syntax-case stx ()
    [(_ e)
     #`(parameterize ([delay-errors? #f]
                      [current-namespace (namespace-anchor->namespace anch)]
                      [orig-module-stx (quote-syntax e)])
         (let ([ex (expand 'e)])
           (find-mutated-vars ex mvar-env)
           (tc-expr ex)))]))

;; check that an expression typechecks correctly
(define-syntax (tc-e stx)
  (syntax-case stx ()
    [(_ expr ty) (syntax/loc stx (tc-e expr #:ret (ret ty)))]
    [(_ expr #:proc p)
     (quasisyntax/loc stx
       (let-values ([(t e) (tc-expr/expand/values expr)])
         #,(quasisyntax/loc stx (check-tc-result-equal? (format "~a ~s" #,(syntax-line stx) 'expr) (t) (p e)))))]
    [(_ expr #:ret r)
     (quasisyntax/loc stx
       (check-tc-result-equal? (format "~a ~a" #,(syntax-line stx) 'expr) (tc-expr/expand expr) r))]
    [(_ expr ty f o) (syntax/loc stx (tc-e expr #:ret (ret ty f o)))]))

(define-syntax (tc-e/t stx)
  (syntax-parse stx
    [(_ e t) (syntax/loc stx (tc-e e #:ret (ret t (-FS -top -bot))))]))

;; duplication of the mzscheme toplevel expander, necessary for expanding the rhs of defines
;; note that this ability is never used
(define-for-syntax (local-expand/top-level form)
  (let ([form* (local-expand form 'module (kernel-form-identifier-list #'here))])
    (kernel-syntax-case form* #f
                        [(define-syntaxes . _) (raise-syntax-error "don't use syntax defs here!" form)]
                        [(define-values vals body)
                         (quasisyntax/loc form (define-values vals #,(local-expand #'body 'expression '())))]
                        [e (local-expand #'e 'expression '())])))

;; check that typechecking this expression fails
(define-syntax tc-err
  (syntax-rules ()
    [(_ expr)
     (test-exn (format "~a" 'expr)
               exn:fail:syntax?
               (lambda () (tc-expr/expand expr)))]))

(define-syntax-class (let-name n)
  #:literals (let-values)
  (pattern (let-values ([(i:id) _] ...) . _)
           #:with x (list-ref (syntax->list #'(i ...)) n)))

(define-syntax-rule (get-let-name id n e)
  (syntax-parser
   [p #:declare p (let-name n)
      #:with id #'p.x
      e]))

(define (typecheck-tests)
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
         N]
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
        (tc-e (- -4) -PosFixnum)
        (tc-e/t 1152921504606846975 -PosInt)
        (tc-e/t -1152921504606846975 -NegInt)
        (tc-e (- 3253463567262345623) -NegInt)
        (tc-e (- -23524623547234734568) -PosInt)
        (tc-e (- 241.3) -NegFlonum)
        (tc-e (- -24.3) -PosFlonum)

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
        (tc-e (expt -1/2 -1/2) N)
        (tc-e (flexpt 0.5 0.3) -NonNegFlonum)
        (tc-e (flexpt 0.00000000001 100000000000.0) -NonNegFlonum)
        (tc-e (flexpt -2.0 -0.5) -Flonum) ; NaN
        (tc-e (angle -1) -Real)
        (tc-e (angle 2.3) -Zero)
        (tc-e (magnitude 3/4) -NonNegRat)
        (tc-e (magnitude 3+2i) -NonNegReal)
        (tc-e (min (ann 3 Fixnum) (ann 3 Fixnum)) -Fixnum)
        (tc-e (min (ann -2 Negative-Fixnum) (ann 3 Fixnum)) -NegFixnum)
        (tc-e (min (ann 3 Fixnum) (ann -2 Negative-Fixnum)) -NegFixnum)
        (tc-e (exact->inexact (ann 3 Number)) (t:Un -InexactReal -InexactComplex))
        (tc-err (let: ([z : 10000000000000 10000000000000]) z)) ; unsafe
        (tc-err (let: ([z : -4611686018427387904 -4611686018427387904]) z)) ; unsafe
        (tc-e (let: ([z : -4611686018427387905 -4611686018427387905]) z) (-val -4611686018427387905))
        (tc-err (let: ([z : -1073741825 -1073741825]) z)) ; unsafe
        (tc-e (let: ([z : -1073741824 -1073741824]) z) (-val -1073741824))
        (tc-e (let: ([z : 268435455 268435455]) z) (-val 268435455))
        (tc-err (let: ([z : 268435456 268435456]) z)) ; unsafe
        (tc-err (let: ([z : 4611686018427387903 4611686018427387903]) z)) ; unsafe
        (tc-e (let: ([z : 4611686018427387904 4611686018427387904]) z) (-val 4611686018427387904))

        [tc-e/t (lambda: () 3) (t:-> -PosByte : -true-lfilter)]
        [tc-e/t (lambda: ([x : Number]) 3) (t:-> N -PosByte : -true-lfilter)]
        [tc-e/t (lambda: ([x : Number] [y : Boolean]) 3) (t:-> N B -PosByte : -true-lfilter)]
        [tc-e/t (lambda () 3) (t:-> -PosByte : -true-lfilter)]
        [tc-e (values 3 4) #:ret (ret (list -PosByte -PosByte) (list -true-filter -true-filter))]
        [tc-e (cons 3 4) (-pair -PosByte -PosByte)]
        [tc-e (cons 3 (ann '() : (Listof Integer))) (make-Listof -Integer)]
        [tc-e (void) -Void]
        [tc-e (void 3 4) -Void]
        [tc-e (void #t #f '(1 2 3)) -Void]
        [tc-e/t #(3 4 5) (make-HeterogeneousVector (list -Integer -Integer -Integer))]
        [tc-e/t '(2 3 4) (-lst* -PosByte -PosByte -PosByte)]
        [tc-e/t '(2 3 #t) (-lst* -PosByte -PosByte (-val #t))]
        [tc-e/t #(2 3 #t) (make-HeterogeneousVector (list -Integer -Integer -Boolean))]
        [tc-e (vector 2 "3" #t) (make-HeterogeneousVector (list -Integer -String -Boolean))]
        [tc-e (vector-immutable 2 "3" #t) (make-HeterogeneousVector (list -Integer -String -Boolean))]
        [tc-e (make-vector 4 1) (-vec -Integer)]
        [tc-e (build-vector 4 (lambda (x) 1)) (-vec -Integer)]
        [tc-e (range 4) (-lst -Byte)]
        [tc-e (range 2 4 1) (-lst -PosByte)]
        [tc-e (range 0 4 1) (-lst -Byte)]
        [tc-e (range 0.0 4/2 0.5) (-lst -Flonum)]
        [tc-e/t '(#t #f) (-lst* (-val #t) (-val #f))]
        [tc-e/t (plambda: (a) ([l : (Listof a)]) (car l))
                (make-Poly '(a) (t:-> (make-Listof (-v a)) (-v a)))]
        [tc-e/t (plambda: (a) ([l : (Listof a)]) (car l))
                (make-Poly '(a) (t:-> (make-Listof (-v a)) (-v a)))]
        [tc-e/t (case-lambda: [([a : Number] [b : Number]) (+ a b)]) (t:-> N N N)]
        [tc-e (let: ([x : Number 5]) x) N]
        [tc-e (let-values ([(x) 4]) (+ x 1)) -PosIndex]
        [tc-e (let-values ([(#{x : Number} #{y : Boolean}) (values 3 #t)]) (and (= x 1) (not y)))
              #:proc (syntax-parser [(_ ([(_ y) . _]) . _) (ret -Boolean (-FS -top -top))])]
        [tc-e/t (values 3) -PosByte]
        [tc-e (values) #:ret (ret null)]
        [tc-e (values 3 #f) #:ret (ret (list -PosByte (-val #f)) (list (-FS -top -bot) (-FS -bot -top)))]
        [tc-e (map #{values @ Symbol} '(a b c)) (-pair Sym (make-Listof  Sym))]
        [tc-e (andmap add1 (ann '() (Listof Number))) (t:Un (-val #t) N)]
        [tc-e (ormap add1 (ann '() (Listof Number))) (t:Un (-val #f) N)]
        [tc-e (letrec: ([fact : (Number -> Number) (lambda: ([n : Number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                       (fact 20))
              N]
        [tc-e (let: fact : Number ([n : Number 20])
                    (if (zero? n) 1 (* n (fact (- n 1)))))
              N]
        [tc-e (let: ([v : Any 5])
                    (if (number? v) (+ v 1) 3))
              N]
        [tc-e (let: ([v : Any #f])
                    (if (number? v) (+ v 1) 3))
              N]
        [tc-e (let: ([v : (Un Number Boolean) #f])
                    (if (boolean? v) 5 (+ v 1)))
              #:proc (get-let-name v 0 (ret N (-FS -top -top)))]
        [tc-e (let: ([f : (Number Number -> Number) +]) (f 3 4)) N]
        [tc-e (let: ([+ : (Boolean -> Number) (lambda: ([x : Boolean]) 3)]) (+ #f)) N]
        [tc-e (when #f #t) -Void]
        [tc-e (when (number? #f) (+ 4 5)) -Void]
        [tc-e (let: ([x : (Un #f Number) 7])
                    (if x (+ x 1) 3))
              N]
        [tc-e (let: ([x : Number 1])
                    (if (and (number? x) #t)
                        (+ x 4)
                        'bc))
              N]
        [tc-e/t (let: ((x : Number 3)) (if (boolean? x) (not x) #t)) (-val #t)]
        [tc-e/t (begin 3) -PosByte]
        [tc-e/t (begin #f 3) -PosByte]
        [tc-e/t (begin #t) (-val #t)]
        [tc-e/t (begin0 #t) (-val #t)]
        [tc-e/t (begin0 #t 3) (-val #t)]
        [tc-e/t #t (-val #t)]
        [tc-e #f #:ret (ret (-val #f) (-FS -bot -top))]
        [tc-e/t '#t (-val #t)]
        [tc-e '#f #:ret (ret (-val #f) (-FS -bot -top))]
        [tc-e/t (if #f 'a 3) -PosByte]
        [tc-e/t (if #f #f #t) (t:Un (-val #t))]
        [tc-e (when #f 3) -Void]
        [tc-e/t '() (-val '())]
        [tc-e/t (let: ([x : (Listof Number) '(1)])
                      (cond [(pair? x) 1]
                            [(null? x) 1]))
              -One]
        [tc-e/t (lambda: ([x : Number] . [y : Number *]) (car y)) (->* (list N) N N)]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3) N]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3 4 5) N]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3 4) N]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '(4)) N]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '(4 6 7)) N]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '()) N]

        [tc-e/t (lambda: ([x : Number] . [y : Boolean *]) (car y)) (->* (list N) B B)]
        [tc-e ((lambda: ([x : Number] . [y : Boolean *]) (car y)) 3) B]
        [tc-e (apply (lambda: ([x : Number] . [y : Boolean *]) (car y)) 3 '(#f)) B]

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
              N]

        [tc-e (let ([x 1]) x) -One]
        [tc-e (let ([x 1]) (boolean? x)) #:ret (ret -Boolean (-FS -bot -top))]
        [tc-e (boolean? number?) #:ret (ret -Boolean (-FS -bot -top))]

        [tc-e (let: ([x : (Option Number) #f]) x) (t:Un N (-val #f))]
        [tc-e (let: ([x : Any 12]) (not (not x))) -Boolean]

        [tc-e (let: ([x : (Option Number) #f])
                    (if (let ([z 1]) x)
                        (add1 x)
                        12))
              N]
        [tc-err (5 4)]
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
        [tc-e ((inst add-between Positive-Byte Symbol) '(1 2 3) 'a #:splice? #t #:before-first '(b)) (-lst (t:Un -PosByte -Symbol))]

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

        [tc-e (let: ([x : Any #f])
                    (if (number? (let ([z 1]) x))
                        (add1 x)
                        12))
              N]

        [tc-e (let: ([x : (Option Number) #f])
                    (if x
                        (add1 x)
                        12))
              N]


        [tc-e null #:ret (-path (-val null) #'null)]

        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                x)
              (t:Un (-val 'squarf) -PosByte)]

        [tc-e/t (if #t 1 2) -One]


        ;; eq? as predicate
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (eq? x 'foo) 3 x))
              #:proc (get-let-name x 0 (ret N (-FS -top -top)))]
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (eq? 'foo x) 3 x))
              #:proc (get-let-name x 0 (ret N (-FS -top -top)))]

        [tc-err (let: ([x : (U String 'foo) 'foo])
                      (if (string=? x 'foo)
                          "foo"
                          x))]
        #;[tc-e (let: ([x : (U String 5) 5])
                      (if (eq? x 5)
                          "foo"
                          x))
                (t:Un -String (-val 5))]

        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? x sym) 3 x))
              #:proc (syntax-parser [(_ _ (_ ([(x) _]) _))
                                     (ret -PosByte (-FS -top -top))])]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? sym x) 3 x))
               #:proc (syntax-parser [(_ _ (_ ([(x) _]) _))
                                      (ret -PosByte (-FS -top -top))])]
        ;; equal? as predicate for symbols
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (equal? x 'foo) 3 x))
                #:proc (get-let-name x 0 (ret N (-FS -top -top)))]
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (equal? 'foo x) 3 x))
                #:proc (get-let-name x 0 (ret N (-FS -top -top)))]

        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? x sym) 3 x))
               #:proc (syntax-parser [(_ _ (_ ([(x) _]) _))
                                      (ret -PosByte (-FS -top -top))])]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? sym x) 3 x))
              #:proc (syntax-parser [(_ _ (_ ([(x) _]) _))
                                     (ret -PosByte (-FS -top -top))])]

        [tc-e (let: ([x : (Listof Symbol)'(a b c)])
                    (cond [(memq 'a x) => car]
                          [else 'foo]))
              Sym]

        [tc-e (list 2 3 4) (-lst* -PosByte -PosByte -PosByte)]
        [tc-e (list 2 3 4 'a) (-lst* -PosByte -PosByte -PosByte (-val 'a))]

        [tc-e `(1 2 ,(+ 3 4)) (-lst* -One -PosByte -PosIndex)]

        [tc-e (let: ([x : Any 1])
                    (when (and (list? x) (not (null? x)))
                      (car x)))
              Univ]

        [tc-err (let: ([x : Any 3])
                      (car x))]
        [tc-err (car #{3 : Any})]
        [tc-err (map #{3 : Any} #{12 : Any})]
        [tc-err (car 3)]

        [tc-e/t (let: ([x : Any 1])
                  (if (and (list? x) (not (null? x)))
                      x
                      'foo))
                (t:Un (-val 'foo) (-pair Univ (-lst Univ)))]

        [tc-e (cadr (cadr (list 1 (list 1 2 3) 3))) -PosByte]



        ;;; tests for and
        [tc-e (let: ([x : Any 1]) (and (number? x) (boolean? x)))
              #:ret (ret B (-FS -bot -top))]
        [tc-e (let: ([x : Any 1]) (and (number? x) x))
              #:proc (get-let-name x 0 (ret (t:Un N (-val #f)) (-FS -top -top)))]
        [tc-e (let: ([x : Any 1]) (and x (boolean? x)))
              #:proc (get-let-name x 0 (ret -Boolean (-FS -top -top)))]

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

        #;
        [tc-e (let: ([x : Any 3])
                    (if (or (boolean? x) (number? x))
                        (if (boolean? x) 12 x)
                        47))
              Univ]

        ;; test for fake or
        [tc-e (let: ([x : Any 1])
                    (if (if (number? x)
                            #t
                            (boolean? x))
                        (if (boolean? x) 1 (+ 1 x))
                        4))
              N]
        ;; these don't invoke the or rule
        [tc-e (let: ([x : Any 1]
                     [y : Any 12])
                    (if (if (number? x)
                            #t
                            (boolean? y))
                        (if (boolean? x) 1 x)
                        4))
              #:ret (ret Univ (-FS -top -bot))]
        [tc-e (let: ([x : Any 1])
                    (if (if ((lambda: ([x : Any]) x) 12)
                            #t
                            (boolean? x))
                        (if (boolean? x) 1 x)
                        4))
              #:proc (get-let-name
                      x 0
                      (ret Univ
                           (-FS
                            -top
                            (-and (make-NotTypeFilter -Boolean null #'x) (make-TypeFilter (-val #f) null #'x)))))]

        ;; T-AbsPred
        [tc-e/t (let ([p? (lambda: ([x : Any]) (number? x))])
                  (lambda: ([x : Any]) (if (p? x) (add1 x) (add1 12))))
                (t:-> Univ N)]
        [tc-e/t (let ([p? (lambda: ([x : Any]) (not (number? x)))])
                  (lambda: ([x : Any]) (if (p? x) 12 (add1 x))))
                (t:-> Univ N : (-FS -top (-filter -Number 0)))]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) (number? z))])
                  (lambda: ([x : Any]) (if (p? x) 11 12)))
                (t:-> Univ -PosByte : -true-lfilter)]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) (number? z))])
                  (lambda: ([x : Any]) (if (p? x) x 12)))
                (t:-> Univ Univ : (-FS  (-not-filter (-val #f) 0) (-filter (-val #f) 0)))
                #;(make-pred-ty (list Univ) Univ (-val #f) 0 null)]
        [tc-e/t (let* ([z (ann 1 : Any)]
                       [p? (lambda: ([x : Any]) (not (number? z)))])
                  (lambda: ([x : Any]) (if (p? x) (ann (add1 7) Any) 12)))
                (t:-> Univ Univ)]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) (not (number? z)))])
                  (lambda: ([x : Any]) (if (p? x) x 12)))
                (t:-> Univ -PosByte : -true-lfilter)]
        [tc-e/t (let* ([z 1]
                       [p? (lambda: ([x : Any]) z)])
                  (lambda: ([x : Any]) (if (p? x) x 12)))
                (t:-> Univ Univ)]

        [tc-e (not 1) #:ret (ret B (-FS -bot -top))]

        [tc-err ((lambda () 1) 2)]
        [tc-err (apply (lambda () 1) '(2))]
        [tc-err ((lambda: ([x : Any] [y : Any]) 1) 2)]
        [tc-err (map map '(2))]
        [tc-err ((plambda: (a) ([x : (a -> a)] [y : a]) (x y)) 5)]
        [tc-err ((plambda: (a) ([x : a] [y : a]) x) 5)]
        [tc-err (ann 5 : String)]

        ;; these don't work because the type annotation gets lost in marshalling
        #|
        [tc-e (letrec-syntaxes+values () ([(#{x : Number}) (values 1)]) (add1 x)) N]
        [tc-e (letrec-values ([(#{x : Number}) (values 1)]) (add1 x)) N]
        [tc-e (letrec ([#{x : Number} (values 1)]) (add1 x)) N]
        |#

        [tc-e (letrec: ([x : Number (values 1)]) (add1 x)) N]

        [tc-err (let ([x (add1 5)])
                  (set! x "foo")
                  x)]
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



        ;; call-with-values

        [tc-e (call-with-values (lambda () (values 1 2))
                                (lambda: ([x : Number] [y : Number]) (+ x y)))
              N]
        [tc-e (call-with-values (lambda () 1)
                                (lambda: ([x : Number]) (+ x 1)))
              N]
        [tc-err (call-with-values (lambda () 1)
                                  (lambda: () 2))]

        [tc-err (call-with-values (lambda () (values 2))
                                  (lambda: ([x : Number] [y : Number]) (+ x y)))]
        [tc-err (call-with-values 5
                                  (lambda: ([x : Number] [y : Number]) (+ x y)))]
        [tc-err (call-with-values (lambda () (values 2))
                                  5)]
        [tc-err (call-with-values (lambda () (values 2 1))
                                  (lambda: ([x : String] [y : Number]) (+ x y)))]
        ;; quote-syntax
        [tc-e/t #'3 (-Syntax -PosByte)]
        [tc-e/t #'(2 3 4) (-Syntax (-lst* -PosByte -PosByte -PosByte))]

        ;; testing some primitives
        [tc-e (let ([app apply]
                    [f (lambda: [x : Number *] 3)])
                (app f (list 1 2 3)))
              -PosByte]
        [tc-e ((lambda () (call/cc (lambda: ([k : (Number -> (U))]) (if (read) 5 (k 10))))))
              N]

        [tc-e (number->string 5) -String]

        [tc-e (let-values ([(a b) (quotient/remainder 5 12)]
                           [(a*) (quotient 5 12)]
                           [(b*) (remainder 5 12)])
                (+ a b a* b*))
              -Nat]

        [tc-e (raise-type-error 'foo "bar" 5) (t:Un)]
        [tc-e (raise-type-error 'foo "bar" 7 (list 5)) (t:Un)]

        #;[tc-e
           (let ((x '(1 3 5 7 9)))
             (do: : Number ((x : (list-of Number) x (cdr x))
                            (sum : Number 0 (+ sum (car x))))
                  ((null? x) sum)))
           N]


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
                      (let*-values ([(#{z : Number} #{w : Number}) (values (g (f x)) 5)])
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

        [tc-err (let ()
                  (define (x) (y))
                  (define (y) 3)
                  1)]

        [tc-e ((case-lambda:
                [[x : Number *] (+ 1 (car x))])
               5)
              N]
        #;
        [tc-e `(4 ,@'(3)) (-pair N (-lst N))]

        [tc-e
         (let ((x '(1 3 5 7 9)))
           (do: : Number ((x : (Listof Number) x (cdr x))
                          (sum : Number 0 (+ sum (car x))))
                ((null? x) sum)))
         #:ret (ret N (-FS -top -top) (make-NoObject))]

        [tc-e/t (if #f 1 'foo) (-val 'foo)]

        [tc-e (list* 1 2 3) (-pair -One (-pair -PosByte -PosByte))]

        [tc-err (apply append (list 1) (list 2) (list 3) (list (list 1) "foo"))]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list 1))) (-lst -PosByte)]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list "foo"))) (-lst (t:Un -String -PosByte))]
        [tc-err (plambda: (b ...) [y : b ... b] (apply append (map list y)))]
        [tc-e/t (plambda: (b ...) [y : (Listof Integer) ... b] (apply append y))
                (-polydots (b) (->... (list) ((-lst -Integer) b) (-lst -Integer)))]

        [tc-err (plambda: (a ...) ([z : String] . [w : Number ... a])
                          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                                 1 1 1 1 w))]

        [tc-err (plambda: (a ...) ([z : String] . [w : Number])
                          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                                 1 w))]

        [tc-e/t (plambda: (a ...) ([z : String] . [w : Number ... a])
                        (apply (plambda: (b ...) ([x : Number] . [y : Number ... b]) x)
                               1 w))
              (-polydots (a) ((list -String) (N a) . ->... . N))]
        [tc-e/t (let ([f (plambda: (a ...) [w : a ... a] w)])
                  (f 1 "hello" #\c))
                (-pair -One (-pair -String (-pair -Char (-val null))))]
        ;; instantiating non-dotted terms
        [tc-e/t (inst (plambda: (a) ([x : a]) x) Integer)
                (make-Function (list (make-arr* (list -Integer) -Integer
                                                #:filters (-FS (-not-filter (-val #f) 0)
                                                               (-filter (-val #f) 0))
                                                #:object (make-Path null 0))))]
        [tc-e/t (inst (plambda: (a) [x : a *] (apply list x)) Integer)
                ((list) -Integer . ->* . (-lst -Integer))]

        ;; instantiating dotted terms
        [tc-e/t (inst (plambda: (a ...) [xs : a ... a] 3) Integer Boolean Integer)
                (-Integer B -Integer . t:-> . -PosByte : -true-lfilter)]
        [tc-e/t (inst (plambda: (a ...) [xs : (a ... a -> Integer) ... a] 3) Integer Boolean Integer)
                ((-Integer B -Integer . t:-> . -Integer)
                 (-Integer B -Integer . t:-> . -Integer)
                 (-Integer B -Integer . t:-> . -Integer)
                 . t:-> . -PosByte : -true-filter)]

        [tc-e/t (plambda: (z x y ...) () (inst map z x y ... y))
              (-polydots (z x y) (t:-> (cl->*
                                        ((t:-> x z) (-pair x (-lst x)) . t:-> . (-pair z (-lst z)))
                                        ((list ((list x) (y y) . ->... . z) (-lst x)) ((-lst y) y) . ->... . (-lst z)))
                                       : (-FS (-not-filter (-val #f) #'map) (-filter (-val #f) #'map))))]

        ;; error tests
        [tc-err (+ 3 #f)]
        [tc-err (let: ([x : Number #f]) x)]
        [tc-err (let: ([x : Number #f]) (+ 1 x))]

        [tc-err
         (let: ([x : Any '(foo)])
               (if (null? x) 1
                   (if (list? x)
                       (add1 x)
                       12)))]

        [tc-err (let*: ([x : Any 1]
                        [f : (-> Void) (lambda () (set! x 'foo))])
                       (if (number? x)
                           (begin (f) (add1 x))
                           12))]

        [tc-err (ann 3 (Rec a a))]
        [tc-err (ann 3 (Rec a (U a 3)))]
        [tc-err (ann 3 (Rec a (Rec b a)))]

        #;
        [tc-err (lambda: ([x : Any])
                         (if (number? (not (not x)))
                             (add1 x)
                             12))]

        [tc-e (filter exact-integer? (list 1 2 3 'foo))
              (-lst -Integer)]

        [tc-e (filter even? (filter exact-integer? (list 1 2 3 'foo)))
              (-lst -Integer)]

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
                (-polydots (a) ((list) (a a) . ->... . -Integer))]|#

        ;; First is same as second, but with map explicitly instantiated.
        [tc-e/t (plambda: (a ...) [ys : (a ... a -> Number) *]
                (lambda: [zs : a ... a]
                  ((inst map Number (a ... a -> Number))
                   (lambda: ([y : (a ... a -> Number)])
                     (apply y zs))
                   ys)))
              (-polydots (a) ((list) ((list) (a a) . ->... . N) . ->* . ((list) (a a) . ->... . (-lst N)) : -true-lfilter))]
        [tc-e/t (plambda: (a ...) [ys : (a ... a -> Number) *]
                (lambda: [zs : a ... a]
                  (map (lambda: ([y : (a ... a -> Number)])
                         (apply y zs))
                       ys)))
              (-polydots (a) ((list) ((list) (a a) . ->... . N) . ->* . ((list) (a a) . ->... . (-lst N)) : -true-lfilter))]

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
                (N N N . t:-> . N)]

        [tc-e (assq 'foo #{'((a b) (foo bar)) :: (Listof (List Symbol Symbol))})
              (t:Un (-val #f) (-pair Sym (-pair Sym (-val null))))]

        [tc-e/t (ann (lambda (x) x) (All (a) (a -> a)))
                (-poly (a) (a . t:-> . a))]
        [tc-e (apply values (list 1 2 3)) #:ret (ret (list -One -PosByte -PosByte))]

        [tc-e/t (ann (if #t 3 "foo") Integer) -Integer]

        [tc-e/t (plambda: (a ...) ([x : Number] . [y : a ... a])
                          (andmap null? (map list y)))
                (-polydots (a) ((list -Number) (a a) . ->... . -Boolean))]
        [tc-e (ann (error 'foo) (values Number Number)) #:ret (ret (list -Number -Number))]

        [tc-e (string->number "123")
              (t:Un (-val #f) -Number)]

        [tc-e #{(make-hash) :: (HashTable Number Number)}
              (make-Hashtable -Number -Number)]
        #;[tc-err (let: ([fact : (Number -> Number) (lambda: ([n : Number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                        (fact 20))]

        [tc-err (ann (lambda: ([x : Any]) #f) (Any -> Boolean : String))]


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
         (call-with-values (lambda () ((inst time-apply Number Number Number Number Number Number Number) + (list 1 2 3 4 5 6)))
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
              #:ret (ret -One (-FS -top -top))]
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
        [tc-err (string-append "bar" (if (zero? (ann 0.0 Float)) #f "foo"))]
        [tc-err (do: : Void
                     ([j : Natural (+ i 'a) (+ j i)])
                     ((>= j 10))
                     #f)]
        [tc-err (apply +)]
        [tc-e/t
         (let ([x eof])
           (if (procedure? x)
               x
               (lambda (z) (eq? x z))))
         (make-pred-ty (-val eof))]
        [tc-e ((inst map Number (Pairof Number Number)) car (ann (list (cons 1 2) (cons 2 3) (cons 4 5)) (Listof (Pairof Number Number))))
              (-lst -Number)]
        [tc-err (list (values 1 2))]

 
        ;;Path tests
        (tc-e (path-string? "foo") B)
        (tc-e (path-string? (string->path "foo")) #:ret (ret B (-FS -top -bot)))
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
        (tc-e (string? "a") #:ret (ret B (-FS -top -bot)))
        (tc-e (string? 2) #:ret (ret B (-FS -bot -top)))

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

        (tc-e (string=? "a" "a") B)
        (tc-e (string<? "a" "a") B)
        (tc-e (string>? "a" "a") B)
        (tc-e (string<=? "a" "a") B)
        (tc-e (string>=? "a" "a") B)

        (tc-e (string-upcase "a") -String)
        (tc-e (string-downcase "a") -String)
        (tc-e (string-titlecase "a") -String)
        (tc-e (string-foldcase "a") -String)


        (tc-e (string-ci=? "a" "A") B)
        (tc-e (string-ci<? "a" "A") B)
        (tc-e (string-ci>? "a" "A") B)
        (tc-e (string-ci<=? "a" "A") B)
        (tc-e (string-ci>=? "a" "A") B)


        (tc-e (string-normalize-nfd "a") -String)
        (tc-e (string-normalize-nfkd "a") -String)
        (tc-e (string-normalize-nfc "a") -String)
        (tc-e (string-normalize-nfkc "a") -String)



        (tc-e (string-locale=? "a" "a") B)
        (tc-e (string-locale<? "a" "a") B)
        (tc-e (string-locale>? "a" "a") B)

        (tc-e (string-locale-upcase "a") -String)
        (tc-e (string-locale-downcase "a") -String)


        (tc-e (string-locale-ci=? "a" "A") B)
        (tc-e (string-locale-ci<? "a" "A") B)
        (tc-e (string-locale-ci>? "a" "A") B)

        ;Symbols

        (tc-e (symbol? 'foo) #:ret (ret B (-FS -top -bot)))
        (tc-e (symbol? 2) #:ret (ret B (-FS -bot -top)))

        (tc-e (symbol-interned? 'foo) B)
        (tc-e (symbol-interned? (string->unreadable-symbol "bar")) B)
        (tc-e (symbol-interned? (string->uninterned-symbol "bar")) B)
        (tc-e (symbol-interned? (gensym 'foo)) B)

        (tc-e (symbol-unreadable? (gensym)) B)
        (tc-e (symbol-unreadable? 'foo) B)
        (tc-e (string->unreadable-symbol "bar") -Symbol)
        (tc-e (string->uninterned-symbol "bar") -Symbol)

        (tc-e (symbol->string 'foo) -String)
        (tc-e (string->symbol (symbol->string 'foo)) -Symbol)

        ;Booleans
        (tc-e (not #f) #:ret (ret B (-FS -top -bot)))
        (tc-e (false? #f) #:ret (ret B (-FS -top -bot)))
        (tc-e (not #t) #:ret (ret B (-FS -bot -top)))
        ;; It's not clear why the following test doesn't work,
        ;; but it works fine in the real typechecker
        ;(tc-e (false? #t) #:ret (ret B (-FS -bot -top)))


        (tc-e (boolean? true) #:ret (ret B (-FS -top -bot)))
        (tc-e (boolean? 6) #:ret (ret B (-FS -bot -top)))
        (tc-e (immutable? (cons 3 4)) B)

        (tc-e (boolean=? #t false) B)
        (tc-e (symbol=? 'foo 'foo) B)

        (tc-e (equal? 1 2) B)
        (tc-e (eqv? 1 2) B)
        (tc-e (eq? 1 2) B)
        (tc-e (equal?/recur 'foo 'bar eq?) B)



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
        (tc-e (regexp-match "foo" (open-input-string "tmp")) (-opt (-pair -Bytes (-lst (-opt -Bytes)))))
        (tc-e (regexp-match #"foo" (open-input-string "tmp")) (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-err (regexp-try-match "foo" "foobar"))
        (tc-e (regexp-try-match "foo" (open-input-string "foobar")) (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-err (regexp-match-peek "foo" "foobar"))
        (tc-e (regexp-match-peek "foo" (open-input-string "foobar")) (-opt (-pair -Bytes (-lst (-opt -Bytes)))))

        (tc-err (regexp-match-peek-immediate "foo" "foobar"))
        (tc-e (regexp-match-peek-immediate "foo" (open-input-string "foobar")) (-opt (-pair -Bytes (-lst (-opt -Bytes)))))



        [tc-e (regexp-match/end "foo" "foobar") #:ret (ret (list (-opt (-pair -String (-lst (-opt -String)))) (-opt -Bytes)) (list (-FS -top -top) (-FS -top -top)))]

        (tc-e (regexp-split "foo" "foobar") (-pair -String (-lst -String)))
        (tc-e (regexp-split "foo" #"foobar") (-pair -Bytes (-lst -Bytes)))
        (tc-e (regexp-split #"foo" "foobar") (-pair -Bytes (-lst -Bytes)))
        (tc-e (regexp-split #"foo" #"foobar") (-pair -Bytes (-lst -Bytes)))

        (tc-err (regexp-split "foo" (path->string "foobar")))

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

        (tc-e (file-exists? "/usr") B)
        (tc-e (link-exists? "/usr") B)
        (tc-e (delete-file "does-not-exist") -Void)

        (tc-e (rename-file-or-directory "old" "new") -Void)
        (tc-e (rename-file-or-directory "old" "new" #t) -Void)

        (tc-e (file-or-directory-modify-seconds "dir") -NonNegFixnum)
        (tc-e (file-or-directory-modify-seconds "dir" #f) -NonNegFixnum)
        (tc-e (file-or-directory-modify-seconds "dir" 20) -Void)
        (tc-e (file-or-directory-modify-seconds "dir" #f (lambda () "error")) (t:Un -NonNegFixnum -String))
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

        (tc-e (directory-exists? "e") B)
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
                            (if (eq? type 'dir) (values res #t) (values res 'ignored))) 'res)  (-val 'res))
        (tc-e (fold-files (lambda: ((p : Path) (type : Symbol) (res : 'res)) res) 'res "tmp" #f)  (-val 'res))

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

        (tc-e (syntax? #'id) #:ret (ret B (-FS -top -bot)))
        (tc-e (syntax? 2) #:ret (ret B (-FS -bot -top)))

        (tc-e (syntax-source #'here) Univ)
        (tc-e (syntax-line #'here) (-opt -PosInt))
        (tc-e (syntax-column #'here) (-opt -Nat))
        (tc-e (syntax-position #'here) (-opt -PosInt))
        (tc-e (syntax-span #'here) (-opt -Nat))


        ;Parameters        
        (tc-e (parameter-procedure=? current-input-port current-output-port) B)

        ;Namespaces
        (tc-e (namespace? 2) #:ret (ret B (-FS -bot -top)))
        (tc-e (namespace? (make-empty-namespace)) #:ret (ret B (-FS -top -bot)))

        (tc-e (namespace-anchor? 3) #:ret (ret B (-FS -bot -top)))
        (tc-e/t (lambda: ((x : Namespace-Anchor)) (namespace-anchor? x)) (t:-> -Namespace-Anchor B : -true-lfilter))


        (tc-e (variable-reference? 3) #:ret (ret B (-FS -bot -top)))
        (tc-e/t (lambda: ((x : Variable-Reference)) (variable-reference? x)) (t:-> -Variable-Reference  B : -true-lfilter))

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
        (tc-e (udp-addresses (udp-open-socket) #t) (list -String -NonNegFixnum -String -NonNegFixnum))

        ;Byte converters
        (tc-e (bytes-open-converter "UTF-8" "UTF-8") (-opt -Bytes-Converter))
        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert c #"abcde"))  (list -Bytes -Nat (one-of/c 'complete 'continues 'aborts 'error)))
        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert c #"abcde" 0 5 (make-bytes 10)))  (list -Nat -Nat (one-of/c 'complete 'continues 'aborts 'error)))

        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert-end c))  (list -Bytes (one-of/c 'complete 'continues)))

        (tc-e (let ()
               (define: c : Bytes-Converter (assert (bytes-open-converter "UTF-8" "UTF-8")))
               (bytes-convert-end c (make-bytes 10)))  (list -Nat (one-of/c 'complete 'continues)))

        ;Subprocess
        (tc-e (subprocess #f #f #f (string->path "/usr/bin/echo") "string" (string->path "path") #"bytes")
              (list
                -Subprocess
                -Input-Port
                -Output-Port
                -Input-Port))




        (tc-e (subprocess (current-output-port) (current-input-port) (current-error-port) (string->path "/usr/bin/echo") 'exact "arg")
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
              #:ret (ret B (-FS -top -bot)))


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
              #:ret (ret B (-FS -top -bot)))
        (tc-e (let: ((e : Compiled-Expression (compile #'(module + racket 2))))
                (compiled-module-expression? e)) B)

        ;Impersonator Property
        (tc-e (make-impersonator-property 'prop) (list -Impersonator-Property (t:-> Univ B) (t:-> Univ Univ)))
        (tc-e (let-values: ((((prop : Impersonator-Property) (pred : (Any -> Any)) (acc : (Any -> Any)))
                             (make-impersonator-property 'prop)))
               (impersonator-property? prop))
              #:ret (ret B (-FS -top -bot)))

        ;Security Guards
        (tc-e (make-security-guard (current-security-guard) (lambda args (void)) (lambda args (void))) -Security-Guard)
        (tc-e (let: ((s : Security-Guard (current-security-guard)))
                (security-guard? s))
              #:ret (ret B (-FS -top -bot)))


        ;Custodians
        (tc-e (make-custodian) -Custodian)
        (tc-e (let: ((c : Custodian (current-custodian)))
                (custodian? c))
              #:ret (ret B (-FS -top -bot)))
        (tc-e (let: ((c : (Custodian-Boxof Integer) (make-custodian-box (current-custodian) 1)))
                (custodian-box-value c)) -Int)

        ;Thread Groups
        (tc-e (make-thread-group) -Thread-Group)
        (tc-e (let: ((tg : Thread-Group (current-thread-group)))
                (thread-group? tg))
              #:ret (ret B (-FS -top -bot)))


        ;Inspector
        (tc-e (make-inspector) -Inspector)
        (tc-e (let: ((i : Inspector (current-inspector)))
                (inspector? i))
              #:ret (ret B (-FS -top -bot)))

        ;Continuation Prompt Tags ang Continuation Mark Sets
        ;; TODO: supporting default-continuation-prompt-tag means we need to
        ;;       specially handle abort-current-continuation in the type system
        ;(tc-e (default-continuation-prompt-tag) -Prompt-Tag)
        (tc-e (let: ((pt : (Prompt-Tagof Integer Integer) (make-continuation-prompt-tag)))
                   (continuation-marks #f pt)) -Cont-Mark-Set)
        (tc-e (let: ((set : Continuation-Mark-Set (current-continuation-marks)))
                   (continuation-mark-set? set)) #:ret (ret B (-FS -top -bot)))

        ;Logging
        (tc-e (make-logger 'name) -Logger)
        (tc-e (let: ((l : Logger (make-logger)))
                (let: ((lr : Log-Receiver (make-log-receiver l 'error)))
                 (log-message l 'error "Message" 'value))) -Void)

        ;Semaphores
        (tc-e (make-semaphore) -Semaphore)
        (tc-e (let: ((s : Semaphore (make-semaphore 3)))
                      (semaphore-post s)) -Void)

        ;Random Numbers
        (tc-e (make-pseudo-random-generator) -Pseudo-Random-Generator)
        (tc-e (let: ((pg : Pseudo-Random-Generator (make-pseudo-random-generator)))
                (pseudo-random-generator->vector pg)) (make-HeterogeneousVector (list -PosInt -PosInt -PosInt -PosInt -PosInt -PosInt)))

        ;Structure Type Properties
        (tc-e (make-struct-type-property 'prop) (list -Struct-Type-Property (t:-> Univ B) (t:-> Univ Univ)))
        (tc-e (let-values: ((((prop : Struct-Type-Property) (pred : (Any -> Any)) (acc : (Any -> Any)))
                             (make-struct-type-property 'prop)))
               (struct-type-property? prop))
              #:ret (ret B (-FS -top -bot)))

        ;Wills
        (tc-e (make-will-executor) -Will-Executor)
        ;; FIXME: Broken because ManyUniv doesn't have a corresponding tc-result
        #;
        (tc-e (let: ((w : Will-Executor (make-will-executor)))
                (will-register w 'a (lambda: ((s : Symbol)) (void)))
                (will-execute w)) #:ret tc-any-results)

        ;Promises
        ;For some reason they are failing in the test suite
        #|
        (tc-e (delay 's) (-Promise -Symbol))
        (tc-e (let: ((p : (Promise Symbol) (delay 's)))
                  (promise-running? p)) B)
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
        [tc-err (exn:fail:contract)]
        [tc-e (#%variable-reference) -Variable-Reference]
        [tc-e (#%variable-reference x) -Variable-Reference]
        [tc-e (#%variable-reference +) -Variable-Reference]
        [tc-e (apply (λ: ([x : String] [y : String]) (string-append x y)) (list "foo" "bar")) -String]
        [tc-e (apply (plambda: (a) ([x : a] [y : a]) x) (list "foo" "bar")) -String]
        [tc-e (ann 
               (case-lambda [(x) (add1 x)]
                            [(x y) (add1 x)])
               (case-> (Integer -> Integer)
                       (Integer Integer -> Integer)))
              #:ret (ret (cl->* (t:-> -Integer -Integer)
                                (t:-> -Integer -Integer -Integer))
                         (-FS -top -bot))]
        [tc-e (let ([my-pred (λ () #f)])
                (for/and: : Any ([i (in-range 4)])
                          (my-pred)))
              #:ret (ret Univ (-FS -top -top) (make-NoObject))]
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
              #:ret (ret -Boolean (-FS -top -top) (make-Empty))]
        [tc-e (let: ([x : Inexact-Real 0.0])
                (= 0 x))
              #:ret (ret -Boolean (-FS -top -top) (make-Empty))]
        [tc-e (let: ([x : Real 0.0])
                (= 0 x))
              #:ret (ret -Boolean (-FS -top -top) (make-Empty))]
        
        [tc-e/t (ann (lambda: ([x : Boolean]) (if x x #t)) (Boolean -> #t)) (t:-> -Boolean (-val #t))]
        
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
              (-seq -Int)]
        [tc-e (sequence-add-between (inst empty-sequence Integer) 'foo)
              (-seq (t:Un -Int (-val 'foo)))]
        [tc-e (let ()
                (define: x : Any (vector 1 2 3))
                (if (vector? x) (vector-ref x 0) #f))
              Univ]
        [tc-e ((inst vector Index) 0)
              (-vec -Index)]
        [tc-err ((inst list Void) 1 2 3)]
        [tc-e ((inst list Any) 1 2 3)
              (-lst Univ)]

        )
  (test-suite
   "check-type tests"
   (test-exn "Fails correctly" exn:fail:syntax? (lambda () (parameterize ([orig-module-stx #'here])
                                                             (check-type #'here N B))))
   (test-not-exn "Doesn't fail on subtypes" (lambda () (check-type #'here N Univ)))
   (test-not-exn "Doesn't fail on equal types" (lambda () (check-type #'here N N))))
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
   [tc-l () (-val null)]
   [tc-l (3 . 4) (-pair -PosByte -PosByte)]
   [tc-l #hash((1 . 2) (3 . 4)) (make-Hashtable -Integer -Integer)]
   [tc-l #hasheq((a . q) (b . w)) (make-Hashtable -Symbol -Symbol)])
  ))


;; these no longer work with the new scheme for top-level identifiers
;; could probably be revived
#;(define (tc-toplevel-tests)
#reader typed-racket/typed-reader
(test-suite "Tests for tc-toplevel"
            (tc-tl 3)
            (tc-tl (define: x : Number 4))
            (tc-tl (define: (f [x : Number]) : Number x))
            [tc-tl (pdefine: (a) (f [x : a]) : Number 3)]
            [tc-tl (pdefine: (a b) (mymap [f : (a -> b)] (l : (list-of a))) : (list-of b)
                             (if (null? l) #{'() : (list-of b)}
                                 (cons (f (car l)) (map f (cdr l)))))]))


(define-go typecheck-tests #;tc-toplevel-tests)
