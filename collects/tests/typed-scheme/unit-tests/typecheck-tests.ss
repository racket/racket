#lang scheme/base

(require "test-utils.ss" "planet-requires.ss"
         (for-syntax scheme/base)
         (for-template scheme/base))
(require (private base-env mutated-vars type-utils union prims type-effect-convenience type-annotation)
	 (typecheck typechecker)
	 (rep type-rep effect-rep)
         (utils tc-utils)
         (env type-name-env type-environments init-envs)
         (schemeunit))

(require (for-syntax (utils tc-utils)
                     (typecheck typechecker)
	             (env type-env)
	             (private base-env))
         (for-template (private base-env base-types)))



(provide typecheck-tests g tc-expr/expand)

(define (g) (run typecheck-tests))

(define-namespace-anchor anch)


;; check that a literal typechecks correctly
(define-syntax tc-l
  (syntax-rules ()
    [(_ lit ty)
     (check-type-equal? (format "~a" 'lit) (tc-literal #'lit) ty)]))

;; local-expand and then typecheck an expression
(define-syntax (tc-expr/expand stx)
  (syntax-case stx ()
    [(_ e)
     #`(parameterize ([delay-errors? #f]
                      [current-namespace (namespace-anchor->namespace anch)]
                      [orig-module-stx (quote-syntax e)])
         (let ([ex (expand 'e)])
           (find-mutated-vars ex)
           (tc-expr ex)))]))

;; check that an expression typechecks correctly
(define-syntax (tc-e stx)
  (syntax-case stx ()
    [(_ expr ty) (syntax/loc stx (tc-e expr ty (list) (list)))]
    [(_ expr ty eff1 eff2)
     (syntax/loc stx (check-tc-result-equal? (format "~a" 'expr)
                                             (tc-expr/expand expr)
                                             (ret ty eff1 eff2)))]))

(require (for-syntax syntax/kerncase))

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


(define (typecheck-tests)
  (test-suite 
   "Typechecker tests"
   #reader typed-scheme/typed-reader
   (let ([-vet (lambda (x) (list (-vet x)))]
	 [-vef (lambda (x) (list (-vef x)))])
     (test-suite
        "tc-expr tests"
        
        [tc-e
         (let: ([x : (U Number (cons Number Number)) (cons 3 4)])
               (if (pair? x)
                   (+ 1 (car x))
                   5))
         N]
        
        (tc-e 3 -Integer)
        (tc-e "foo" -String)
        (tc-e (+ 3 4) -Integer)
        [tc-e (lambda: () 3) (-> -Integer)]
        [tc-e (lambda: ([x : Number]) 3) (-> N -Integer)]
        [tc-e (lambda: ([x : Number] [y : Boolean]) 3) (-> N B -Integer)]
        [tc-e (lambda () 3) (-> -Integer)]
        [tc-e (values 3 4) (-values (list -Integer -Integer))]
        [tc-e (cons 3 4) (-pair -Integer -Integer)]
        [tc-e (cons 3 #{'() : (Listof -Integer)}) (make-Listof -Integer)]
        [tc-e (void) -Void]
        [tc-e (void 3 4) -Void]
        [tc-e (void #t #f '(1 2 3)) -Void]
        [tc-e #(3 4 5) (make-Vector -Integer)]
        [tc-e '(2 3 4) (-lst* -Integer -Integer -Integer)]
        [tc-e '(2 3 #t) (-lst* -Integer -Integer (-val #t))]
        [tc-e #(2 3 #t) (make-Vector (Un -Integer (-val #t)))]
        [tc-e '(#t #f) (-lst* (-val #t) (-val #f))]
        [tc-e (plambda: (a) ([l : (Listof a)]) (car l))
              (make-Poly '(a) (-> (make-Listof  (-v a)) (-v a)))]
        [tc-e (plambda: (a) ([l : (Listof a)]) (car l))
              (make-Poly '(a) (-> (make-Listof  (-v a)) (-v a)))]
        [tc-e (case-lambda: [([a : Number] [b : Number]) (+ a b)]) (-> N N N)]
        [tc-e (let: ([x : Number 5]) x) N (-vet #'x) (-vef #'x)]
        [tc-e (let-values ([(x) 4]) (+ x 1)) -Integer]
        [tc-e (let-values ([(#{x : Number} #{y : Boolean}) (values 3 #t)]) (and (= x 1) (not y))) 
              B (list (-rest (-val #f) #'y)) (list)]
        [tc-e (values 3) -Integer]
        [tc-e (values) (-values (list))]
        [tc-e (values 3 #f) (-values (list -Integer (-val #f)))]
        [tc-e (map #{values @ Symbol} '(a b c)) (make-Listof  Sym)]
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
              N]
        [tc-e (let: ([f : (Number Number -> Number) +]) (f 3 4)) N]
        [tc-e (let: ([+ : (Boolean -> Number) (lambda: ([x : Boolean]) 3)]) (+ #f)) N]
        [tc-e (when #f #t) (Un -Void)]
        [tc-e (when (number? #f) (+ 4 5)) (Un -Integer -Void)]
        [tc-e (let: ([x : (Un #f Number) 7])
                    (if x (+ x 1) 3))
              N]
	[tc-e (let: ([x : Number 1])
		    (if (and (number? x) #t)
			(+ x 4)
			'bc))
	      N]
        [tc-e (let: ((x : Number 3)) (if (boolean? x) (not x) #t)) (-val #t)]
        [tc-e (begin 3) -Integer]
        [tc-e (begin #f 3) -Integer]
        [tc-e (begin #t) (-val #t) (list (make-True-Effect)) (list (make-True-Effect))]
        [tc-e (begin0 #t) (-val #t) (list (make-True-Effect)) (list (make-True-Effect))]
        [tc-e (begin0 #t 3) (-val #t) (list (make-True-Effect)) (list (make-True-Effect))]
        [tc-e #t (-val #t) (list (make-True-Effect)) (list (make-True-Effect))]
        [tc-e #f (-val #f) (list (make-False-Effect)) (list (make-False-Effect))]
        [tc-e '#t (-val #t) (list (make-True-Effect)) (list (make-True-Effect))]
        [tc-e '#f (-val #f) (list (make-False-Effect)) (list (make-False-Effect))]
        [tc-e (if #f 'a 3) -Integer]
        [tc-e (if #f #f #t) (Un (-val #t))]
        [tc-e (when #f 3) -Void]
        [tc-e '() (-val '())]
        [tc-e (let: ([x : (Listof Number) '(1)]) 
                    (cond [(pair? x) 1]
                          [(null? x) 1]))
              -Integer]
        [tc-e (lambda: ([x : Number] . [y : Number *]) (car y)) (->* (list N) N N)]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3) N]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3 4 5) N]
        [tc-e ((lambda: ([x : Number] . [y : Number *]) (car y)) 3 4) N]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '(4)) N]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '(4 6 7)) N]
        [tc-e (apply (lambda: ([x : Number] . [y : Number *]) (car y)) 3 '()) N]
        
        [tc-e (lambda: ([x : Number] . [y : Boolean *]) (car y)) (->* (list N) B B)]
        [tc-e ((lambda: ([x : Number] . [y : Boolean *]) (car y)) 3) B]
        [tc-e (apply (lambda: ([x : Number] . [y : Boolean *]) (car y)) 3 '(#f)) B]
        
        [tc-e (let: ([x : Number 3])
                    (when (number? x) #t))
              (-val #t) (list (make-True-Effect)) (list (make-True-Effect))]
        [tc-e (let: ([x : Number 3])
                    (when (boolean? x) #t))
              -Void]
        
        [tc-e (let: ([x : Any 3])
                    (if (list? x)
                        (begin (car x) 1) 2))
              -Integer]
        
        
        [tc-e (let: ([x : (U Number Boolean) 3])
                    (if (not (boolean? x))
                        (add1 x)
                        3))
              N]
        
        [tc-e (let ([x 1]) x) -Integer (-vet #'x) (-vef #'x)]
        [tc-e (let ([x 1]) (boolean? x)) B (list (-rest B #'x)) (list (-rem B #'x))]
        [tc-e (boolean? number?) B (list (-rest B #'number?)) (list (-rem B #'number?))]
        
        [tc-e (let: ([x : (Option Number) #f]) x) (Un N (-val #f)) (-vet #'x) (-vef #'x)]
        [tc-e (let: ([x : Any 12]) (not (not x))) 
              B (list (-rem (-val #f) #'x)) (list (-rest (-val #f) #'x))]
        
        [tc-e (let: ([x : (Option Number) #f])
                    (if (let ([z 1]) x)
                        (add1 x)
                        12)) 
              N]
        [tc-err (5 4)]
        [tc-err (apply 5 '(2))]
        [tc-err (map (lambda: ([x : Any] [y : Any]) 1) '(1))]
        [tc-e (map add1 '(1)) (-lst -Integer)]
        
        [tc-e (let ([x 5])
                (if (eq? x 1)
                    12
                    14))
              -Integer]
        
        [tc-e (car (append (list 1 2) (list 3 4))) -Integer]
        
        [tc-e 
         (let-syntax ([a 
                       (syntax-rules ()
                         [(_ e) (let ([v 1]) e)])])
           (let: ([v : String "a"])
                 (string-append "foo" (a v))))
         -String]
        
        [tc-e (apply (plambda: (a) [x : a *] x) '(5)) (-lst -Integer)]
        [tc-e (apply append (list '(1 2 3) '(4 5 6))) (-lst -Integer)]
        
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
        
        
        [tc-e null (-val null) (-vet #'null) (-vef #'null)]
        
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                x)
              (Un (-val 'squarf) -Integer)
              (-vet #'x) (-vef #'x)]
        
        [tc-e (if #t 1 2) -Integer]
        
        
        ;; eq? as predicate
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (eq? x 'foo) 3 x)) N]
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (eq? 'foo x) 3 x)) N]
        
        [tc-err (let: ([x : (U String 'foo) 'foo])
                      (if (string=? x 'foo)
                          "foo"
                          x))]
        #;[tc-e (let: ([x : (U String 5) 5])
                      (if (eq? x 5)
                          "foo"
                          x))
                (Un -String (-val 5))]
        
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? x sym) 3 x))
              -Integer]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? sym x) 3 x))
              -Integer]
        ;; equal? as predicate for symbols
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (equal? x 'foo) 3 x)) N]
        [tc-e (let: ([x : (Un 'foo Number) 'foo])
                    (if (equal? 'foo x) 3 x)) N]
        
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? x sym) 3 x))
              -Integer]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? sym x) 3 x))
              -Integer]
        
        [tc-e (let: ([x : (Listof Symbol)'(a b c)])
                    (cond [(memq 'a x) => car]
                          [else 'foo]))
              Sym]
        
        [tc-e (list 1 2 3) (-lst* -Integer -Integer -Integer)]
        [tc-e (list 1 2 3 'a) (-lst* -Integer -Integer -Integer (-val 'a))]
        #;
        [tc-e `(1 2 ,(+ 3 4)) (-lst* N N N)]
        
        [tc-e (let: ([x : Any 1])
                    (when (and (list? x) (not (null? x)))
                      (car x)))
              Univ]
        
        [tc-err (let: ([x : Any 3])
                      (car x))]
        [tc-err (car #{3 : Any})]
        [tc-err (map #{3 : Any} #{12 : Any})]
        [tc-err (car 3)]
        
        [tc-e (let: ([x : Any 1])
                    (if (and (list? x) (not (null? x)))
                        x
                        'foo))
              (Un (-val 'foo) (-pair Univ (-lst Univ)))]
        
        [tc-e (cadr (cadr (list 1 (list 1 2 3) 3))) -Integer]
        
        
        
        ;;; tests for and
        [tc-e (let: ([x : Any 1]) (and (number? x) (boolean? x))) B 
              (list (-rest N #'x) (-rest B #'x)) (list)]
        [tc-e (let: ([x : Any 1]) (and (number? x) x)) (Un N (-val #f)) 
              (list (-rest N #'x) (make-Var-True-Effect #'x)) (list)]
        [tc-e (let: ([x : Any 1]) (and x (boolean? x))) B
              (list (-rem (-val #f) #'x) (-rest B #'x)) (list)]
        
        [tc-e (let: ([x : Any 3])
                    (if (and (list? x) (not (null? x)))
                        (begin (car x) 1) 2))
              -Integer]
        
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
              Univ]
        [tc-e (let: ([x : Any 1])
                    (if (if ((lambda: ([x : Any]) x) 12)
                            #t
                            (boolean? x))
                        (if (boolean? x) 1 x)
                        4))
              Univ]
        
        ;; T-AbsPred
        [tc-e (let ([p? (lambda: ([x : Any]) (number? x))])
                (lambda: ([x : Any]) (if (p? x) (add1 x) 12)))
              (-> Univ N)]
        [tc-e (let ([p? (lambda: ([x : Any]) (not (number? x)))])
                (lambda: ([x : Any]) (if (p? x) 12 (add1 x))))
              (-> Univ N)]
        [tc-e (let* ([z 1]
                     [p? (lambda: ([x : Any]) (number? z))])
                (lambda: ([x : Any]) (if (p? x) 11 12)))
              (-> Univ -Integer)]
        [tc-e (let* ([z 1]
                     [p? (lambda: ([x : Any]) (number? z))])
                (lambda: ([x : Any]) (if (p? x) x 12)))
              (-> Univ Univ)]
        [tc-e (let* ([z 1]
                     [p? (lambda: ([x : Any]) (not (number? z)))])
                (lambda: ([x : Any]) (if (p? x) x 12)))
              (-> Univ Univ)]
        [tc-e (let* ([z 1]
                     [p? (lambda: ([x : Any]) z)])
                (lambda: ([x : Any]) (if (p? x) x 12)))
              (-> Univ Univ)]
        
        [tc-e (not 1) B]
        
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
        [tc-e (with-continuation-mark 'key 'mark 
                3)
              -Integer]
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
        [tc-e #'3 (-Syntax -Integer)]
        [tc-e #'(1 2 3) (-Syntax (-lst* -Integer -Integer -Integer))]
        
        ;; testing some primitives
        [tc-e (let ([app apply]
                    [f (lambda: [x : Number *] 3)])
                (app f (list 1 2 3)))
              -Integer]
        [tc-e ((lambda () (call/cc (lambda: ([k : (Number -> (U))]) (if (read) 5 (k 10))))))
              N]
        
        [tc-e (number->string 5) -String]
        
        [tc-e (let-values ([(a b) (quotient/remainder 5 12)]
                           [(a*) (quotient 5 12)]
                           [(b*) (remainder 5 12)])
                (+ a b a* b*))
              -Integer]
        
        [tc-e (raise-type-error 'foo "bar" 5) (Un)]
        [tc-e (raise-type-error 'foo "bar" 7 (list 5)) (Un)]
        
        #;[tc-e
           (let ((x '(1 3 5 7 9)))
             (do: : Number ((x : (list-of Number) x (cdr x))
                            (sum : Number 0 (+ sum (car x))))
                  ((null? x) sum)))
           N]
        
        
        ;; inference with internal define
        [tc-e (let ()
                (define x 1)
                (define y 2)
                (define z (+ x y))
                (* x z))
              -Integer]
        
        [tc-e (let ()
                (define: (f [x : Number]) : Number
                  (define: (g [y : Number]) : Number
                    (let*-values ([(#{z : Number} #{w : Number}) (values (g (f x)) 5)])
                      (+ z w)))
                  (g 4))
                5)
              -Integer]
        
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
         (let ((x '(1 3 5 7 9)))
           (do: : Number ((x : (Listof Number) x (cdr x))
                          (sum : Number 0 (+ sum (car x))))
                ((null? x) sum)))
         N]
        
        [tc-e (if #f 1 'foo) (-val 'foo)]
        
        [tc-e (list* 1 2 3) (-pair -Integer (-pair -Integer -Integer))]
        
        [tc-err (apply append (list 1) (list 2) (list 3) (list (list 1) "foo"))]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list 1))) (-lst -Integer)]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list "foo"))) (-lst (Un -String -Integer))]
        [tc-err (plambda: (b ...) [y : b ... b] (apply append (map list y)))]
        [tc-e (plambda: (b ...) [y : (Listof Integer) ... b] (apply append y))
              (-polydots (b) (->... (list) ((-lst -Integer) b) (-lst -Integer)))]
        
        [tc-err (plambda: (a ...) ([z : String] . [w : Number ... a])
                          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                                 1 1 1 1 w))]
        
        [tc-err (plambda: (a ...) ([z : String] . [w : Number])
                          (apply (plambda: (b) ([x : Number] . [y : Number ... a]) x)
                                 1 w))]
        
        [tc-e (plambda: (a ...) ([z : String] . [w : Number ... a])
                        (apply (plambda: (b ...) ([x : Number] . [y : Number ... b]) x)
                               1 w))
              (-polydots (a) ((list -String) (N a) . ->... . N))]
        ;; instantiating non-dotted terms
        [tc-e (inst (plambda: (a) ([x : a]) x) Integer)
              (-Integer . -> . -Integer : (list (make-Latent-Var-True-Effect)) (list (make-Latent-Var-False-Effect)))]
        [tc-e (inst (plambda: (a) [x : a *] (apply list x)) Integer)
              ((list) -Integer . ->* . (-lst -Integer))]
        
        ;; instantiating dotted terms
        [tc-e (inst (plambda: (a ...) [xs : a ... a] 3) Integer Boolean Integer)
              (-Integer B -Integer . -> . -Integer)]
        [tc-e (inst (plambda: (a ...) [xs : (a ... a -> Integer) ... a] 3) Integer Boolean Integer)
              ((-Integer B -Integer . -> . -Integer)
               (-Integer B -Integer . -> . -Integer)
               (-Integer B -Integer . -> . -Integer)
               . -> . -Integer)]
        
        [tc-e (plambda: (z x y ...) () (inst map z x y ... y))
              (-polydots (z x y) (-> ((list ((list x) (y y) . ->... . z) (-lst x)) ((-lst y) y) . ->... . (-lst z))))]
        
        ;; error tests
        [tc-err (#%variable-reference number?)]
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
        
        [tc-err (lambda: ([x : Any])
                         (if (number? (not (not x)))
                             (add1 x)
                             12))]
        
        [tc-e (filter integer? (list 1 2 3 'foo))
              (-lst -Integer)]
        
        [tc-e (filter even? (filter integer? (list 1 2 3 'foo)))
              (-lst -Integer)]
        
        [tc-err (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : Char] . [xs : a ... a]) c)
                                 3 (list #\c) as))]
        [tc-err (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : String] . [xs : a ... a]) c)
                                 3 (list #\c) (map list as)))]
        [tc-err (plambda: (a ...) [as : a ... a]
                          (apply fold-left (lambda: ([c : Integer] [a : Char] . [xs : a ... a]) c)
                                 3 (list #\c) (map list (map list as))))]
        
        [tc-e (plambda: (a ...) [as : a ... a]
                        (apply fold-left (lambda: ([c : Integer] [a : Char] . [xs : a ... a]) c)
                               3 (list #\c) (map list as)))
              (-polydots (a) ((list) (a a) . ->... . -Integer))]
        
        ;; First is same as second, but with map explicitly instantiated.
        [tc-e (plambda: (a ...) [ys : (a ... a -> Number) *]
                (lambda: [zs : a ... a]
                  ((inst map Number (a ... a -> Number))
                   (lambda: ([y : (a ... a -> Number)])
                     (apply y zs))
                   ys)))
              (-polydots (a) ((list) ((list) (a a) . ->... . N) . ->* . ((list) (a a) . ->... . (-lst N))))]
        [tc-e (plambda: (a ...) [ys : (a ... a -> Number) *]
                (lambda: [zs : a ... a]
                  (map (lambda: ([y : (a ... a -> Number)])
                         (apply y zs))
                       ys)))
              (-polydots (a) ((list) ((list) (a a) . ->... . N) . ->* . ((list) (a a) . ->... . (-lst N))))]
        
        [tc-e (lambda: ((x : (All (t) t)))
                       ((inst (inst x (All (t) (t -> t))) 
                              (All (t) t))
                        x))
              ((-poly (a) a)  . -> . (-poly (a) a))]
        
        ;; We need to make sure that even if a isn't free in the dotted type, that it gets replicated
        ;; appropriately.
        [tc-e (inst (plambda: (a ...) [ys : Number ... a]
                              (apply + ys))
                    Boolean String Number)
              (N N N . -> . N)]
        
        [tc-e (assq 'foo #{'((a b) (foo bar)) :: (Listof (List Symbol Symbol))})
              (Un (-val #f) (-pair Sym (-pair Sym (-val null))))]
        
        #;[tc-err (let: ([fact : (Number -> Number) (lambda: ([n : Number]) (if (zero? n) 1 (* n (fact (- n 1)))))])
                        (fact 20))]
        
        #;[tc-err ]
        ))
  (test-suite
   "check-type tests"
   (test-exn "Fails correctly" exn:fail:syntax? (lambda () (parameterize ([orig-module-stx #'here])
                                                             (check-type #'here N B))))
   (test-not-exn "Doesn't fail on subtypes" (lambda () (check-type #'here N Univ)))
   (test-not-exn "Doesn't fail on equal types" (lambda () (check-type #'here N N)))
   )
  (test-suite
   "tc-literal tests"
   (tc-l 5 -Integer)
   (tc-l 5# -Integer)
   (tc-l 5.1 N)
   (tc-l #t (-val #t))
   (tc-l "foo" -String)
   (tc-l foo (-val 'foo))
   (tc-l #:foo (-val '#:foo))
   (tc-l #f (-val #f))
   (tc-l #"foo" -Bytes)
   [tc-l () (-val null)]
   )
  ))


;; these no longer work with the new scheme for top-level identifiers
;; could probably be revived
#;(define (tc-toplevel-tests)
#reader typed-scheme/typed-reader
(test-suite "Tests for tc-toplevel"
	    (tc-tl 3)
	    (tc-tl (define: x : Number 4))
	    (tc-tl (define: (f [x : Number]) : Number x))
	    [tc-tl (pdefine: (a) (f [x : a]) : Number 3)]
	    [tc-tl (pdefine: (a b) (mymap [f : (a -> b)] (l : (list-of a))) : (list-of b)
			     (if (null? l) #{'() : (list-of b)}
				 (cons (f (car l)) (map f (cdr l)))))]))


(define-go typecheck-tests #;tc-toplevel-tests)

