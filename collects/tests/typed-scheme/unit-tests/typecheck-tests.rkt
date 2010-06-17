#lang racket/base

(require "test-utils.ss"
         (for-syntax scheme/base)
         (for-template scheme/base))
(require (private base-env prims type-annotation 
		  base-types-extra
		  base-env-numeric
		  base-env-indexing
                  parse-type)
	 (typecheck typechecker)
	 (rep type-rep filter-rep object-rep)
         (rename-in (types utils union convenience abbrev filter-ops)
                    [Un t:Un]
                    [true-lfilter -true-lfilter]
                    [true-filter -true-filter]
                    [-> t:->])
         (utils tc-utils utils)
         unstable/mutated-vars
         (env type-name-env type-environments init-envs)
         rackunit rackunit/text-ui
         syntax/parse
         (for-syntax (utils tc-utils)
                     (typecheck typechecker)
	             (env type-env)
	             (private base-env base-env-numeric
			      base-env-indexing))
         (for-template (private base-env base-types base-types-extra
				base-env-numeric
				base-env-indexing))
         (for-syntax syntax/kerncase syntax/parse))

(provide typecheck-tests g tc-expr/expand)

(define N -Number)
(define B -Boolean)
(define Sym -Symbol)
(define -Pos -ExactPositiveInteger)
(define R -Real)
(define F -Flonum)

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
           (parameterize ([mutated-vars (find-mutated-vars ex)])
             (values (lambda () (tc-expr ex)) ex))))]))

(define-syntax (tc-expr/expand stx)
  (syntax-case stx ()
    [(_ e)
     #`(parameterize ([delay-errors? #f]
                      [current-namespace (namespace-anchor->namespace anch)]
                      [orig-module-stx (quote-syntax e)])
         (let ([ex (expand 'e)])
           (parameterize ([mutated-vars (find-mutated-vars ex)])
             (tc-expr ex))))]))

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
   #reader typed-scheme/typed-reader
     (test-suite
        "tc-expr tests"
        
        [tc-e
         (let: ([x : (U Number (cons Number Number)) (cons 3 4)])
               (if (pair? x)
                   (+ 1 (car x))
                   5))
         N]
        (tc-e/t (if (let ([y 12]) y) 3 4) -Pos)
        (tc-e/t 3 -Pos)
        (tc-e/t "foo" -String)
        (tc-e (+ 3 4) -Pos)
        [tc-e/t (lambda: () 3) (t:-> -Pos : -true-lfilter)]
        [tc-e/t (lambda: ([x : Number]) 3) (t:-> N -Pos : -true-lfilter)]
        [tc-e/t (lambda: ([x : Number] [y : Boolean]) 3) (t:-> N B -Pos : -true-lfilter)]
        [tc-e/t (lambda () 3) (t:-> -Pos : -true-lfilter)]
        [tc-e (values 3 4) #:ret (ret (list -Pos -Pos) (list -true-filter -true-filter))]
        [tc-e (cons 3 4) (-pair -Pos -Pos)]
        [tc-e (cons 3 (ann '() : (Listof Integer))) (make-Listof -Integer)]
        [tc-e (void) -Void]
        [tc-e (void 3 4) -Void]
        [tc-e (void #t #f '(1 2 3)) -Void]
        [tc-e/t #(3 4 5) (make-Vector -Pos)]
        [tc-e/t '(2 3 4) (-lst* -Pos -Pos -Pos)]
        [tc-e/t '(2 3 #t) (-lst* -Pos -Pos (-val #t))]
        [tc-e/t #(2 3 #t) (make-Vector (t:Un -Pos (-val #t)))]
        [tc-e/t '(#t #f) (-lst* (-val #t) (-val #f))]
        [tc-e/t (plambda: (a) ([l : (Listof a)]) (car l))
                (make-Poly '(a) (t:-> (make-Listof (-v a)) (-v a)))]
        [tc-e/t (plambda: (a) ([l : (Listof a)]) (car l))
                (make-Poly '(a) (t:-> (make-Listof (-v a)) (-v a)))]
        [tc-e/t (case-lambda: [([a : Number] [b : Number]) (+ a b)]) (t:-> N N N)]
        [tc-e (let: ([x : Number 5]) x) N]
        [tc-e (let-values ([(x) 4]) (+ x 1)) -Pos]
        [tc-e (let-values ([(#{x : Number} #{y : Boolean}) (values 3 #t)]) (and (= x 1) (not y))) 
              #:proc (syntax-parser [(_ ([(_ y) . _]) . _) (ret -Boolean (-FS -top -top))])]
        [tc-e/t (values 3) -Pos]
        [tc-e (values) #:ret (ret null)]
        [tc-e (values 3 #f) #:ret (ret (list -Pos (-val #f)) (list (-FS -top -bot) (-FS -bot -top)))]
        [tc-e (map #{values @ Symbol} '(a b c)) (-pair Sym (make-Listof  Sym))]
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
        [tc-e/t (begin 3) -Pos]
        [tc-e/t (begin #f 3) -Pos]
        [tc-e/t (begin #t) (-val #t)]
        [tc-e/t (begin0 #t) (-val #t)]
        [tc-e/t (begin0 #t 3) (-val #t)]
        [tc-e/t #t (-val #t)]
        [tc-e #f #:ret (ret (-val #f) (-FS -bot -top))]
        [tc-e/t '#t (-val #t)]
        [tc-e '#f #:ret (ret (-val #f) (-FS -bot -top))]
        [tc-e/t (if #f 'a 3) -Pos]
        [tc-e/t (if #f #f #t) (t:Un (-val #t))]
        [tc-e (when #f 3) -Void]
        [tc-e/t '() (-val '())]
        [tc-e/t (let: ([x : (Listof Number) '(1)]) 
                      (cond [(pair? x) 1]
                            [(null? x) 1]))
              -Pos]
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
        
        [tc-e/t (let: ([x : Any 3])
                    (if (list? x)
                        (begin (car x) 1)
                        2))
              -Pos]
        
        
        [tc-e (let: ([x : (U Number Boolean) 3])
                    (if (not (boolean? x))
                        (add1 x)
                        3))
              N]
        
        [tc-e (let ([x 1]) x) -Pos]
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
        [tc-e (map add1 '(1)) (-pair -Pos (-lst -Pos))]
        
        [tc-e/t (let ([x 5])
                (if (eq? x 1)
                    12
                    14))
              -Pos]
        
        [tc-e (car (append (list 1 2) (list 3 4))) -Pos]
        
        [tc-e 
         (let-syntax ([a 
                       (syntax-rules ()
                         [(_ e) (let ([v 1]) e)])])
           (let: ([v : String "a"])
                 (string-append "foo" (a v))))
         -String]
        
        [tc-e (apply (plambda: (a) [x : a *] x) '(5)) (-lst -Pos)]
        [tc-e (apply append (list '(1 2 3) '(4 5 6))) (-lst -Pos)]
        
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
              (t:Un (-val 'squarf) -Pos)]
        
        [tc-e/t (if #t 1 2) -Pos]
        
        
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
                                     (ret -Pos (-FS -top -top))])]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (eq? sym x) 3 x))
               #:proc (syntax-parser [(_ _ (_ ([(x) _]) _))
                                      (ret -Pos (-FS -top -top))])]
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
                                      (ret -Pos (-FS -top -top))])]
        [tc-e (let* ([sym 'squarf]
                     [x (if (= 1 2) 3 sym)])
                (if (equal? sym x) 3 x))
              #:proc (syntax-parser [(_ _ (_ ([(x) _]) _))
                                     (ret -Pos (-FS -top -top))])]
        
        [tc-e (let: ([x : (Listof Symbol)'(a b c)])
                    (cond [(memq 'a x) => car]
                          [else 'foo]))
              Sym]
        
        [tc-e (list 1 2 3) (-lst* -Pos -Pos -Pos)]
        [tc-e (list 1 2 3 'a) (-lst* -Pos -Pos -Pos (-val 'a))]
        
        [tc-e `(1 2 ,(+ 3 4)) (-lst* -Pos -Pos -Pos)]
        
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
        
        [tc-e (cadr (cadr (list 1 (list 1 2 3) 3))) -Pos]
        
        
        
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
              -Pos]
        
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
                (t:-> Univ -Pos : -true-lfilter)]
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
                (t:-> Univ -Pos : -true-lfilter)]
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
        [tc-e/t (with-continuation-mark 'key 'mark 
                3)
              -Pos]
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
        [tc-e/t #'3 (-Syntax -Pos)]
        [tc-e/t #'(1 2 3) (-Syntax (-lst* -Pos -Pos -Pos))]
        
        ;; testing some primitives
        [tc-e (let ([app apply]
                    [f (lambda: [x : Number *] 3)])
                (app f (list 1 2 3)))
              -Pos]
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
              -Pos]
        
        [tc-e/t (let ()
                  (define: (f [x : Number]) : Number
                    (define: (g [y : Number]) : Number
                      (let*-values ([(#{z : Number} #{w : Number}) (values (g (f x)) 5)])
                        (+ z w)))
                    (g 4))
                  5)
                -Pos]
        
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
        
        [tc-e (list* 1 2 3) (-pair -Pos (-pair -Pos -Pos))]
        
        [tc-err (apply append (list 1) (list 2) (list 3) (list (list 1) "foo"))]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list 1))) (-lst -Pos)]
        [tc-e (apply append (list 1) (list 2) (list 3) (list (list 1) (list "foo"))) (-lst (t:Un -String -Pos))]
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
                (-Integer B -Integer . t:-> . -Pos : -true-lfilter)]
        [tc-e/t (inst (plambda: (a ...) [xs : (a ... a -> Integer) ... a] 3) Integer Boolean Integer)
                ((-Integer B -Integer . t:-> . -Integer)
                 (-Integer B -Integer . t:-> . -Integer)
                 (-Integer B -Integer . t:-> . -Integer)
                 . t:-> . -Pos : -true-filter)]
        
        [tc-e/t (plambda: (z x y ...) () (inst map z x y ... y))
              (-polydots (z x y) (t:-> (cl->*
                                        ((t:-> x z) (-pair x (-lst x)) . t:-> . (-pair z (-lst z)))
                                        ((list ((list x) (y y) . ->... . z) (-lst x)) ((-lst y) y) . ->... . (-lst z)))
                                       : (-FS (-not-filter (-val #f) #'map) (-filter (-val #f) #'map))))]
        
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
        [tc-e (apply values (list 1 2 3)) #:ret (ret (list -Pos -Pos -Pos))]
        
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
        
        
        [tc-e (time (+ 3 4)) -ExactPositiveInteger]



        [tc-e
         (call-with-values (lambda () (time-apply + (list 1 2)))
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
              #:ret (ret -Pos (-FS -top -top))]
        [tc-e (let: ([x : (U (Vectorof Number) String) (vector 1 2 3)])
                (if (vector? x) (vector-ref x 0) (string-length x)))
         -Number]
        [tc-e (let ()
                (define: foo : (Integer * -> Integer) +)
                (foo 1 2 3 4 5))
              -Integer]
        [tc-e (let ()
                (define: x : Any 7)
                (if (box? x) (unbox x) (+ 1)))
              Univ]        
        [tc-e (floor 1/2) -Integer]
        [tc-e (ceiling 1/2) -Integer]
        [tc-e (truncate 0.5) -Flonum]
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
        )
  (test-suite
   "check-type tests"
   (test-exn "Fails correctly" exn:fail:syntax? (lambda () (parameterize ([orig-module-stx #'here])
                                                             (check-type #'here N B))))
   (test-not-exn "Doesn't fail on subtypes" (lambda () (check-type #'here N Univ)))
   (test-not-exn "Doesn't fail on equal types" (lambda () (check-type #'here N N))))
  (test-suite
   "tc-literal tests"
   (tc-l 5 -ExactPositiveInteger)
   (tc-l 5# -Flonum)
   (tc-l 5.0 -Flonum)
   (tc-l 5.1 -Flonum)
   (tc-l #t (-val #t))
   (tc-l "foo" -String)
   (tc-l foo (-val 'foo))
   (tc-l #:foo (-val '#:foo))
   (tc-l #f (-val #f))
   (tc-l #"foo" -Bytes)
   [tc-l () (-val null)]
   [tc-l (3 . 4) (-pair -Pos -Pos)]
   [tc-l #hash((1 . 2) (3 . 4)) (make-Hashtable -Pos -Pos)])
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

