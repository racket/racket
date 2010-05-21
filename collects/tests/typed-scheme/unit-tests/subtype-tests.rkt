#lang scheme/base

(require "test-utils.ss"
         (types subtype convenience union)
	 (rep type-rep)
	 (env init-envs type-env-structs)
	 (r:infer infer infer-dummy)
         rackunit
         (for-syntax scheme/base))

(provide subtype-tests)

(define-syntax (subtyping-tests stx)
  (define (single-test stx)
    (syntax-case stx (FAIL)
      [(FAIL t s) (syntax/loc stx (test-check (format "FAIL ~a" '(t s)) (lambda (a b) (not (subtype a b))) t s))]
      [(t s) (syntax/loc stx (test-check (format "~a" '(t s)) subtype t s))]))
  (syntax-case stx ()
    [(_ cl ...)
     (with-syntax ([(new-cl ...) (map single-test (syntax->list #'(cl ...)))])
		  (syntax/loc stx
			      (begin (test-suite "Tests for subtyping"
						 new-cl ...))))]))

(infer-param infer)

(define (subtype-tests)
  (subtyping-tests
   ;; trivial examples
   (Univ Univ)
   (-Number Univ)
   (-Boolean Univ)
   (-Symbol Univ)
   (-Void Univ)
   [-Number -Number]
   [(Un (-pair Univ (-lst Univ)) (-val '())) (-lst Univ)]
   [(-pair -Number (-pair -Number (-pair (-val 'foo) (-val '())))) (-lst Univ)]
   [(-pair -Number (-pair -Number (-pair (-val 'foo) (-val '())))) (-lst (Un -Number -Symbol))]
   [(-pair (-val 6) (-val 6)) (-pair -Number -Number)]
   [(-val 6) (-val 6)]
   ;; unions
   [(Un -Number) -Number]
   [(Un -Number -Number) -Number]
   [(Un -Number -Symbol) (Un -Symbol -Number)]
   [(Un (-val 6) (-val 7)) -Number]
   [(Un (-val #f) (Un (-val 6) (-val 7))) (Un -Number (Un -Boolean -Symbol))]
   [(Un (-val #f) (Un (-val 6) (-val 7))) (-mu x (Un -Number (Un -Boolean -Symbol)))]
   [(Un -Number (-val #f) (-mu x (Un -Number -Symbol (make-Listof x)))) 
    (-mu x (Un -Number -Symbol -Boolean (make-Listof x)))]
   ;; sexps vs list*s of nums
   [(-mu x (Un -Number -Symbol (make-Listof x))) (-mu x (Un -Number -Symbol -Boolean (make-Listof x)))]
   [(-mu x (Un -Number (make-Listof x))) (-mu x (Un -Number -Symbol (make-Listof x)))]
   [(-mu x (Un -Number (make-Listof x))) (-mu y (Un -Number -Symbol (make-Listof y)))]
   ;; a hard one
   [(-mu x (*Un -Number (-pair x (-pair -Symbol (-pair x (-val null)))))) -Sexp]
   ;; simple function types
   ((Univ . -> . -Number) (-Number . -> . Univ))
   [(Univ Univ Univ . -> . -Number) (Univ Univ -Number . -> . -Number)]
   ;; simple list types
   [(make-Listof -Number) (make-Listof Univ)]
   [(make-Listof -Number) (make-Listof -Number)]
   [FAIL (make-Listof -Number) (make-Listof -Symbol)]
   [(-mu x (make-Listof x)) (-mu x* (make-Listof x*))]
   [(-pair -Number -Number) (-pair Univ -Number)]
   [(-pair -Number -Number) (-pair -Number -Number)]
   ;; from page 7
   [(-mu t (-> t t)) (-mu s (-> s s))]
   [(-mu s (-> -Number s)) (-mu t (-> -Number (-> -Number t)))]
   ;; polymorphic types
   [(-poly (t) (-> t t)) (-poly (s) (-> s s))]
   [FAIL (make-Listof -Number) (-poly (t) (make-Listof t))]
   [(-poly (a) (make-Listof (-v a))) (make-Listof -Number)]     ;; 
   [(-poly (a) -Number) -Number]

   [(-val 6) -Number]
   [(-val 'hello) -Symbol]
   [((Un -Symbol -Number) . -> . -Number) (-> -Number -Number)]
   [(-poly (t) (-> -Number t)) (-mu t (-> -Number t))]
   ;; not subtypes
   [FAIL (-val 'hello) -Number]
   [FAIL (-val #f) -Symbol]
   [FAIL (Univ Univ -Number -Number . -> . -Number) (Univ Univ Univ . -> . -Number)]
   [FAIL (-Number . -> . -Number) (-> Univ Univ)]
   [FAIL (Un -Number -Symbol) -Number]
   [FAIL -Number (Un (-val 6) (-val 11))]
   [FAIL -Symbol (-val 'Sym)]
   [FAIL (Un -Symbol -Number) (-poly (a) -Number)]
   ;; bugs found
   [(Un (-val 'foo) (-val 6)) (Un (-val 'foo) (-val 6))]
   [(-poly (a) (make-Listof (-v a))) (make-Listof (-mu x (Un (make-Listof x) -Number)))]
   [FAIL (make-Listof (-mu x (Un (make-Listof x) -Number))) (-poly (a) (make-Listof a))]
   ;; case-lambda
   [(cl-> [(-Number) -Number] [(-Boolean) -Boolean]) (-Number . -> . -Number)]
   ;; special case for unused variables
   [-Number (-poly (a) -Number)]
   [FAIL (cl-> [(-Number) -Boolean] [(-Boolean) -Number]) (-Number . -> . -Number)]
   ;; varargs
   [(->* (list -Number) Univ -Boolean) (->* (list -Number) -Number -Boolean)]
   [(->* (list Univ) -Number -Boolean) (->* (list -Number) -Number -Boolean)]
   [(->* (list -Number) -Number -Boolean) (->* (list -Number) -Number -Boolean)]
   [(->* (list -Number) -Number -Boolean) (->* (list -Number) -Number Univ)]
   [(->* (list -Number) -Number -Number) (->* (list -Number -Number) -Number)]
   [(->* (list -Number) -Number -Number) (->* (list -Number -Number -Number) -Number)]
   [(->* (list -Number -Number) -Boolean -Number) (->* (list -Number -Number) -Number)]
   [FAIL (->* (list -Number) -Number -Boolean) (->* (list -Number -Number -Number) -Number)]
   [(->* (list -Number -Number) -Boolean -Number) (->* (list -Number -Number -Boolean -Boolean) -Number)]

   [(-poly (a) (cl-> [() a]
		     [(-Number) a]))
    (cl-> [() (-pair -Number (-v b))]
	  [(-Number) (-pair -Number (-v b))])]
   
   [(-values (list -Number)) (-values (list Univ))]
   
   [(-poly (a) ((Un (make-Base 'foo #'dummy) (-struct 'bar #f (list -Number a) null #'values)) . -> . (-lst a)))
    ((Un (make-Base 'foo #'dummy) (-struct 'bar #f (list -Number (-pair -Number (-v a))) null #'values)) 
     . -> . (-lst (-pair -Number (-v a))))]
   [(-poly (a) ((-struct 'bar #f (list -Number a) null #'values) . -> . (-lst a)))
    ((-struct 'bar #f (list -Number (-pair -Number (-v a))) null #'values) . -> . (-lst (-pair -Number (-v a))))]
   
   [(-poly (a) (a . -> . (make-Listof a))) ((-v b) . -> . (make-Listof (-v b)))]
   [(-poly (a) (a . -> . (make-Listof a))) ((-pair -Number (-v b)) . -> . (make-Listof (-pair -Number (-v b))))]

   (FAIL (-poly (a b) (-> a a)) (-poly (a b) (-> a b)))
   
   ;; polymorphic function types should be subtypes of the function top
   [(-poly (a) (a . -> . a)) top-func]
   (FAIL (-> Univ) (null Univ . ->* . Univ))

   [(cl->* (-Number . -> . -String) (-Boolean . -> . -String)) ((Un -Boolean -Number) . -> . -String)]
   ))

(define-go 
  subtype-tests)
