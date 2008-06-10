#lang scheme/base

(require "test-utils.ss")

(require (private subtype type-rep type-effect-convenience 
                  planet-requires init-envs type-environments union infer infer-dummy))

(require (schemeunit)
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

(define (subtype-tests)
  (subtyping-tests
   ;; trivial examples
   (Univ Univ)
   (N Univ)
   (B Univ)
   (Sym Univ)
   (-Void Univ)
   #;(Sym Dyn)
   #;(Dyn N)
   [N N]
   [(Un (-pair Univ (-lst Univ)) (-val '())) (-lst Univ)]
   [(-pair N (-pair N (-pair (-val 'foo) (-val '())))) (-lst Univ)]
   [(-pair N (-pair N (-pair (-val 'foo) (-val '())))) (-lst (Un N Sym))]
   [(-pair (-val 6) (-val 6)) (-pair N N)]
   [(-val 6) (-val 6)]
   ;; unions
   [(Un N) N]
   [(Un N N) N]
   [(Un N Sym) (Un Sym N)]
   [(Un (-val 6) (-val 7)) N]
   [(Un (-val #f) (Un (-val 6) (-val 7))) (Un N (Un B Sym))]
   [(Un (-val #f) (Un (-val 6) (-val 7))) (-mu x (Un N (Un B Sym)))]
   [(Un N (-val #f) (-mu x (Un N Sym (make-Listof x)))) 
    (-mu x (Un N Sym B (make-Listof x)))]
   ;; sexps vs list*s of nums
   [(-mu x (Un N Sym (make-Listof x))) (-mu x (Un N Sym B (make-Listof x)))]
   [(-mu x (Un N (make-Listof x))) (-mu x (Un N Sym (make-Listof x)))]
   [(-mu x (Un N (make-Listof x))) (-mu y (Un N Sym (make-Listof y)))]
   ;; a hard one
   [-NE -Sexp]
   ;; simple function types
   ((Univ . -> . N) (N . -> . Univ))
   [(Univ Univ Univ . -> . N) (Univ Univ N . -> . N)]
   ;; simple list types
   [(make-Listof N) (make-Listof Univ)]
   [(make-Listof N) (make-Listof N)]
   [FAIL (make-Listof N) (make-Listof Sym)]
   [(-mu x (make-Listof x)) (-mu x* (make-Listof x*))]
   [(-pair N N) (-pair Univ N)]
   [(-pair N N) (-pair N N)]
   ;; from page 7
   [(-mu t (-> t t)) (-mu s (-> s s))]
   [(-mu s (-> N s)) (-mu t (-> N (-> N t)))]
   ;; polymorphic types
   [(-poly (t) (-> t t)) (-poly (s) (-> s s))]
   [FAIL (make-Listof N) (-poly (t) (make-Listof t))]
   [(-poly (a) (make-Listof (-v a))) (make-Listof N)]     ;; 
   [(-poly (a) N) N]

   [(-val 6) N]
   [(-val 'hello) Sym]
   [((Un Sym N) . -> . N) (-> N N)]
   [(-poly (t) (-> N t)) (-mu t (-> N t))]
   ;; not subtypes
   [FAIL (-val 'hello) N]
   [FAIL (-val #f) Sym]
   [FAIL (Univ Univ N N . -> . N) (Univ Univ Univ . -> . N)]
   [FAIL (N . -> . N) (-> Univ Univ)]
   [FAIL (Un N Sym) N]
   [FAIL N (Un (-val 6) (-val 11))]
   [FAIL Sym (-val 'Sym)]
   [FAIL (Un Sym N) (-poly (a) N)]
   ;; bugs found
   [(Un (-val 'foo) (-val 6)) (Un (-val 'foo) (-val 6))]
   [(-poly (a) (make-Listof (-v a))) (make-Listof (-mu x (Un (make-Listof x) N)))]
   [FAIL (make-Listof (-mu x (Un (make-Listof x) N))) (-poly (a) (make-Listof a))]
   ;; case-lambda
   [(cl-> [(N) N] [(B) B]) (N . -> . N)]
   ;; special case for unused variables
   [N (-poly (a) N)]
   [FAIL (cl-> [(N) B] [(B) N]) (N . -> . N)]
   ;; varargs
   [(->* (list N) Univ B) (->* (list N) N B)]
   [(->* (list Univ) N B) (->* (list N) N B)]
   [(->* (list N) N B) (->* (list N) N B)]
   [(->* (list N) N B) (->* (list N) N Univ)]
   [(->* (list N) N N) (->* (list N N) N)]
   [(->* (list N) N N) (->* (list N N N) N)]
   [(->* (list N N) B N) (->* (list N N) N)]
   [FAIL (->* (list N) N B) (->* (list N N N) N)]
   [(->* (list N N) B N) (->* (list N N B B) N)]

   [(-poly (a) (cl-> [() a]
		     [(N) a]))
    (cl-> [() (-pair N (-v b))]
	  [(N) (-pair N (-v b))])]
   
   [(-poly (a) ((Un (-base 'foo) (-struct 'bar #f (list N a) #f #f #f values)) . -> . (-lst a)))
    ((Un (-base 'foo) (-struct 'bar #f (list N (-pair N (-v a))) #f #f #f values)) . -> . (-lst (-pair N (-v a))))]
   [(-poly (a) ((-struct 'bar #f (list N a) #f #f #f values) . -> . (-lst a)))
    ((-struct 'bar #f (list N (-pair N (-v a))) #f #f #f values) . -> . (-lst (-pair N (-v a))))]
   
   [(-poly (a) (a . -> . (make-Listof a))) ((-v b) . -> . (make-Listof (-v b)))]
   [(-poly (a) (a . -> . (make-Listof a))) ((-pair N (-v b)) . -> . (make-Listof (-pair N (-v b))))]

   (FAIL (-poly (a b) (-> a a)) (-poly (a b) (-> a b)))
   
   ))

(define-go 
  subtype-tests)
