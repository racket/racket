#lang scheme/base

(require "test-utils.ss" "planet-requires.ss" (for-syntax scheme/base))
(require (rep type-rep)
	 (private type-comparison type-effect-convenience union subtype)
         (schemeunit))

(provide type-equal-tests)


(define-syntax (te-tests stx)
  (define (single-test stx)
    (syntax-case stx (FAIL)
      [(FAIL t s) #'((test-check (format "FAIL ~a" '(t s)) (lambda (a b) (not (type-equal? a b))) t s)
                     (test-check (format "FAIL ~a" '(s t)) (lambda (a b) (not (type-equal? a b))) s t))]
      [(t s) (syntax/loc stx
               ((test-check (format "~a" '(t s)) type-equal? t s)
                (test-check (format "~a" '(s t)) type-equal? s t)))]))
  (syntax-case stx ()
    [(_ cl ...)
     (with-syntax ([((cl1 cl2) ...) (map single-test (syntax->list #'(cl ...)))])
       #'(test-suite "Tests for type equality"
                     cl1 ... cl2 ...))]))

(define (type-equal-tests)
  (te-tests
   [N N]
   [(Un N) N]
   [(Un N Sym B) (Un N B Sym)]
   [(Un N Sym B) (Un Sym B N)]
   [(Un N Sym B) (Un Sym N B)]
   [(Un N Sym B) (Un B (Un Sym N))]
   [(Un N Sym) (Un Sym N)]
   [(-poly (x) (-> (Un Sym N) x)) (-poly (xyz) (-> (Un N Sym) xyz))]
   [(-mu x (Un N Sym x)) (-mu y (Un N Sym y))]     
   ;; found bug
   [FAIL (Un (-mu heap-node 
                  (-struct 'heap-node #f (list (-base 'comparator) N (-v a) (Un heap-node (-base 'heap-empty))) #f  #f #f values)) 
             (-base 'heap-empty))
         (Un (-mu heap-node 
                  (-struct 'heap-node #f (list (-base 'comparator) N (-pair N N) (Un heap-node (-base 'heap-empty))) #f #f #f values)) 
             (-base 'heap-empty))]))



(define-go
  type-equal-tests)


