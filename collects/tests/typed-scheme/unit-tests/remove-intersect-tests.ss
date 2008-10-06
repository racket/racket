#lang scheme/base
(require "test-utils.ss" "planet-requires.ss" (for-syntax scheme/base))
(require (rep type-rep)
	 (r:infer infer)
	 (private type-effect-convenience remove-intersect subtype union)
         (schemeunit))

(define-syntax (restr-tests stx)    
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     #'(test-suite "Tests for intersect"
                   (test-check (format "Restrict test: ~a ~a" t1 t2) type-compare? (restrict t1 t2) res) ...)]))

(define (restrict-tests) 
  (restr-tests
   [N (Un N Sym) N]
   [N N N]
   [(Un (-val 'foo) (-val 6)) (Un N Sym) (Un (-val 'foo) (-val 6))]
   [N (-mu a (Un N Sym (make-Listof a))) N]
   [(Un N B) (-mu a (Un N Sym (make-Listof a))) N]
   [(-mu x (Un N (make-Listof x))) (Un Sym N B) N]
   [(Un N -String Sym B) N N]
   
   [(-lst N) (-pair Univ Univ) (-pair N (-lst N))]
   ;; FIXME
   #;
   [-Listof -Sexp (-lst (Un B N -String Sym))]
   #;
   [-Sexp -Listof (-lst -Sexp)]
   ))

(define-syntax (remo-tests stx)    
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     (syntax/loc stx
       (test-suite "Tests for remove"
                   (test-check (format "Remove test: ~a ~a" t1 t2) type-compare? (remove t1 t2) res) ...))]))

(define (remove-tests)
  (remo-tests
   [(Un N Sym) N Sym]
   [N N (Un)]
   [(-mu x (Un N Sym (make-Listof x))) N (Un Sym (make-Listof (-mu x (Un N Sym (make-Listof x)))))]
   [(-mu x (Un N Sym B (make-Listof x))) N (Un Sym B (make-Listof (-mu x (Un N Sym B (make-Listof x)))))]
   [(Un (-val #f) (-mu x (Un N Sym (make-Listof (-v x)))))
    (Un B N) 
    (Un Sym (make-Listof (-mu x (Un N Sym (make-Listof x)))))]
   [(Un (-val 'foo) (-val 6)) (Un N Sym) (Un)]
   [(-> (Un Sym N) N) (-> N N) (Un)]
   [(Un (-poly (a) (make-Listof a)) (-> N N)) (-> N N) (-poly (a) (make-Listof a))]
   [(Un Sym N) (-poly (a) N) Sym]
   [(-pair N (-v a)) (-pair Univ Univ) (Un)]
   ))

(define-go 
  restrict-tests
  remove-tests)

(define x1 
  (-mu list-rec 
       (Un 
        (-val '()) 
        (-pair (-mu x (Un B N -String Sym (-val '()) (-pair x x)))
               list-rec))))
(define x2 
  (Un (-val '()) 
      (-pair (-mu x (Un B N -String Sym (-val '()) (-pair x x)))
             (-mu x (Un B N -String Sym (-val '()) (-pair x x))))))
(provide remove-tests restrict-tests)

