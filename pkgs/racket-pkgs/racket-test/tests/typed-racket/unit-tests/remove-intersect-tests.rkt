#lang scheme/base
(require "test-utils.rkt" (for-syntax scheme/base)
         (rep type-rep)
         (r:infer infer)
         (types abbrev numeric-tower subtype union remove-intersect)
         rackunit)

(define-syntax (over-tests stx)
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     #'(test-suite "Tests for intersect"
                   (test-check (format "Overlap test: ~a ~a" t1 t2) (lambda (a b) (eq? (not (not a)) b)) (overlap t1 t2) res) ...)]))

(define (overlap-tests)
  (over-tests
   [-Number -Integer #t]))

(define-syntax (restr-tests stx)
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     #'(test-suite "Tests for intersect"
                   (test-check (format "Restrict test: ~a ~a" t1 t2) type-compare? (restrict t1 t2) res) ...)]))


(define (restrict-tests)
  (restr-tests
   [-Number (Un -Number -Symbol) -Number]
   [-Number -Number -Number]
   [(Un (-val 'foo) (-val 6)) (Un -Number -Symbol) (Un (-val 'foo) (-val 6))]
   [-Number (-mu a (Un -Number -Symbol (make-Listof a))) -Number]
   [(Un -Number -Boolean) (-mu a (Un -Number -Symbol (make-Listof a))) -Number]
   [(-mu x (Un -Number (make-Listof x))) (Un -Symbol -Number -Boolean) -Number]
   [(Un -Number -String -Symbol -Boolean) -Number -Number]

   [(-lst -Number) (-pair Univ Univ) (-pair -Number (-lst -Number))]
   [(-lst -Number) (-poly (a) (-lst a)) (-lst -Number)]
   ;; FIXME
   #;
   [-Listof -Sexp (-lst (Un B N -String Sym))]
   #;
   [-Sexp -Listof (-lst -Sexp)]
   [(-val "one") -Fixnum (Un)]
   [(Un (-val "one") (-val "two")) (Un (-val "one") (-val 1)) (-val "one")]
   ))

(define-syntax (remo-tests stx)
  (syntax-case stx ()
    [(_ [t1 t2 res] ...)
     (syntax/loc stx
       (test-suite "Tests for remove"
                   (test-check (format "Remove test: ~a ~a" t1 t2) type-compare? (remove t1 t2) res) ...))]))

(define (remove-tests)
  (remo-tests
   [(Un -Number -Symbol) -Number -Symbol]
   [-Number -Number (Un)]
   [(-mu x (Un -Number -Symbol (make-Listof x))) -Number (Un -Symbol (make-Listof (-mu x (Un -Number -Symbol (make-Listof x)))))]
   [(-mu x (Un -Number -Symbol -Boolean (make-Listof x))) -Number (Un -Symbol -Boolean (make-Listof (-mu x (Un -Number -Symbol -Boolean (make-Listof x)))))]
   [(Un (-val #f) (-mu x (Un -Number -Symbol (make-Listof (-v x)))))
    (Un -Boolean -Number)
    (Un -Symbol (make-Listof (-mu x (Un -Number -Symbol (make-Listof x)))))]
   [(Un (-val 'foo) (-val 6)) (Un -Number -Symbol) (Un)]
   [(-> (Un -Symbol -Number) -Number) (-> -Number -Number) (Un)]
   [(Un (-poly (a) (make-Listof a)) (-> -Number -Number)) (-> -Number -Number) (-poly (a) (make-Listof a))]
   [(Un -Symbol -Number) (-poly (a) -Number) -Symbol]
   [(-pair -Number (-v a)) (-pair Univ Univ) (Un)]
   ))

(define-go
  restrict-tests
  remove-tests
  overlap-tests)

(define x1
  (-mu list-rec
       (Un
        (-val '())
        (-pair (-mu x (Un -Boolean -Number -String -Symbol (-val '()) (-pair x x)))
               list-rec))))
(define x2
  (Un (-val '())
      (-pair (-mu x (Un -Boolean -Number -String -Symbol (-val '()) (-pair x x)))
             (-mu x (Un -Boolean -Number -String -Symbol (-val '()) (-pair x x))))))
(provide remove-tests restrict-tests overlap-tests)

