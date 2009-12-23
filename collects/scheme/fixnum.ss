#lang at-exp scheme/base

(require scheme/contract
         scribble/srcdoc
         (only-in rnrs/arithmetic/fixnums-6 fixnum-width)
         (prefix-in r6: rnrs/arithmetic/fixnums-6))

(require/doc scheme/base
             scribble/manual
             (for-label scheme/unsafe/ops))

(define fx->fl exact->inexact)
(define fxabs abs)
(define (fx- a b) (- a b))
(define (fx+ a b) (+ a b))
(define (fx* a b) (* a b))
(define (fxnot a) (r6:fxnot a))

(define (fx>= a b) (>= a b))
(define (fx> a b) (> a b))
(define (fx= a b) (= a b))
(define (fx< a b) (< a b))
(define (fx<= a b) (<= a b))

(define fxquotient quotient)
(define fxremainder remainder)
(define (fxrshift x y) (arithmetic-shift x (- y)))
(define (fxlshift x y) (arithmetic-shift x y))

(define (fxand x y) (r6:fxand x y))
(define (fxior x y) (r6:fxior x y))
(define (fxxor x y) (r6:fxxor x y))

(provide/doc
 [proc-doc/names 
  fx>= (-> fixnum? fixnum? boolean?) (x y)
  @{A safe version of @scheme[unsafe-fx>=].}]
 
 [proc-doc/names fx> (-> fixnum? fixnum? boolean?) (x y)
                 @{A safe version of @scheme[unsafe-fx>].}]
 [proc-doc/names fx= (-> fixnum? fixnum? boolean?) (x y)
                 @{A safe version of @scheme[unsafe-fx=].}]
 [proc-doc/names fx< (-> fixnum? fixnum? boolean?) (x y)
                 @{A safe version of @scheme[unsafe-fx<].}]
 [proc-doc/names fx<= (-> fixnum? fixnum? boolean?) (x y)
                 @{A safe version of @scheme[unsafe-fx<=].}]
 [proc-doc/names fxnot (-> fixnum? fixnum?) (x)
                 @{A safe version of @scheme[unsafe-fxnot].}]
 [proc-doc/names fxand (-> fixnum? fixnum? fixnum?) (x y)
                 @{A safe version of @scheme[unsafe-fxand].}]
 [proc-doc/names fxior (-> fixnum? fixnum? fixnum?) (x y)
                 @{A safe version of @scheme[unsafe-fxior].}]
 [proc-doc/names fxxor (-> fixnum? fixnum? fixnum?) (x y)
                 @{A safe version of @scheme[unsafe-fxxor].}]
 
 [proc-doc fx+ 
           (->d ([x fixnum?] [y fixnum?])
                ()
                #:pre-cond (fixnum? (+ x y))
                [result fixnum?])
           @{A safe version of @scheme[unsafe-fx+].}]
 
 [proc-doc fx* (->d ([x fixnum?] [y fixnum?])
                    ()
                    #:pre-cond (fixnum? (* x y))
                    [result fixnum?])
           @{A safe version of @scheme[unsafe-fx*].}]
 [proc-doc fx- (->d ([x fixnum?] [y fixnum?])
                    ()
                    #:pre-cond (fixnum? (- x y))
                    [result fixnum?])
           @{A safe version of @scheme[unsafe-fx-].}]
 
 [proc-doc/names fx->fl (-> fixnum? inexact-real?) (x)
                 @{A safe version of @scheme[unsafe-fx->fl].}]
 [proc-doc/names fxabs (-> fixnum? fixnum?) (x)
                 @{A safe version of @scheme[unsafe-fxabs]}]
 [proc-doc/names fxquotient (-> fixnum? fixnum? fixnum?) (x y) 
                 @{A safe version of @scheme[unsafe-fxquotient].}]
 [proc-doc/names fxremainder (-> fixnum? fixnum? fixnum?) (x y)
                 @{A safe version of @scheme[unsafe-fxremainder].}]
 [proc-doc/names fxlshift 
                 (-> fixnum? 
                     (and/c fixnum? (between/c 0 (fixnum-width)))
                     fixnum?)
                 (x y)
                 @{A safe version of @scheme[unsafe-fxlshift].}]
 [proc-doc/names fxrshift
                 (-> fixnum? 
                     (and/c fixnum? (between/c 0 (fixnum-width)))
                     fixnum?)
                 (x y)
                 @{A safe version of @scheme[unsafe-fxrshift].}])
