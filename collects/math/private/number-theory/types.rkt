#lang typed/racket

(provide N N+ Z Q Ns Zs Base-Exponent Factorization Prime
         cast
         natural? naturals? Integer? integers? exact-zero?)

;;;
;;; Types and Predicates
;;;

(define-type N  Natural)
(define-type N+ Exact-Positive-Integer)
(define-type Z  Integer)
(define-type Q Exact-Rational)

(define-type Ns (Listof N))
(define-type Zs (Listof Z))

(define-type Base-Exponent (List N N))
(define-type BE Base-Exponent)
(define-type Factorization (List Base-Exponent))

(define-type Prime N) ; non-checked (for documentation purposes)

(define-syntax (cast stx) (syntax-case stx () [(_ . more) #'(assert . more)]))
; Note: (cast val predicate) is used in the code, where
; the math says predicate must be true, but the type system
; can prove it. Replace assert with a "proper" cast when
; it appears in Typed Racket.

(define-predicate natural?  N)  ; Note: 0 is natural
(define-predicate naturals? Ns)
(define-predicate Integer?  Z)
(define-predicate integers? Zs)
(define-predicate exact-zero? Zero)
