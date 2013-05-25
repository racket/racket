#lang typed/racket

(provide cast natural? exact-zero?)

(define-syntax (cast stx) (syntax-case stx () [(_ . more) #'(assert . more)]))
; Note: (cast val predicate) is used in the code, where
; the math says predicate must be true, but the type system
; can prove it. Replace assert with a "proper" cast when
; it appears in Typed Racket.

(define natural? exact-nonnegative-integer?)
(define-predicate exact-zero? Zero)
