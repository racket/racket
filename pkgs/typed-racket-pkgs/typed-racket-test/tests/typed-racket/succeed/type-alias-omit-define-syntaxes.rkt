#lang typed/racket
(define-type (Type1 t) t #:omit-define-syntaxes)
(define-type (Type1* t) t)
(define-type Type2 (All (t) t) #:omit-define-syntaxes)
(define-type Type2* (All (t) t))
(define-type Type3 Symbol #:omit-define-syntaxes)
(define-type Type3* Symbol)
