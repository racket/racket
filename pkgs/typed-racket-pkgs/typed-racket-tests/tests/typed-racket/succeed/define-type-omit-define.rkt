#lang typed/racket

(define-syntax (x stx) #'5)

(define-type x Number #:omit-define-syntaxes)

(ann 7 x)

(add1 x)
