#lang racket/base
(provide (all-defined-out))

(define-struct collection (label contents) #:transparent)
(define-struct individual (label form attrs) #:transparent)

(define-syntax define-tests
  (syntax-rules ()
    [(define-tests var label clause ...)
     (define var (Test [#:suite label clause ...]))]))

(define-syntax Test
  (syntax-rules ()
    [(Test [#:suite label clause ...])
     (make-collection label (list (Test clause) ...))]
    [(Test expr)
     expr]))

(define-syntax test
  (syntax-rules ()
    [(test label form clause ...)
     (make-individual label 'form
                      (cons (cons '#:ok-deriv? #t)
                            (append (IClause form clause) ...)))]))

(define-syntax testE
  (syntax-rules ()
    [(testE form clause ...)
     (make-individual (format "~s" 'form) 'form
                      (cons (cons '#:ok-deriv? #f)
                            (append (IClause form clause) ...)))]))

(define-syntax testK
  (syntax-rules ()
    [(testK label form clause ...)
     (test label form #:kernel clause ...)]))

(define-syntax testKE
  (syntax-rules ()
    [(testEK label form clause ...)
     (testE label form #:kernel clause ...)]))

(define-syntax IClause
  (syntax-rules ()
    [(IClause _form [#:steps spec ...])
     (list (cons '#:steps '(spec ...)))]
    [(IClause _form #:no-steps)
     (list (cons '#:steps '())
           (cons '#:hidden-steps '()))]
    [(IClause _form #:error-step)
     (list (cons '#:steps '(error)))]
    [(IClause form [#:rename+error-step rename-type])
     (list (cons '#:steps '((rename-type form) error)))]
    [(IClause _form [#:hidden-steps spec ...])
     (list (cons '#:hidden-steps '(spec ...)))]
    [(IClause form #:same-hidden-steps)
     (list (cons '#:same-hidden-steps #t))]
    [(IClause form #:no-hidden-steps)
     (list (cons '#:hidden-steps '()))]
    [(Iclause form #:kernel)
     (list (cons '#:kernel #t))]))
