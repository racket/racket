#lang racket/base
;; This file is for the abbreviations need to implement union.rkt

(require "../utils/utils.rkt")

(require (rep type-rep)
         racket/match
         (types resolve)
         (for-template racket/base)
         (for-syntax racket/base syntax/parse racket/list))

(provide (all-defined-out))

;Top and error types
(define Univ (make-Univ))
(define Err (make-Error))

;A Type that corresponds to the any contract for the
;return type of functions
;FIXME
;This is not correct as Univ is only a single value.
(define ManyUniv Univ)




;; Char type (needed because of how sequences are checked in subtype)
(define -Char (make-Base 'Char #'char? char? #'-Char #f))


;;List expanders
(define-match-expander Listof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat (~optional var-pat #:defaults ([var-pat #'var])))
       (syntax/loc stx (Mu: var-pat (Union: (list (Value: '()) (Pair: elem-pat (F: var-pat))))))])))

(define-match-expander List:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pats)
       #'(app untuple (? values elem-pats))])))

(define (untuple t)
  (match (resolve t)
    [(Value: '()) null]
    [(Pair: a b) (cond [(untuple b) => (lambda (l) (cons a l))]
                       [else #f])]
    [_ #f]))

(define-match-expander MListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       #'(Mu: var (Union: (list (Value: '()) (MPair: elem-pat (F: var)))))])))

