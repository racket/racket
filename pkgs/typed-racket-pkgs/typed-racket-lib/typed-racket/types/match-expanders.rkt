#lang racket/base


(require "../utils/utils.rkt")

(require (rep type-rep)
         racket/match
         (types resolve)
         (contract-req)
         racket/set
         (for-syntax racket/base syntax/parse))

(provide Listof: List: MListof:)
(provide/cond-contract
  [untuple (Type/c . -> . (or/c #f (listof Type/c)))])


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
  (let loop ((t t) (seen (set)))
    (and (not (set-member? seen (Type-seq t)))
         (match (resolve t)
           [(Value: '()) null]
           [(Pair: a b) (cond [(loop b (set-add seen (Type-seq t))) => (lambda (l) (cons a l))]
                              [else #f])]
           [_ #f]))))

(define-match-expander MListof:
  (lambda (stx)
    (syntax-parse stx
      [(_ elem-pat)
       #'(Mu: var (Union: (list (Value: '()) (MPair: elem-pat (F: var)))))])))
