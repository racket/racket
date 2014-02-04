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
       ;; Note: in practice it's unlikely that the second pattern will ever come up
       ;;       because the sequence number for '() will be low and the union will
       ;;       be sorted by sequence number. As a paranoid precaution, however,
       ;;       we will match against both patterns here.
       (syntax/loc stx (or (Mu: var-pat (Union: (list (Value: '()) (Pair: elem-pat (F: var-pat)))))
                           (Mu: var-pat (Union: (list (Pair: elem-pat (F: var-pat)) (Value: '()))))))])))

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
       ;; see note above
       #'(or (Mu: var (Union: (list (Value: '()) (MPair: elem-pat (F: var)))))
             (Mu: var (Union: (list (MPair: elem-pat (F: var)) (Value: '())))))])))
