#lang racket/base

;; Functions for the different kinds of contracts, which are represented by the symbols:
;; 'flat, 'chaperone, and 'impersonator
;;
;; There is an ordering with 'flat < 'chaperone < 'impersonator.

(require racket/match racket/contract)

(provide
  (contract-out
    [contract-kind? predicate/c]
    [contract-kind<= (contract-kind? contract-kind? . -> . boolean?)]
    [kind->keyword (contract-kind? . -> . keyword?)]
    [combine-kinds ((contract-kind?) #:rest (listof contract-kind?) . ->* . contract-kind?)]))

(define (contract-kind? v)
  (case v
    ((flat chaperone impersonator) #t)
    (else #f)))


(define (contract-kind<= v1 v2)
  (match* (v1 v2)
    [('flat _) #t]
    [('chaperone 'flat) #f]
    [('chaperone (or 'chaperone 'impersonator)) #t]
    [('impersonator (or 'flat 'chaperone)) #f]
    [('impersonator 'impersonator) #t]))

;; Computes the maximum over the supplied kinds.
(define combine-kinds
  (case-lambda
    ((v) v)
    ((v1 v2 . vs)
     (define combined (if (contract-kind<= v1 v2) v2 v1))
     (apply combine-kinds combined vs))))

(define (kind->keyword kind)
  (case kind
    ((flat) '#:flat)
    ((chaperone) '#:chaperone)
    ((impersonator) '#:impersonator)))
