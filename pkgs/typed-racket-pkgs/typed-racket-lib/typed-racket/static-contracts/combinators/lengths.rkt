#lang racket/base

;; Static contracts for list and vector lengths.
;; These are used during optimizations as simplifications.
;; Ex: (list/sc any/sc) => (list-length/sc 1)

(require
  "../structures.rkt"
  "../terminal.rkt"
  "simple.rkt"
  racket/match
  racket/contract
  (for-template racket/base))

(provide
  (contract-out
    [rename list-length/sc* list-length/sc (natural-number/c . -> . static-contract?)]
    [vector-length/sc (natural-number/c . -> . static-contract?)]
    [empty-list/sc static-contract?]
    [empty-vector/sc static-contract?]))

(define-terminal-sc list-length/sc (n) #:flat
   #`(λ (l) (and (list? l) (= #,n (length l)))))

(define-terminal-sc vector-length/sc (n) #:flat
   #`(λ (l) (and (vector? l) (= #,n (vector-length l)))))

(define (list-length/sc* n)
  (if (zero? n)
      empty-list/sc
      empty-vector/sc))


(define empty-list/sc (flat/sc #'null?))
(define empty-vector/sc (vector-length/sc 0))
