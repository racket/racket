#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep rep-utils)
         (utils tc-utils)
         (contract-req)
         (types utils subtype abbrev printer comparison)
         racket/match)



(provide/cond-contract
 [Un (() #:rest (c:listof Type/c) . c:->* . Type/c)])

(define (make-union* set)
  (match set
    [(list t) t]
    [_ (make-Union set)]))

(define empty-union (make-Union null))

(define (remove-subtypes ts)
  (let loop ([ts* ts] [result '()])
    (cond [(null? ts*) (reverse result)]
          [(ormap (lambda (t) (subtype (car ts*) t)) result) (loop (cdr ts*) result)]
          [else (loop (cdr ts*) (cons (car ts*) result))])))

;; Union constructor
;; Normalizes representation by sorting types.
(define Un
  (case-lambda
    [() empty-union]
    [(t) t]
    [args
     ;; a is a Type (not a union type)
     ;; b is a List[Type]
     (define (union2 a b)
       (define b* (make-union* b))
       (cond
         [(subtype a b*) (list b*)]
         [(subtype b* a) (list a)]
         [else (cons a b)]))
     (let ([types (remove-dups (sort (apply append (map flat args)) type<?))])
       (cond
         [(null? types) (make-union* null)]
         [(null? (cdr types)) (car types)]
         ;; FIXME: this sort is unneccessary
         [else (make-union* (sort (foldr union2 '() (remove-subtypes types)) type<?))]))]))

(define (u-maker args) (apply Un args))

(set-union-maker! u-maker)

